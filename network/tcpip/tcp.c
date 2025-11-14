/* AstraLisp OS TCP Implementation */

#include "tcp.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/driver/network.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define TCP_MAX_CONNECTIONS 1024
#define TCP_DEFAULT_MSS 1460
#define TCP_DEFAULT_WINDOW 65535
#define TCP_INITIAL_CWND 1
#define TCP_INITIAL_SSTHRESH 65535
#define TCP_RTO_MIN 1000
#define TCP_RTO_MAX 60000

static struct tcp_connection* connection_list = NULL;
static uint32_t next_local_port = 32768;

/* Calculate TCP checksum */
uint16_t tcp_calculate_checksum(const struct tcp_header* header, const uint8_t* data,
                                size_t data_len, uint32_t src_addr, uint32_t dst_addr) {
    uint32_t sum = 0;
    
    /* Pseudo header */
    sum += (src_addr >> 16) + (src_addr & 0xFFFF);
    sum += (dst_addr >> 16) + (dst_addr & 0xFFFF);
    sum += 6;  /* TCP protocol */
    sum += (uint16_t)(sizeof(struct tcp_header) + data_len);
    
    /* TCP header */
    const uint16_t* hdr_words = (const uint16_t*)header;
    for (size_t i = 0; i < sizeof(struct tcp_header) / 2; i++) {
        if (i != 8) {  /* Skip checksum field */
            sum += hdr_words[i];
        }
    }
    
    /* Data */
    const uint16_t* data_words = (const uint16_t*)data;
    for (size_t i = 0; i < data_len / 2; i++) {
        sum += data_words[i];
    }
    
    if (data_len % 2) {
        sum += ((uint16_t)data[data_len - 1]) << 8;
    }
    
    /* Fold to 16 bits */
    while (sum >> 16) {
        sum = (sum & 0xFFFF) + (sum >> 16);
    }
    
    return ~sum;
}

/* Send TCP segment */
static int tcp_send_segment(struct tcp_connection* conn, uint8_t flags,
                            const uint8_t* data, size_t data_len) {
    if (!conn) {
        return -1;
    }
    
    /* Allocate segment */
    struct tcp_segment* segment = (struct tcp_segment*)kmalloc(sizeof(struct tcp_segment));
    if (!segment) {
        return -1;
    }
    
    memset(&segment->header, 0, sizeof(struct tcp_header));
    segment->header.src_port = conn->local_port;
    segment->header.dst_port = conn->remote_port;
    segment->header.seq_num = conn->send_seq;
    segment->header.ack_num = conn->recv_next;
    segment->header.data_offset = 5;  /* 20 bytes header */
    segment->header.flags = flags;
    segment->header.window = conn->recv_window;
    segment->header.urgent_ptr = 0;
    
    if (data && data_len > 0) {
        segment->data = (uint8_t*)kmalloc(data_len);
        if (!segment->data) {
            kfree(segment);
            return -1;
        }
        memcpy(segment->data, data, data_len);
        segment->data_len = data_len;
    } else {
        segment->data = NULL;
        segment->data_len = 0;
    }
    
    /* Calculate checksum */
    segment->header.checksum = tcp_calculate_checksum(&segment->header, segment->data,
                                                       segment->data_len,
                                                       conn->local_addr, conn->remote_addr);
    
    /* Add to send queue */
    segment->next = conn->send_queue;
    conn->send_queue = segment;
    
    /* Update sequence number */
    if (flags & TCP_FLAG_SYN) {
        conn->send_seq++;
    }
    if (flags & TCP_FLAG_FIN) {
        conn->send_seq++;
    }
    if (data_len > 0) {
        conn->send_seq += data_len;
    }
    
    conn->send_next = conn->send_seq;
    
    /* Send via network interface */
    /* This would call the network driver */
    
    conn->segments_sent++;
    if (data_len > 0) {
        conn->bytes_sent += data_len;
    }
    
    return 0;
}

/* Handle state transitions */
int tcp_handle_state_transition(struct tcp_connection* conn, const struct tcp_header* header) {
    if (!conn || !header) {
        return -1;
    }
    
    uint8_t flags = header->flags;
    
    switch (conn->state) {
        case TCP_CLOSED:
            /* Connection closed, ignore */
            return -1;
            
        case TCP_LISTEN:
            if (flags & TCP_FLAG_SYN) {
                /* SYN received - send SYN-ACK */
                conn->state = TCP_SYN_RECEIVED;
                conn->recv_seq = header->seq_num;
                conn->recv_next = conn->recv_seq + 1;
                tcp_send_segment(conn, TCP_FLAG_SYN | TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_SYN_SENT:
            if (flags & TCP_FLAG_SYN && flags & TCP_FLAG_ACK) {
                /* SYN-ACK received */
                conn->state = TCP_ESTABLISHED;
                conn->recv_seq = header->seq_num;
                conn->recv_next = conn->recv_seq + 1;
                conn->send_una = header->ack_num;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            } else if (flags & TCP_FLAG_SYN) {
                /* Simultaneous open */
                conn->state = TCP_SYN_RECEIVED;
                conn->recv_seq = header->seq_num;
                conn->recv_next = conn->recv_seq + 1;
                tcp_send_segment(conn, TCP_FLAG_SYN | TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_SYN_RECEIVED:
            if (flags & TCP_FLAG_ACK) {
                /* ACK received - connection established */
                conn->state = TCP_ESTABLISHED;
                conn->send_una = header->ack_num;
                return 0;
            }
            break;
            
        case TCP_ESTABLISHED:
            if (flags & TCP_FLAG_FIN) {
                /* FIN received */
                conn->state = TCP_CLOSE_WAIT;
                conn->recv_next++;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_FIN_WAIT_1:
            if (flags & TCP_FLAG_ACK) {
                /* ACK for our FIN */
                conn->state = TCP_FIN_WAIT_2;
                return 0;
            } else if (flags & TCP_FLAG_FIN) {
                /* Simultaneous close */
                conn->state = TCP_CLOSING;
                conn->recv_next++;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_FIN_WAIT_2:
            if (flags & TCP_FLAG_FIN) {
                /* FIN received */
                conn->state = TCP_TIME_WAIT;
                conn->recv_next++;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_CLOSE_WAIT:
            /* Application should close */
            break;
            
        case TCP_CLOSING:
            if (flags & TCP_FLAG_ACK) {
                /* ACK for our FIN */
                conn->state = TCP_TIME_WAIT;
                return 0;
            }
            break;
            
        case TCP_LAST_ACK:
            if (flags & TCP_FLAG_ACK) {
                /* ACK for our FIN - connection closed */
                conn->state = TCP_CLOSED;
                return 0;
            }
            break;
            
        case TCP_TIME_WAIT:
            /* Wait for 2MSL */
            break;
            
        default:
            break;
    }
    
    return 0;
}

/* Update congestion window (TCP Reno) */
void tcp_update_cwnd(struct tcp_connection* conn, bool ack_received, bool timeout) {
    if (!conn) {
        return;
    }
    
    if (timeout) {
        /* Timeout - enter slow start */
        conn->ssthresh = conn->cwnd / 2;
        if (conn->ssthresh < 1) {
            conn->ssthresh = 1;
        }
        conn->cwnd = TCP_INITIAL_CWND;
        conn->in_slow_start = true;
        conn->in_congestion_avoidance = false;
        conn->retransmissions++;
        return;
    }
    
    if (ack_received) {
        if (conn->in_slow_start) {
            /* Slow start: exponential growth */
            conn->cwnd++;
            if (conn->cwnd >= conn->ssthresh) {
                conn->in_slow_start = false;
                conn->in_congestion_avoidance = true;
            }
        } else if (conn->in_congestion_avoidance) {
            /* Congestion avoidance: linear growth (additive increase) */
            /* cwnd += 1/cwnd per ACK, so we increment by 1 every cwnd ACKs */
            static uint32_t ack_counter = 0;
            ack_counter++;
            if (ack_counter >= conn->cwnd) {
                conn->cwnd++;
                ack_counter = 0;
            }
        }
    } else {
        /* Duplicate ACK - fast retransmit */
        conn->dup_ack_count++;
        if (conn->dup_ack_count == 3) {
            /* Fast retransmit */
            conn->ssthresh = conn->cwnd / 2;
            if (conn->ssthresh < 1) {
                conn->ssthresh = 1;
            }
            conn->cwnd = conn->ssthresh + 3;
            conn->in_slow_start = false;
            conn->in_congestion_avoidance = true;
            tcp_retransmit(conn);
            conn->retransmissions++;
        }
    }
}

/* Retransmit segment */
int tcp_retransmit(struct tcp_connection* conn) {
    if (!conn) {
        return -1;
    }
    
    /* Retransmit oldest unacked segment */
    struct tcp_segment* segment = conn->send_unacked;
    if (!segment) {
        return -1;
    }
    
    /* Resend segment */
    /* This would call tcp_send_segment with the same data */
    
    /* Update retransmit timeout (exponential backoff) */
    conn->retransmit_timeout *= 2;
    if (conn->retransmit_timeout > TCP_RTO_MAX) {
        conn->retransmit_timeout = TCP_RTO_MAX;
    }
    
    return 0;
}

/* Process incoming TCP segment */
int tcp_process_segment(struct tcp_connection* conn, const struct tcp_header* header,
                        const uint8_t* data, size_t data_len) {
    if (!conn || !header) {
        return -1;
    }
    
    /* Verify checksum */
    uint16_t calculated_checksum = tcp_calculate_checksum(header, data, data_len,
                                                          conn->remote_addr, conn->local_addr);
    if (calculated_checksum != header->checksum) {
        return -1;  /* Invalid checksum */
    }
    
    /* Update receive window */
    conn->recv_window = header->window;
    
    /* Handle ACK */
    if (header->flags & TCP_FLAG_ACK) {
        if (header->ack_num > conn->send_una) {
            /* New ACK */
            uint32_t acked_bytes = header->ack_num - conn->send_una;
            conn->send_una = header->ack_num;
            conn->send_buffer_used -= acked_bytes;
            
            /* Remove acked segments from unacked queue */
            struct tcp_segment* seg = conn->send_unacked;
            struct tcp_segment* prev = NULL;
            while (seg) {
                if (seg->header.seq_num + seg->data_len <= conn->send_una) {
                    /* Segment fully acked */
                    struct tcp_segment* next = seg->next;
                    if (prev) {
                        prev->next = next;
                    } else {
                        conn->send_unacked = next;
                    }
                    if (seg->data) {
                        kfree(seg->data);
                    }
                    kfree(seg);
                    seg = next;
                } else {
                    prev = seg;
                    seg = seg->next;
                }
            }
            
            /* Update congestion window */
            tcp_update_cwnd(conn, true, false);
            conn->dup_ack_count = 0;
        } else if (header->ack_num == conn->send_una) {
            /* Duplicate ACK */
            conn->dup_ack_count++;
            tcp_update_cwnd(conn, false, false);
        }
    }
    
    /* Handle data */
    if (data_len > 0) {
        if (header->seq_num == conn->recv_next) {
            /* In-order data */
            struct tcp_segment* segment = (struct tcp_segment*)kmalloc(sizeof(struct tcp_segment));
            if (segment) {
                memcpy(&segment->header, header, sizeof(struct tcp_header));
                segment->data = (uint8_t*)kmalloc(data_len);
                if (segment->data) {
                    memcpy(segment->data, data, data_len);
                    segment->data_len = data_len;
                    segment->next = conn->recv_queue;
                    conn->recv_queue = segment;
                    conn->recv_next += data_len;
                    conn->recv_buffer_used += data_len;
                    conn->bytes_received += data_len;
                } else {
                    kfree(segment);
                }
            }
            
            /* Send ACK */
            tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
        } else {
            /* Out-of-order data - buffer for now */
            /* Full implementation would handle out-of-order segments */
        }
    }
    
    /* Handle state transitions */
    tcp_handle_state_transition(conn, header);
    
    conn->segments_received++;
    
    return 0;
}

/* Send data */
int tcp_send(struct tcp_connection* conn, const void* data, size_t len) {
    if (!conn || !data || len == 0) {
        return -1;
    }
    
    if (conn->state != TCP_ESTABLISHED) {
        return -1;
    }
    
    /* Check send window */
    uint32_t available = conn->send_window - conn->send_buffer_used;
    if (len > available) {
        len = available;
    }
    
    /* Check congestion window */
    if (len > conn->cwnd * TCP_DEFAULT_MSS) {
        len = conn->cwnd * TCP_DEFAULT_MSS;
    }
    
    /* Send data in segments */
    const uint8_t* data_ptr = (const uint8_t*)data;
    size_t sent = 0;
    
    while (sent < len) {
        size_t segment_len = len - sent;
        if (segment_len > TCP_DEFAULT_MSS) {
            segment_len = TCP_DEFAULT_MSS;
        }
        
        tcp_send_segment(conn, TCP_FLAG_ACK | TCP_FLAG_PSH, data_ptr + sent, segment_len);
        sent += segment_len;
    }
    
    return (int)sent;
}

/* Receive data */
int tcp_receive(struct tcp_connection* conn, void* buffer, size_t len) {
    if (!conn || !buffer || len == 0) {
        return -1;
    }
    
    if (conn->state != TCP_ESTABLISHED && conn->state != TCP_CLOSE_WAIT) {
        return -1;
    }
    
    /* Get data from receive queue */
    struct tcp_segment* segment = conn->recv_queue;
    if (!segment) {
        return 0;  /* No data available */
    }
    
    size_t copied = 0;
    uint8_t* buf = (uint8_t*)buffer;
    
    while (segment && copied < len) {
        size_t to_copy = segment->data_len;
        if (to_copy > len - copied) {
            to_copy = len - copied;
        }
        
        memcpy(buf + copied, segment->data, to_copy);
        copied += to_copy;
        
        if (to_copy == segment->data_len) {
            /* Segment fully consumed */
            struct tcp_segment* next = segment->next;
            if (segment->data) {
                kfree(segment->data);
            }
            kfree(segment);
            conn->recv_queue = next;
            conn->recv_buffer_used -= segment->data_len;
        } else {
            /* Partial consumption */
            memmove(segment->data, segment->data + to_copy, segment->data_len - to_copy);
            segment->data_len -= to_copy;
            conn->recv_buffer_used -= to_copy;
        }
        
        segment = conn->recv_queue;
    }
    
    return (int)copied;
}

/* Create TCP connection */
struct tcp_connection* tcp_create_connection(uint32_t local_addr, uint16_t local_port,
                                            uint32_t remote_addr, uint16_t remote_port) {
    struct tcp_connection* conn = (struct tcp_connection*)kmalloc(sizeof(struct tcp_connection));
    if (!conn) {
        return NULL;
    }
    
    memset(conn, 0, sizeof(struct tcp_connection));
    conn->local_addr = local_addr;
    conn->remote_addr = remote_addr;
    conn->local_port = local_port;
    conn->remote_port = remote_port;
    conn->state = TCP_CLOSED;
    
    /* Initialize sequence numbers */
    conn->send_seq = 0;  /* Would use random ISN */
    conn->send_una = conn->send_seq;
    conn->send_next = conn->send_seq;
    conn->recv_seq = 0;
    conn->recv_next = conn->recv_seq;
    
    /* Initialize windows */
    conn->recv_window = TCP_DEFAULT_WINDOW;
    conn->send_window = TCP_DEFAULT_WINDOW;
    
    /* Initialize congestion control */
    conn->cwnd = TCP_INITIAL_CWND;
    conn->ssthresh = TCP_INITIAL_SSTHRESH;
    conn->rtt = 100;  /* Initial RTT estimate */
    conn->rtt_var = 50;
    conn->retransmit_timeout = TCP_RTO_MIN;
    conn->in_slow_start = true;
    conn->in_congestion_avoidance = false;
    
    /* Initialize buffers */
    conn->send_buffer_size = 65535;
    conn->recv_buffer_size = 65535;
    
    /* Add to connection list */
    conn->next = connection_list;
    connection_list = conn;
    
    return conn;
}

/* Destroy TCP connection */
void tcp_destroy_connection(struct tcp_connection* conn) {
    if (!conn) {
        return;
    }
    
    /* Free send queue */
    struct tcp_segment* seg = conn->send_queue;
    while (seg) {
        struct tcp_segment* next = seg->next;
        if (seg->data) {
            kfree(seg->data);
        }
        kfree(seg);
        seg = next;
    }
    
    /* Free receive queue */
    seg = conn->recv_queue;
    while (seg) {
        struct tcp_segment* next = seg->next;
        if (seg->data) {
            kfree(seg->data);
        }
        kfree(seg);
        seg = next;
    }
    
    /* Remove from connection list */
    if (conn == connection_list) {
        connection_list = conn->next;
    } else {
        struct tcp_connection* prev = connection_list;
        while (prev && prev->next != conn) {
            prev = prev->next;
        }
        if (prev) {
            prev->next = conn->next;
        }
    }
    
    kfree(conn);
}

/* Find connection */
struct tcp_connection* tcp_find_connection(uint32_t local_addr, uint16_t local_port,
                                          uint32_t remote_addr, uint16_t remote_port) {
    struct tcp_connection* conn = connection_list;
    
    while (conn) {
        if (conn->local_addr == local_addr &&
            conn->local_port == local_port &&
            conn->remote_addr == remote_addr &&
            conn->remote_port == remote_port) {
            return conn;
        }
        conn = conn->next;
    }
    
    return NULL;
}

/* Initialize TCP */
int tcp_init(void) {
    connection_list = NULL;
    next_local_port = 32768;
    return 0;
}
