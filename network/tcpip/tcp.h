/* AstraLisp OS TCP Implementation */

#ifndef TCP_H
#define TCP_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* TCP states */
typedef enum {
    TCP_CLOSED = 0,
    TCP_LISTEN = 1,
    TCP_SYN_SENT = 2,
    TCP_SYN_RECEIVED = 3,
    TCP_ESTABLISHED = 4,
    TCP_FIN_WAIT_1 = 5,
    TCP_FIN_WAIT_2 = 6,
    TCP_CLOSE_WAIT = 7,
    TCP_CLOSING = 8,
    TCP_LAST_ACK = 9,
    TCP_TIME_WAIT = 10
} tcp_state_t;

/* TCP flags */
#define TCP_FLAG_FIN 0x01
#define TCP_FLAG_SYN 0x02
#define TCP_FLAG_RST 0x04
#define TCP_FLAG_PSH 0x08
#define TCP_FLAG_ACK 0x10
#define TCP_FLAG_URG 0x20

/* TCP options */
#define TCP_OPT_END         0
#define TCP_OPT_NOP         1
#define TCP_OPT_MSS         2
#define TCP_OPT_WINDOW      3
#define TCP_OPT_SACK_PERM   4
#define TCP_OPT_SACK        5
#define TCP_OPT_TIMESTAMP   8

/* SACK block */
struct tcp_sack_block {
    uint32_t left_edge;   /* Left edge of block */
    uint32_t right_edge;  /* Right edge of block */
};

/* Maximum SACK blocks per segment (RFC 2018) */
#define TCP_MAX_SACK_BLOCKS 4

/* Red-Black tree colors */
typedef enum {
    RB_RED = 0,
    RB_BLACK = 1
} rb_color_t;

/* Out-of-order segment node (Red-Black tree) */
struct ooo_segment {
    uint32_t seq_start;           /* Start sequence number */
    uint32_t seq_end;             /* End sequence number (exclusive) */
    uint8_t* data;                /* Segment data */
    size_t data_len;              /* Data length */
    uint64_t timestamp;           /* Arrival timestamp */

    /* Red-Black tree linkage */
    struct ooo_segment* parent;
    struct ooo_segment* left;
    struct ooo_segment* right;
    rb_color_t color;
};

/* Reassembly queue (Red-Black tree root) */
struct reassembly_queue {
    struct ooo_segment* root;     /* Tree root */
    struct ooo_segment* nil;      /* Sentinel node */
    uint32_t segment_count;       /* Number of segments */
    size_t total_bytes;           /* Total buffered bytes */
    size_t max_bytes;             /* Maximum buffer size */
};

/* TCP header */
struct tcp_header {
    uint16_t src_port;
    uint16_t dst_port;
    uint32_t seq_num;
    uint32_t ack_num;
    uint8_t data_offset:4;
    uint8_t reserved:3;
    uint8_t flags:9;
    uint16_t window;
    uint16_t checksum;
    uint16_t urgent_ptr;
    uint8_t options[40];
};

/* TCP segment */
struct tcp_segment {
    struct tcp_header header;
    uint8_t* data;
    size_t data_len;
    uint64_t timestamp;
    struct tcp_segment* next;
};

/* TCP connection */
struct tcp_connection {
    uint32_t local_addr;
    uint32_t remote_addr;
    uint16_t local_port;
    uint16_t remote_port;
    tcp_state_t state;
    
    /* Sequence numbers */
    uint32_t send_seq;
    uint32_t send_una;
    uint32_t send_next;
    uint32_t recv_seq;
    uint32_t recv_next;
    
    /* Receive window */
    uint16_t recv_window;
    uint16_t recv_window_scale;
    
    /* Send window */
    uint16_t send_window;
    
    /* Congestion control */
    uint32_t cwnd;              /* Congestion window */
    uint32_t ssthresh;          /* Slow start threshold */
    uint32_t dup_ack_count;     /* Duplicate ACK counter */
    uint32_t rtt;               /* Round trip time */
    uint32_t rtt_var;           /* RTT variance */
    uint32_t retransmit_timeout;
    bool in_slow_start;
    bool in_congestion_avoidance;
    
    /* Send buffer */
    struct tcp_segment* send_queue;
    struct tcp_segment* send_unacked;
    size_t send_buffer_size;
    size_t send_buffer_used;
    
    /* Receive buffer */
    struct tcp_segment* recv_queue;
    size_t recv_buffer_size;
    size_t recv_buffer_used;

    /* Out-of-order reassembly queue */
    struct reassembly_queue* reasm_queue;

    /* SACK support */
    bool sack_permitted;              /* SACK enabled for this connection */
    struct tcp_sack_block sack_blocks[TCP_MAX_SACK_BLOCKS];
    uint32_t sack_block_count;        /* Number of SACK blocks */

    /* Timers */
    uint64_t retransmit_timer;
    uint64_t persist_timer;
    uint64_t keepalive_timer;
    uint64_t time_wait_timer;
    
    /* Statistics */
    uint64_t bytes_sent;
    uint64_t bytes_received;
    uint32_t segments_sent;
    uint32_t segments_received;
    uint32_t retransmissions;
    uint32_t duplicate_acks;
    
    struct tcp_connection* next;
};

/* Initialize TCP */
int tcp_init(void);

/* Create TCP connection */
struct tcp_connection* tcp_create_connection(uint32_t local_addr, uint16_t local_port,
                                            uint32_t remote_addr, uint16_t remote_port);

/* Destroy TCP connection */
void tcp_destroy_connection(struct tcp_connection* conn);

/* Process incoming TCP segment */
int tcp_process_segment(struct tcp_connection* conn, const struct tcp_header* header,
                        const uint8_t* data, size_t data_len);

/* Send data */
int tcp_send(struct tcp_connection* conn, const void* data, size_t len);

/* Receive data */
int tcp_receive(struct tcp_connection* conn, void* buffer, size_t len);

/* Handle state transitions */
int tcp_handle_state_transition(struct tcp_connection* conn, const struct tcp_header* header);

/* Calculate TCP checksum */
uint16_t tcp_calculate_checksum(const struct tcp_header* header, const uint8_t* data,
                                size_t data_len, uint32_t src_addr, uint32_t dst_addr);

/* Update congestion window */
void tcp_update_cwnd(struct tcp_connection* conn, bool ack_received, bool timeout);

/* Retransmit segment */
int tcp_retransmit(struct tcp_connection* conn);

/* Find connection */
struct tcp_connection* tcp_find_connection(uint32_t local_addr, uint16_t local_port,
                                          uint32_t remote_addr, uint16_t remote_port);

/* Reassembly queue operations */
struct reassembly_queue* reasm_queue_create(size_t max_bytes);
void reasm_queue_destroy(struct reassembly_queue* queue);
int reasm_queue_insert(struct reassembly_queue* queue, uint32_t seq_start,
                       uint32_t seq_end, const uint8_t* data, size_t data_len);
struct ooo_segment* reasm_queue_extract_ready(struct reassembly_queue* queue,
                                              uint32_t recv_next);
void reasm_queue_remove_below(struct reassembly_queue* queue, uint32_t seq);

/* SACK operations */
void tcp_generate_sack_blocks(struct tcp_connection* conn);
int tcp_add_sack_option(struct tcp_header* header, const struct tcp_sack_block* blocks,
                        uint32_t block_count);
int tcp_parse_sack_option(const uint8_t* options, size_t options_len,
                          struct tcp_sack_block* blocks, uint32_t* block_count);

#endif /* TCP_H */
