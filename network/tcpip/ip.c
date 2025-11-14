/* AstraLisp OS IP Implementation */

#include "ip.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/driver/network.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define IP_MTU 1500
#define IP_HEADER_SIZE 20
#define IP_FRAGMENT_TIMEOUT 60000  /* 60 seconds */
#define IP_MAX_FRAGMENT_SIZE (IP_MTU - IP_HEADER_SIZE)

static struct ip_route* route_table = NULL;
static struct ip_reassembly* reassembly_list = NULL;
static uint16_t next_packet_id = 1;

/* Calculate IP checksum */
uint16_t ip_calculate_checksum(const struct ip_header* header) {
    uint32_t sum = 0;
    const uint16_t* words = (const uint16_t*)header;
    
    for (size_t i = 0; i < sizeof(struct ip_header) / 2; i++) {
        sum += words[i];
    }
    
    while (sum >> 16) {
        sum = (sum & 0xFFFF) + (sum >> 16);
    }
    
    return ~sum;
}

/* Find route */
struct ip_route* ip_find_route(uint32_t dst_addr) {
    struct ip_route* best_route = NULL;
    uint8_t best_metric = 255;
    
    struct ip_route* route = route_table;
    while (route) {
        if ((dst_addr & route->netmask) == route->network) {
            if (route->metric < best_metric) {
                best_route = route;
                best_metric = route->metric;
            }
        }
        route = route->next;
    }
    
    return best_route;
}

/* Add route */
int ip_add_route(uint32_t network, uint32_t netmask, uint32_t gateway, uint8_t metric) {
    struct ip_route* route = (struct ip_route*)kmalloc(sizeof(struct ip_route));
    if (!route) {
        return -1;
    }
    
    route->network = network;
    route->netmask = netmask;
    route->gateway = gateway;
    route->metric = metric;
    route->interface_addr = 0;  /* Would set from interface */
    route->next = route_table;
    route_table = route;
    
    return 0;
}

/* Fragment IP packet */
int ip_fragment(uint32_t src_addr, uint32_t dst_addr, uint8_t protocol,
                const void* data, size_t len, struct ip_fragment** fragments, size_t* fragment_count) {
    if (!data || len == 0 || !fragments || !fragment_count) {
        return -1;
    }
    
    size_t max_data_per_fragment = IP_MAX_FRAGMENT_SIZE;
    size_t num_fragments = (len + max_data_per_fragment - 1) / max_data_per_fragment;
    
    *fragments = NULL;
    *fragment_count = 0;
    
    uint16_t packet_id = next_packet_id++;
    const uint8_t* data_ptr = (const uint8_t*)data;
    
    for (size_t i = 0; i < num_fragments; i++) {
        struct ip_fragment* fragment = (struct ip_fragment*)kmalloc(sizeof(struct ip_fragment));
        if (!fragment) {
            /* Free already allocated fragments */
            struct ip_fragment* frag = *fragments;
            while (frag) {
                struct ip_fragment* next = frag->next;
                if (frag->data) {
                    kfree(frag->data);
                }
                kfree(frag);
                frag = next;
            }
            return -1;
        }
        
        size_t fragment_data_len = len - (i * max_data_per_fragment);
        if (fragment_data_len > max_data_per_fragment) {
            fragment_data_len = max_data_per_fragment;
        }
        
        fragment->id = packet_id;
        fragment->src_addr = src_addr;
        fragment->dst_addr = dst_addr;
        fragment->offset = i * max_data_per_fragment / 8;
        fragment->last_fragment = (i == num_fragments - 1);
        fragment->data = (uint8_t*)kmalloc(fragment_data_len);
        
        if (!fragment->data) {
            kfree(fragment);
            /* Free already allocated fragments */
            struct ip_fragment* frag = *fragments;
            while (frag) {
                struct ip_fragment* next = frag->next;
                if (frag->data) {
                    kfree(frag->data);
                }
                kfree(frag);
                frag = next;
            }
            return -1;
        }
        
        memcpy(fragment->data, data_ptr + (i * max_data_per_fragment), fragment_data_len);
        fragment->data_len = fragment_data_len;
        fragment->timestamp = 0;  /* Would use real timestamp */
        fragment->next = *fragments;
        *fragments = fragment;
        (*fragment_count)++;
    }
    
    return 0;
}

/* Reassemble IP fragments */
int ip_reassemble_fragments(struct ip_reassembly* reassembly, uint8_t** reassembled_data, size_t* reassembled_len) {
    if (!reassembly || !reassembled_data || !reassembled_len) {
        return -1;
    }
    
    /* Check if all fragments received */
    if (reassembly->received_length < reassembly->total_length) {
        return -1;  /* Not complete */
    }
    
    /* Allocate reassembled buffer */
    *reassembled_data = (uint8_t*)kmalloc(reassembly->total_length);
    if (!*reassembled_data) {
        return -1;
    }
    
    /* Copy fragments in order */
    struct ip_fragment* fragment = reassembly->fragments;
    uint8_t* buffer = *reassembled_data;
    size_t offset = 0;
    
    while (fragment) {
        if (offset + fragment->data_len > reassembly->total_length) {
            kfree(*reassembled_data);
            *reassembled_data = NULL;
            return -1;
        }
        
        memcpy(buffer + offset, fragment->data, fragment->data_len);
        offset += fragment->data_len;
        fragment = fragment->next;
    }
    
    *reassembled_len = reassembly->total_length;
    
    return 0;
}

/* Process incoming IP packet */
int ip_process_packet(const uint8_t* packet, size_t len) {
    if (!packet || len < IP_HEADER_SIZE) {
        return -1;
    }
    
    const struct ip_header* header = (const struct ip_header*)packet;
    
    /* Verify version */
    if (header->version != 4) {
        return -1;
    }
    
    /* Verify checksum */
    uint16_t calculated_checksum = ip_calculate_checksum(header);
    if (calculated_checksum != header->checksum) {
        return -1;
    }
    
    /* Check if fragmented */
    if (header->flags & 0x1 || header->fragment_offset != 0) {
        /* Fragment - handle reassembly */
        struct ip_reassembly* reassembly = reassembly_list;
        while (reassembly) {
            if (reassembly->id == header->id &&
                reassembly->src_addr == header->src_addr &&
                reassembly->dst_addr == header->dst_addr) {
                /* Add fragment to reassembly */
                struct ip_fragment* fragment = (struct ip_fragment*)kmalloc(sizeof(struct ip_fragment));
                if (fragment) {
                    fragment->id = header->id;
                    fragment->src_addr = header->src_addr;
                    fragment->dst_addr = header->dst_addr;
                    fragment->offset = header->fragment_offset;
                    fragment->last_fragment = !(header->flags & 0x1);
                    fragment->data_len = header->total_length - (header->ihl * 4);
                    fragment->data = (uint8_t*)kmalloc(fragment->data_len);
                    if (fragment->data) {
                        memcpy(fragment->data, packet + (header->ihl * 4), fragment->data_len);
                        fragment->next = reassembly->fragments;
                        reassembly->fragments = fragment;
                        reassembly->received_length += fragment->data_len;
                        
                        if (fragment->last_fragment) {
                            reassembly->total_length = fragment->offset * 8 + fragment->data_len;
                        }
                        
                        /* Check if complete */
                        if (reassembly->received_length >= reassembly->total_length) {
                            uint8_t* reassembled_data;
                            size_t reassembled_len;
                            if (ip_reassemble_fragments(reassembly, &reassembled_data, &reassembled_len) == 0) {
                                /* Process reassembled packet */
                                /* This would call the protocol handler */
                                kfree(reassembled_data);
                            }
                        }
                    } else {
                        kfree(fragment);
                    }
                }
                return 0;
            }
            reassembly = reassembly->next;
        }
        
        /* Create new reassembly */
        reassembly = (struct ip_reassembly*)kmalloc(sizeof(struct ip_reassembly));
        if (reassembly) {
            memset(reassembly, 0, sizeof(struct ip_reassembly));
            reassembly->id = header->id;
            reassembly->src_addr = header->src_addr;
            reassembly->dst_addr = header->dst_addr;
            reassembly->timestamp = 0;  /* Would use real timestamp */
            reassembly->next = reassembly_list;
            reassembly_list = reassembly;
            
            /* Add first fragment */
            struct ip_fragment* fragment = (struct ip_fragment*)kmalloc(sizeof(struct ip_fragment));
            if (fragment) {
                fragment->id = header->id;
                fragment->src_addr = header->src_addr;
                fragment->dst_addr = header->dst_addr;
                fragment->offset = header->fragment_offset;
                fragment->last_fragment = !(header->flags & 0x1);
                fragment->data_len = header->total_length - (header->ihl * 4);
                fragment->data = (uint8_t*)kmalloc(fragment->data_len);
                if (fragment->data) {
                    memcpy(fragment->data, packet + (header->ihl * 4), fragment->data_len);
                    reassembly->fragments = fragment;
                    reassembly->received_length = fragment->data_len;
                    
                    if (fragment->last_fragment) {
                        reassembly->total_length = fragment->offset * 8 + fragment->data_len;
                    }
                } else {
                    kfree(fragment);
                }
            }
        }
        
        return 0;
    }
    
    /* Not fragmented - process directly */
    const uint8_t* payload = packet + (header->ihl * 4);
    size_t payload_len = header->total_length - (header->ihl * 4);
    
    /* Route to protocol handler */
    switch (header->protocol) {
        case IP_PROTOCOL_TCP:
            /* Call TCP handler */
            break;
        case IP_PROTOCOL_UDP:
            /* Call UDP handler */
            break;
        case IP_PROTOCOL_ICMP:
            /* Call ICMP handler */
            break;
        default:
            return -1;
    }
    
    return 0;
}

/* Send IP packet */
int ip_send(uint32_t dst_addr, uint8_t protocol, const void* data, size_t len) {
    if (!data || len == 0) {
        return -1;
    }
    
    /* Find route */
    struct ip_route* route = ip_find_route(dst_addr);
    if (!route) {
        return -1;  /* No route */
    }
    
    uint32_t src_addr = route->interface_addr;
    if (src_addr == 0) {
        return -1;
    }
    
    /* Check if fragmentation needed */
    if (len + IP_HEADER_SIZE > IP_MTU) {
        struct ip_fragment* fragments;
        size_t fragment_count;
        if (ip_fragment(src_addr, dst_addr, protocol, data, len, &fragments, &fragment_count) != 0) {
            return -1;
        }
        
        /* Send each fragment */
        struct ip_fragment* fragment = fragments;
        while (fragment) {
            struct ip_fragment* next = fragment->next;
            
            /* Create IP header for fragment */
            struct ip_header header;
            memset(&header, 0, sizeof(struct ip_header));
            header.version = 4;
            header.ihl = 5;
            header.total_length = IP_HEADER_SIZE + fragment->data_len;
            header.id = fragment->id;
            header.flags = fragment->last_fragment ? 0 : 0x1;
            header.fragment_offset = fragment->offset;
            header.ttl = 64;
            header.protocol = protocol;
            header.src_addr = src_addr;
            header.dst_addr = dst_addr;
            header.checksum = ip_calculate_checksum(&header);
            
            /* Send fragment via network interface */
            /* This would call the network driver */
            
            if (fragment->data) {
                kfree(fragment->data);
            }
            kfree(fragment);
            fragment = next;
        }
        
        return (int)len;
    }
    
    /* Single packet */
    struct ip_header header;
    memset(&header, 0, sizeof(struct ip_header));
    header.version = 4;
    header.ihl = 5;
    header.total_length = IP_HEADER_SIZE + len;
    header.id = next_packet_id++;
    header.flags = 0x2;  /* Don't fragment */
    header.fragment_offset = 0;
    header.ttl = 64;
    header.protocol = protocol;
    header.src_addr = src_addr;
    header.dst_addr = dst_addr;
    header.checksum = ip_calculate_checksum(&header);
    
    /* Send via network interface */
    /* This would call the network driver */
    
    return (int)len;
}

/* Initialize IP */
int ip_init(void) {
    route_table = NULL;
    reassembly_list = NULL;
    next_packet_id = 1;
    
    /* Add default route */
    ip_add_route(0, 0, 0, 1);  /* Default route */
    
    return 0;
}
