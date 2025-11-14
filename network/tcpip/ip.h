/* AstraLisp OS IP Implementation */

#ifndef IP_H
#define IP_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* IP protocol numbers */
#define IP_PROTOCOL_ICMP 1
#define IP_PROTOCOL_TCP 6
#define IP_PROTOCOL_UDP 17

/* IP header */
struct ip_header {
    uint8_t version:4;
    uint8_t ihl:4;
    uint8_t tos;
    uint16_t total_length;
    uint16_t id;
    uint16_t flags:3;
    uint16_t fragment_offset:13;
    uint8_t ttl;
    uint8_t protocol;
    uint16_t checksum;
    uint32_t src_addr;
    uint32_t dst_addr;
    uint8_t options[40];
};

/* IP fragment */
struct ip_fragment {
    uint16_t id;
    uint32_t src_addr;
    uint32_t dst_addr;
    uint8_t* data;
    size_t data_len;
    uint16_t offset;
    bool last_fragment;
    uint64_t timestamp;
    struct ip_fragment* next;
};

/* IP fragment reassembly */
struct ip_reassembly {
    uint16_t id;
    uint32_t src_addr;
    uint32_t dst_addr;
    struct ip_fragment* fragments;
    uint16_t total_length;
    uint16_t received_length;
    uint64_t timestamp;
    struct ip_reassembly* next;
};

/* Route entry */
struct ip_route {
    uint32_t network;
    uint32_t netmask;
    uint32_t gateway;
    uint32_t interface_addr;
    uint8_t metric;
    struct ip_route* next;
};

/* Initialize IP */
int ip_init(void);

/* Send IP packet */
int ip_send(uint32_t dst_addr, uint8_t protocol, const void* data, size_t len);

/* Process incoming IP packet */
int ip_process_packet(const uint8_t* packet, size_t len);

/* Fragment IP packet */
int ip_fragment(uint32_t src_addr, uint32_t dst_addr, uint8_t protocol,
                const void* data, size_t len, struct ip_fragment** fragments, size_t* fragment_count);

/* Reassemble IP fragments */
int ip_reassemble_fragments(struct ip_reassembly* reassembly, uint8_t** reassembled_data, size_t* reassembled_len);

/* Add route */
int ip_add_route(uint32_t network, uint32_t netmask, uint32_t gateway, uint8_t metric);

/* Find route */
struct ip_route* ip_find_route(uint32_t dst_addr);

/* Calculate IP checksum */
uint16_t ip_calculate_checksum(const struct ip_header* header);

#endif /* IP_H */
