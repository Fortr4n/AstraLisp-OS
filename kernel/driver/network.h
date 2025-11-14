/* AstraLisp OS Network Drivers */

#ifndef NETWORK_H
#define NETWORK_H

#include <stdint.h>
#include <stddef.h>

/* Network interface structure */
struct network_interface {
    uint32_t id;
    uint8_t mac_address[6];
    uint32_t mtu;
    int (*send)(struct network_interface* iface, const void* data, size_t size);
    int (*receive)(struct network_interface* iface, void* buffer, size_t size);
    void* private_data;
};

/* Initialize network drivers */
int network_init(void);

/* Register network interface */
int network_register_interface(struct network_interface* iface);

/* Send packet */
int network_send(struct network_interface* iface, const void* data, size_t size);

/* Receive packet */
int network_receive(struct network_interface* iface, void* buffer, size_t size);

#endif /* NETWORK_H */
