/* AstraLisp OS Network Drivers Implementation */

#include "network.h"
#include "../mm/heap.h"
#include <stddef.h>
#include <string.h>

static struct network_interface* interface_list = NULL;
static uint32_t next_interface_id = 1;

/* Initialize network drivers */
int network_init(void) {
    interface_list = NULL;
    /* Initialize e1000, virtio-net drivers here */
    return 0;
}

/* Register network interface */
int network_register_interface(struct network_interface* iface) {
    if (!iface) {
        return -1;
    }
    
    iface->id = next_interface_id++;
    iface->next = interface_list;
    interface_list = iface;
    
    return 0;
}

/* Send packet */
int network_send(struct network_interface* iface, const void* data, size_t size) {
    if (!iface || !data || size == 0) {
        return -1;
    }
    
    return iface->send(iface, data, size);
}

/* Receive packet */
int network_receive(struct network_interface* iface, void* buffer, size_t size) {
    if (!iface || !buffer || size == 0) {
        return -1;
    }
    
    return iface->receive(iface, buffer, size);
}
