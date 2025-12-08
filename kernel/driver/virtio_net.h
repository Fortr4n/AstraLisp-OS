/* AstraLisp OS - VirtIO Network Driver Header */
/* VirtIO 1.1 Spec Compliant */

#ifndef _KERNEL_DRIVER_VIRTIO_NET_H
#define _KERNEL_DRIVER_VIRTIO_NET_H

#include <stdint.h>
#include <stdbool.h>
#include "network.h"

/* VirtIO PCI Config Space */
#define VIRTIO_PCI_HOST_FEATURES    0x00
#define VIRTIO_PCI_GUEST_FEATURES   0x04
#define VIRTIO_PCI_QUEUE_PFN        0x08
#define VIRTIO_PCI_QUEUE_SIZE       0x0C
#define VIRTIO_PCI_QUEUE_SEL        0x0E
#define VIRTIO_PCI_QUEUE_NOTIFY     0x10
#define VIRTIO_PCI_STATUS           0x12
#define VIRTIO_PCI_ISR              0x13
#define VIRTIO_PCI_CONFIG           0x14

/* VirtIO Status Bits */
#define VIRTIO_STATUS_ACK           0x01
#define VIRTIO_STATUS_DRIVER        0x02
#define VIRTIO_STATUS_DRIVER_OK     0x04
#define VIRTIO_STATUS_FEATURES_OK   0x08
#define VIRTIO_STATUS_FAILED        0x80

/* VirtIO Net Features */
#define VIRTIO_NET_F_CSUM           (1 << 0)
#define VIRTIO_NET_F_GUEST_CSUM     (1 << 1)
#define VIRTIO_NET_F_MAC            (1 << 5)
#define VIRTIO_NET_F_STATUS         (1 << 16)
#define VIRTIO_NET_F_MRG_RXBUF      (1 << 15)

/* Virtqueue Descriptor Flags */
#define VRING_DESC_F_NEXT           1
#define VRING_DESC_F_WRITE          2
#define VRING_DESC_F_INDIRECT       4

/* Queue IDs */
#define VIRTIO_NET_RX_QUEUE         0
#define VIRTIO_NET_TX_QUEUE         1
#define VIRTIO_NET_CTRL_QUEUE       2

/* Max packet size */
#define VIRTIO_NET_MAX_PACKET       1514
#define VIRTIO_NET_HDR_SIZE         10

/* Virtqueue Descriptor */
struct vring_desc {
    uint64_t addr;
    uint32_t len;
    uint16_t flags;
    uint16_t next;
} __attribute__((packed));

/* Virtqueue Available Ring */
struct vring_avail {
    uint16_t flags;
    uint16_t idx;
    uint16_t ring[];
} __attribute__((packed));

/* Virtqueue Used Element */
struct vring_used_elem {
    uint32_t id;
    uint32_t len;
} __attribute__((packed));

/* Virtqueue Used Ring */
struct vring_used {
    uint16_t flags;
    uint16_t idx;
    struct vring_used_elem ring[];
} __attribute__((packed));

/* Virtqueue */
struct virtqueue {
    uint16_t qid;
    uint16_t size;
    struct vring_desc* desc;
    struct vring_avail* avail;
    struct vring_used* used;
    uint16_t free_head;
    uint16_t num_free;
    uint16_t last_used_idx;
    volatile uint16_t* notify;
    void** buffers;     /* Buffer tracking */
};

/* VirtIO Net Header */
struct virtio_net_hdr {
    uint8_t  flags;
    uint8_t  gso_type;
    uint16_t hdr_len;
    uint16_t gso_size;
    uint16_t csum_start;
    uint16_t csum_offset;
} __attribute__((packed));

/* VirtIO Net Device */
struct virtio_net_device {
    volatile uint8_t* base;         /* PCI BAR0 */
    struct virtqueue rx_queue;
    struct virtqueue tx_queue;
    uint8_t mac[6];
    uint16_t mtu;
    uint32_t features;
    struct network_interface* iface;
};

/* Initialize VirtIO net device */
int virtio_net_init(struct virtio_net_device* dev, volatile void* bar0);

/* Probe for VirtIO net devices */
int virtio_net_probe(void);

/* Send packet */
int virtio_net_send(struct network_interface* iface, const void* data, size_t size);

/* Receive packet */
int virtio_net_receive(struct network_interface* iface, void* buffer, size_t size);

/* Handle interrupt */
void virtio_net_irq(struct virtio_net_device* dev);

#endif
