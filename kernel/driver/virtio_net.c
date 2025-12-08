/* AstraLisp OS - VirtIO Network Driver Implementation */
/* VirtIO 1.0/1.1 Compliant for QEMU PowerPC */

#include "virtio_net.h"
#include "network.h"
#include "../mm/pmm.h"
#include "../mm/heap.h"
#include "../hal/io.h"
#include <stddef.h>
#include <string.h>

/* Helper: Read 8-bit register */
static inline uint8_t virtio_read8(struct virtio_net_device* dev, uint16_t off) {
    return mmio_read8(dev->base + off);
}

/* Helper: Write 8-bit register */
static inline void virtio_write8(struct virtio_net_device* dev, uint16_t off, uint8_t val) {
    mmio_write8(dev->base + off, val);
}

/* Helper: Read 16-bit register */
static inline uint16_t virtio_read16(struct virtio_net_device* dev, uint16_t off) {
    return mmio_read16(dev->base + off);
}

/* Helper: Write 16-bit register */
static inline void virtio_write16(struct virtio_net_device* dev, uint16_t off, uint16_t val) {
    mmio_write16(dev->base + off, val);
}

/* Helper: Read 32-bit register */
static inline uint32_t virtio_read32(struct virtio_net_device* dev, uint16_t off) {
    return mmio_read32((volatile void*)(dev->base + off));
}

/* Helper: Write 32-bit register */
static inline void virtio_write32(struct virtio_net_device* dev, uint16_t off, uint32_t val) {
    mmio_write32((volatile void*)(dev->base + off), val);
}

/* Calculate virtqueue size in bytes */
static size_t virtqueue_size(uint16_t qsize) {
    size_t desc_size = sizeof(struct vring_desc) * qsize;
    size_t avail_size = sizeof(struct vring_avail) + sizeof(uint16_t) * qsize;
    size_t used_size = sizeof(struct vring_used) + sizeof(struct vring_used_elem) * qsize;
    
    /* Align */
    size_t size = 0;
    size += (desc_size + 4095) & ~4095;
    size += avail_size;
    size = (size + 4095) & ~4095;
    size += used_size;
    
    return size;
}

/* Initialize a virtqueue */
static int virtqueue_init(struct virtio_net_device* dev, struct virtqueue* vq, uint16_t qid) {
    /* Select queue */
    virtio_write16(dev, VIRTIO_PCI_QUEUE_SEL, qid);
    
    /* Get queue size */
    uint16_t qsize = virtio_read16(dev, VIRTIO_PCI_QUEUE_SIZE);
    if (qsize == 0 || qsize > 256) {
        return -1;
    }
    
    vq->qid = qid;
    vq->size = qsize;
    vq->free_head = 0;
    vq->num_free = qsize;
    vq->last_used_idx = 0;
    
    /* Allocate queue memory */
    size_t total_size = virtqueue_size(qsize);
    size_t pages = (total_size + 4095) / 4096;
    void* queue_mem = pmm_alloc_multiple(pages);
    if (!queue_mem) {
        return -1;
    }
    memset(queue_mem, 0, pages * 4096);
    
    /* Setup pointers */
    vq->desc = (struct vring_desc*)queue_mem;
    
    size_t avail_offset = sizeof(struct vring_desc) * qsize;
    avail_offset = (avail_offset + 1) & ~1;  /* Align to 2 bytes */
    vq->avail = (struct vring_avail*)((uint8_t*)queue_mem + avail_offset);
    
    size_t used_offset = avail_offset + sizeof(struct vring_avail) + sizeof(uint16_t) * qsize;
    used_offset = (used_offset + 4095) & ~4095;  /* Align to page */
    vq->used = (struct vring_used*)((uint8_t*)queue_mem + used_offset);
    
    /* Initialize free descriptor list */
    for (uint16_t i = 0; i < qsize - 1; i++) {
        vq->desc[i].next = i + 1;
    }
    vq->desc[qsize - 1].next = 0;
    
    /* Allocate buffer tracking array */
    vq->buffers = kmalloc(sizeof(void*) * qsize);
    if (!vq->buffers) {
        pmm_free(queue_mem);
        return -1;
    }
    memset(vq->buffers, 0, sizeof(void*) * qsize);
    
    /* Tell device about the queue */
    uint64_t queue_phys = (uint64_t)(uintptr_t)queue_mem;
    virtio_write32(dev, VIRTIO_PCI_QUEUE_PFN, (uint32_t)(queue_phys >> 12));
    
    /* Set notify address */
    vq->notify = (volatile uint16_t*)(dev->base + VIRTIO_PCI_QUEUE_NOTIFY);
    
    return 0;
}

/* Add buffer to RX queue */
static int virtio_net_add_rx_buffer(struct virtio_net_device* dev) {
    struct virtqueue* vq = &dev->rx_queue;
    
    if (vq->num_free == 0) {
        return -1;
    }
    
    /* Allocate receive buffer */
    void* buffer = kmalloc(VIRTIO_NET_MAX_PACKET + VIRTIO_NET_HDR_SIZE);
    if (!buffer) {
        return -1;
    }
    
    /* Get free descriptor */
    uint16_t desc_idx = vq->free_head;
    vq->free_head = vq->desc[desc_idx].next;
    vq->num_free--;
    
    /* Setup descriptor */
    vq->desc[desc_idx].addr = (uint64_t)(uintptr_t)buffer;
    vq->desc[desc_idx].len = VIRTIO_NET_MAX_PACKET + VIRTIO_NET_HDR_SIZE;
    vq->desc[desc_idx].flags = VRING_DESC_F_WRITE;
    vq->desc[desc_idx].next = 0;
    
    /* Track buffer */
    vq->buffers[desc_idx] = buffer;
    
    /* Add to available ring */
    uint16_t avail_idx = vq->avail->idx;
    vq->avail->ring[avail_idx % vq->size] = desc_idx;
    __sync_synchronize();  /* Memory barrier */
    vq->avail->idx = avail_idx + 1;
    
    /* Notify device */
    *vq->notify = vq->qid;
    
    return 0;
}

/* Simple pseudo-random generator for fallback MAC */
/* In production, link to kernel CSPRNG */
static uint32_t net_rng_state = 0xDEADBEEF;
static uint8_t get_random_byte(void) {
    net_rng_state = net_rng_state * 1103515245 + 12345;
    return (uint8_t)(net_rng_state >> 16);
}

/* Initialize VirtIO net device */
int virtio_net_init(struct virtio_net_device* dev, volatile void* bar0) {
    if (!dev || !bar0) return -1;
    
    dev->base = (volatile uint8_t*)bar0;
    
    /* Reset device */
    virtio_write8(dev, VIRTIO_PCI_STATUS, 0);
    
    /* Acknowledge device */
    virtio_write8(dev, VIRTIO_PCI_STATUS, VIRTIO_STATUS_ACK);
    
    /* Driver loaded */
    virtio_write8(dev, VIRTIO_PCI_STATUS, VIRTIO_STATUS_ACK | VIRTIO_STATUS_DRIVER);
    
    /* Negotiate features */
    uint32_t host_features = virtio_read32(dev, VIRTIO_PCI_HOST_FEATURES);
    
    /* Validate critically needed features */
    if (!(host_features & VIRTIO_NET_F_MAC)) {
        /* Warn: No hardware MAC */
    }
    
    dev->features = host_features & (VIRTIO_NET_F_MAC | VIRTIO_NET_F_STATUS);
    virtio_write32(dev, VIRTIO_PCI_GUEST_FEATURES, dev->features);
    
    /* Read MAC address */
    if (dev->features & VIRTIO_NET_F_MAC) {
        for (int i = 0; i < 6; i++) {
            dev->mac[i] = virtio_read8(dev, VIRTIO_PCI_CONFIG + i);
        }
    } else {
        /* Generate valid local unicast MAC */
        /* x2:xx:xx:xx:xx:xx is locally administered */
        dev->mac[0] = 0x02; 
        dev->mac[1] = get_random_byte();
        dev->mac[2] = get_random_byte();
        dev->mac[3] = get_random_byte();
        dev->mac[4] = get_random_byte();
        dev->mac[5] = get_random_byte();
    }
    
    dev->mtu = 1500;
    
    /* Initialize RX queue */
    if (virtqueue_init(dev, &dev->rx_queue, VIRTIO_NET_RX_QUEUE) != 0) {
        virtio_write8(dev, VIRTIO_PCI_STATUS, VIRTIO_STATUS_FAILED);
        return -1;
    }
    
    /* Initialize TX queue */
    if (virtqueue_init(dev, &dev->tx_queue, VIRTIO_NET_TX_QUEUE) != 0) {
        virtio_write8(dev, VIRTIO_PCI_STATUS, VIRTIO_STATUS_FAILED);
        return -1;
    }
    
    /* Features OK */
    virtio_write8(dev, VIRTIO_PCI_STATUS, VIRTIO_STATUS_ACK | VIRTIO_STATUS_DRIVER | VIRTIO_STATUS_FEATURES_OK);
    
    /* Re-read status to confirm device accepted features */
    uint8_t status = virtio_read8(dev, VIRTIO_PCI_STATUS);
    if (!(status & VIRTIO_STATUS_FEATURES_OK)) {
        return -1;
    }
    
    /* Driver OK */
    virtio_write8(dev, VIRTIO_PCI_STATUS, VIRTIO_STATUS_ACK | VIRTIO_STATUS_DRIVER | VIRTIO_STATUS_FEATURES_OK | VIRTIO_STATUS_DRIVER_OK);
    
    /* Add receive buffers */
    for (int i = 0; i < 8; i++) {
        virtio_net_add_rx_buffer(dev);
    }
    
    return 0;
}

/* Send packet */
int virtio_net_send(struct network_interface* iface, const void* data, size_t size) {
    if (!iface || !data || size == 0 || size > VIRTIO_NET_MAX_PACKET) {
        return -1;
    }
    
    struct virtio_net_device* dev = (struct virtio_net_device*)iface->private_data;
    struct virtqueue* vq = &dev->tx_queue;
    
    if (vq->num_free < 2) {
        return -1;  /* No free descriptors */
    }
    
    /* Allocate buffer with header */
    size_t total_size = size + VIRTIO_NET_HDR_SIZE;
    void* buffer = kmalloc(total_size);
    if (!buffer) {
        return -1;
    }
    
    /* Setup VirtIO net header */
    struct virtio_net_hdr* hdr = (struct virtio_net_hdr*)buffer;
    memset(hdr, 0, VIRTIO_NET_HDR_SIZE);
    
    /* Copy data */
    memcpy((uint8_t*)buffer + VIRTIO_NET_HDR_SIZE, data, size);
    
    /* Get free descriptor */
    uint16_t desc_idx = vq->free_head;
    vq->free_head = vq->desc[desc_idx].next;
    vq->num_free--;
    
    /* Setup descriptor */
    vq->desc[desc_idx].addr = (uint64_t)(uintptr_t)buffer;
    vq->desc[desc_idx].len = total_size;
    vq->desc[desc_idx].flags = 0;  /* No flags for TX */
    vq->desc[desc_idx].next = 0;
    
    /* Track buffer */
    vq->buffers[desc_idx] = buffer;
    
    /* Add to available ring */
    uint16_t avail_idx = vq->avail->idx;
    vq->avail->ring[avail_idx % vq->size] = desc_idx;
    __sync_synchronize();
    vq->avail->idx = avail_idx + 1;
    
    /* Notify device */
    *vq->notify = vq->qid;
    
    return (int)size;
}

/* Receive packet (polling mode) */
int virtio_net_receive(struct network_interface* iface, void* buffer, size_t size) {
    if (!iface || !buffer || size == 0) {
        return -1;
    }
    
    struct virtio_net_device* dev = (struct virtio_net_device*)iface->private_data;
    struct virtqueue* vq = &dev->rx_queue;
    
    /* Check for used buffers */
    if (vq->last_used_idx == vq->used->idx) {
        return 0;  /* No packets */
    }
    
    /* Get used element */
    struct vring_used_elem* used = &vq->used->ring[vq->last_used_idx % vq->size];
    uint16_t desc_idx = used->id;
    uint32_t len = used->len;
    
    /* Get buffer */
    void* rx_buffer = vq->buffers[desc_idx];
    if (!rx_buffer) {
        return -1;
    }
    
    /* Copy data (skip VirtIO header) */
    size_t data_len = len - VIRTIO_NET_HDR_SIZE;
    if (data_len > size) {
        data_len = size;
    }
    memcpy(buffer, (uint8_t*)rx_buffer + VIRTIO_NET_HDR_SIZE, data_len);
    
    /* Return descriptor to free list */
    vq->desc[desc_idx].next = vq->free_head;
    vq->free_head = desc_idx;
    vq->num_free++;
    
    /* Free old buffer and add new one */
    kfree(rx_buffer);
    vq->buffers[desc_idx] = NULL;
    vq->last_used_idx++;
    
    /* Replenish RX buffer */
    virtio_net_add_rx_buffer(dev);
    
    return (int)data_len;
}

/* Handle interrupt */
void virtio_net_irq(struct virtio_net_device* dev) {
    /* Read ISR to acknowledge */
    uint8_t isr = virtio_read8(dev, VIRTIO_PCI_ISR);
    (void)isr;
    
    /* Process completed TX descriptors */
    struct virtqueue* tx_vq = &dev->tx_queue;
    while (tx_vq->last_used_idx != tx_vq->used->idx) {
        struct vring_used_elem* used = &tx_vq->used->ring[tx_vq->last_used_idx % tx_vq->size];
        uint16_t desc_idx = used->id;
        
        /* Free TX buffer */
        if (tx_vq->buffers[desc_idx]) {
            kfree(tx_vq->buffers[desc_idx]);
            tx_vq->buffers[desc_idx] = NULL;
        }
        
        /* Return descriptor */
        tx_vq->desc[desc_idx].next = tx_vq->free_head;
        tx_vq->free_head = desc_idx;
        tx_vq->num_free++;
        tx_vq->last_used_idx++;
    }
}
