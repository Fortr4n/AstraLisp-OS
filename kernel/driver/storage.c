/* AstraLisp OS Storage Drivers Implementation */

#include "storage.h"
#include "../mm/heap.h"
#include <stddef.h>
#include <string.h>

static struct block_device* device_list = NULL;
static uint32_t next_device_id = 1;

/* Initialize storage drivers */
int storage_init(void) {
    device_list = NULL;
    /* Initialize AHCI, NVMe drivers here */
    return 0;
}

/* Register block device */
int storage_register_device(struct block_device* dev) {
    if (!dev) {
        return -1;
    }
    
    dev->id = next_device_id++;
    dev->next = device_list;
    device_list = dev;
    
    return 0;
}

/* Read from block device */
int storage_read(struct block_device* dev, uint64_t offset, void* buffer, size_t size) {
    if (!dev || !buffer || size == 0) {
        return -1;
    }
    
    uint64_t start_block = offset / dev->block_size;
    size_t block_count = (size + dev->block_size - 1) / dev->block_size;
    
    return dev->read(dev, start_block, buffer, block_count);
}

/* Write to block device */
int storage_write(struct block_device* dev, uint64_t offset, const void* buffer, size_t size) {
    if (!dev || !buffer || size == 0) {
        return -1;
    }
    
    uint64_t start_block = offset / dev->block_size;
    size_t block_count = (size + dev->block_size - 1) / dev->block_size;
    
    return dev->write(dev, start_block, buffer, block_count);
}

/* Find device by name */
struct block_device* storage_find_device(const char* name) {
    if (!name) {
        return NULL;
    }
    
    struct block_device* dev = device_list;
    while (dev) {
        if (strncmp(dev->name, name, sizeof(dev->name)) == 0) {
            return dev;
        }
        dev = dev->next;
    }
    
    return NULL;
}
