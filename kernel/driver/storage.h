/* AstraLisp OS Storage Drivers */

#ifndef STORAGE_H
#define STORAGE_H

#include <stdint.h>
#include <stddef.h>

/* Block device structure */
struct block_device {
    uint32_t id;
    uint32_t block_size;
    uint64_t block_count;
    int (*read)(struct block_device* dev, uint64_t block, void* buffer, size_t count);
    int (*write)(struct block_device* dev, uint64_t block, const void* buffer, size_t count);
    void* private_data;
};

/* Initialize storage drivers */
int storage_init(void);

/* Register block device */
int storage_register_device(struct block_device* dev);

/* Read from block device */
int storage_read(struct block_device* dev, uint64_t offset, void* buffer, size_t size);

/* Write to block device */
int storage_write(struct block_device* dev, uint64_t offset, const void* buffer, size_t size);

#endif /* STORAGE_H */
