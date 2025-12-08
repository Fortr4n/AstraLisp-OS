/* AstraLisp OS LFSX Filesystem */

#ifndef LFSX_H
#define LFSX_H

#include <stdint.h>
#include <stddef.h>

/* File handle */
struct lfsx_file {
    uint32_t inode;
    uint64_t offset;
    uint32_t flags;
};

/* Initialize LFSX filesystem */
int lfsx_init(void);

/* Mount filesystem */
int lfsx_mount(const char* device);

/* Open file */
struct lfsx_file* lfsx_open(const char* path, uint32_t flags);

/* Read from file */
size_t lfsx_read(struct lfsx_file* file, void* buffer, size_t size);

/* Write to file */
size_t lfsx_write(struct lfsx_file* file, const void* buffer, size_t size);

/* Close file */
void lfsx_close(struct lfsx_file* file);

/* Create transaction */
void* lfsx_transaction_begin(void);

/* Commit transaction */
int lfsx_transaction_commit(void* transaction);

/* Abort transaction */
void lfsx_transaction_abort(void* transaction);

/* Create file */
int lfsx_create(const char* path, uint32_t mode);

/* Unlink file */
int lfsx_unlink(const char* path);

/* Make directory */
int lfsx_mkdir(const char* path);

#endif /* LFSX_H */
