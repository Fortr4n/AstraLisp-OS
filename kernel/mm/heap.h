/* AstraLisp OS Kernel Heap Allocator */

#ifndef HEAP_H
#define HEAP_H

#include <stddef.h>
#include <stdint.h>

/* Initialize kernel heap */
int heap_init(void);

/* Allocate memory */
void* kmalloc(size_t size);

/* Free memory */
void kfree(void* ptr);

/* Reallocate memory */
void* krealloc(void* ptr, size_t size);

/* Allocate aligned memory */
void* kmalloc_aligned(size_t size, size_t alignment);

#endif /* HEAP_H */
