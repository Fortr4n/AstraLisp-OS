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

/* Buddy allocator operations */
int buddy_init(void* start, size_t size, uint32_t min_order, uint32_t max_order);
void* buddy_alloc(size_t size);
void buddy_free(void* ptr, size_t size);
void* buddy_alloc_order(uint32_t order);
void buddy_free_order(void* ptr, uint32_t order);

/* Slab allocator operations */
struct slab_cache;
struct slab_cache* slab_cache_create(const char* name, size_t obj_size, size_t align);
void* slab_alloc(struct slab_cache* cache);
void slab_free(struct slab_cache* cache, void* obj);
void slab_cache_destroy(struct slab_cache* cache);

#endif /* HEAP_H */
