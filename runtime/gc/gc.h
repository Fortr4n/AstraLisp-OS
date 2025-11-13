/* AstraLisp OS Garbage Collector */

#ifndef GC_H
#define GC_H

#include <stdint.h>
#include <stddef.h>

/* GC statistics */
struct gc_stats {
    size_t total_allocated;
    size_t total_freed;
    uint32_t collection_count;
};

/* Initialize garbage collector */
int gc_init(void);

/* Allocate object */
void* gc_alloc(size_t size);

/* Mark object as reachable */
void gc_mark(void* obj);

/* Run garbage collection */
void gc_collect(void);

/* Get GC statistics */
void gc_get_stats(struct gc_stats* stats);

#endif /* GC_H */
