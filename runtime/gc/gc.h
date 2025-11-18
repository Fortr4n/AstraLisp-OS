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

/*
 * Write barrier - MUST be called when modifying object references
 * This is CRITICAL for generational GC correctness!
 *
 * Usage:
 *   gc_write_barrier(obj, &obj->field, new_value);
 *   obj->field = new_value;  // Then perform the actual write
 *
 * Prevents premature collection of young objects referenced by old objects
 */
void gc_write_barrier(void* obj, void** field_addr, void* new_value);

/* Add root pointer */
int gc_add_root(void** pointer);

/* Remove root pointer */
void gc_remove_root(void** pointer);

/* Get GC statistics */
void gc_get_stats(struct gc_stats* stats);

#endif /* GC_H */
