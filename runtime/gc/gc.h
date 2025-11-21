/* AstraLisp OS Garbage Collector */

#ifndef GC_H
#define GC_H

#include <stddef.h>
#include <stdint.h>
#include "../lisp/tagged.h"

/* GC Statistics */
struct gc_stats {
    size_t total_allocated;
    size_t total_freed;
    size_t collection_count;
};

/* Initialize GC */
int gc_init(void);

/* Allocate object */
void* gc_alloc(size_t size);

/* Mark object */
void gc_mark(lisp_value obj);

/* Run GC */
void gc_collect(void);

/* Global Roots (Global variables) */
int gc_add_root(lisp_value* root);
void gc_remove_root(lisp_value* root);

/* Shadow Stack for Local Roots (Stack variables) */
/* Usage:
 * lisp_value a = ...;
 * lisp_value b = ...;
 * GC_PUSH_2(a, b);
 * ... can trigger GC ...
 * GC_POP();
 */

#define GC_MAX_ROOTS_PER_FRAME 8

struct gc_stack_frame {
    lisp_value* roots[GC_MAX_ROOTS_PER_FRAME];
    size_t count;
    struct gc_stack_frame* prev;
};

void gc_push_frame(struct gc_stack_frame* frame);
void gc_pop_frame(void);

/* Macros for convenience */
#define GC_PUSH_1(v1) \
    do { \
        struct gc_stack_frame _frame; \
        _frame.roots[0] = &(v1); \
        _frame.count = 1; \
        gc_push_frame(&_frame); \
    } while(0)

#define GC_PUSH_2(v1, v2) \
    do { \
        struct gc_stack_frame _frame; \
        _frame.roots[0] = &(v1); \
        _frame.roots[1] = &(v2); \
        _frame.count = 2; \
        gc_push_frame(&_frame); \
    } while(0)

#define GC_PUSH_3(v1, v2, v3) \
    do { \
        struct gc_stack_frame _frame; \
        _frame.roots[0] = &(v1); \
        _frame.roots[1] = &(v2); \
        _frame.roots[2] = &(v3); \
        _frame.count = 3; \
        gc_push_frame(&_frame); \
    } while(0)

#define GC_POP() gc_pop_frame()

/* Write Barrier */
void gc_write_barrier(lisp_value obj, lisp_value* field, lisp_value new_val);

/* Get Stats */
void gc_get_stats(struct gc_stats* stats);

#endif /* GC_H */
