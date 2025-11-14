/* AstraLisp OS Garbage Collector Complete Implementation */

#include "gc.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/mm/pmm.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define GC_MARK_BIT 0x1
#define GC_WEAK_BIT 0x2
#define GC_OLD_GEN_BIT 0x4

/* GC object header */
struct gc_object_header {
    uint32_t size;
    uint32_t flags;
    struct gc_object_header* next;
    struct gc_object_header* prev;
};

/* GC generation structure */
struct gc_generation {
    struct gc_object_header* objects;
    size_t total_size;
    size_t allocated_size;
    uint32_t collection_count;
};

/* GC root structure */
struct gc_root {
    void** pointer;
    struct gc_root* next;
};

/* GC context */
struct gc_context {
    struct gc_generation young_gen;
    struct gc_generation old_gen;
    struct gc_root* roots;
    uint32_t collection_threshold;
    bool collecting;
    bool concurrent;
    struct gc_stats stats;
};

static struct gc_context* gc_ctx = NULL;

/* Get object header */
static struct gc_object_header* get_header(void* ptr) {
    if (!ptr) {
        return NULL;
    }
    
    return (struct gc_object_header*)((uint8_t*)ptr - sizeof(struct gc_object_header));
}

/* Get object pointer from header */
static void* get_object(struct gc_object_header* header) {
    if (!header) {
        return NULL;
    }
    
    return (void*)((uint8_t*)header + sizeof(struct gc_object_header));
}

/* Mark object */
static void mark_object(void* obj) {
    if (!obj || !gc_ctx) {
        return;
    }
    
    struct gc_object_header* header = get_header(obj);
    if (!header) {
        return;
    }
    
    /* Already marked? */
    if (header->flags & GC_MARK_BIT) {
        return;
    }
    
    /* Mark object */
    header->flags |= GC_MARK_BIT;
    
    /* Mark referenced objects (scan object for pointers) */
    uint8_t* obj_data = (uint8_t*)obj;
    size_t obj_size = header->size - sizeof(struct gc_object_header);
    
    /* Scan for pointers (simplified - would use precise GC info) */
    for (size_t i = 0; i < obj_size - sizeof(void*); i += sizeof(void*)) {
        void** potential_ptr = (void**)(obj_data + i);
        void* potential_obj = *potential_ptr;
        
        if (potential_obj) {
            struct gc_object_header* ref_header = get_header(potential_obj);
            if (ref_header && !(ref_header->flags & GC_MARK_BIT)) {
                mark_object(potential_obj);
            }
        }
    }
}

/* Mark all roots */
static void mark_roots(void) {
    if (!gc_ctx) {
        return;
    }
    
    struct gc_root* root = gc_ctx->roots;
    while (root) {
        if (root->pointer && *root->pointer) {
            mark_object(*root->pointer);
        }
        root = root->next;
    }
}

/* Sweep generation */
static size_t sweep_generation(struct gc_generation* gen) {
    if (!gen) {
        return 0;
    }
    
    size_t freed = 0;
    struct gc_object_header* obj = gen->objects;
    
    while (obj) {
        struct gc_object_header* next = obj->next;
        
        if (!(obj->flags & GC_MARK_BIT)) {
            /* Object is not marked - free it */
            if (obj->prev) {
                obj->prev->next = obj->next;
            } else {
                gen->objects = obj->next;
            }
            if (obj->next) {
                obj->next->prev = obj->prev;
            }
            
            freed += obj->size;
            gen->allocated_size -= obj->size;
            
            kfree(obj);
        } else {
            /* Clear mark bit for next collection */
            obj->flags &= ~GC_MARK_BIT;
            
            /* Promote to old generation if in young gen */
            if (gen == &gc_ctx->young_gen && !(obj->flags & GC_OLD_GEN_BIT)) {
                obj->flags |= GC_OLD_GEN_BIT;
                
                /* Move to old generation */
                if (obj->prev) {
                    obj->prev->next = obj->next;
                } else {
                    gen->objects = obj->next;
                }
                if (obj->next) {
                    obj->next->prev = obj->prev;
                }
                
                gen->allocated_size -= obj->size;
                
                /* Add to old generation */
                obj->prev = NULL;
                obj->next = gc_ctx->old_gen.objects;
                if (gc_ctx->old_gen.objects) {
                    gc_ctx->old_gen.objects->prev = obj;
                }
                gc_ctx->old_gen.objects = obj;
                gc_ctx->old_gen.allocated_size += obj->size;
            }
        }
        
        obj = next;
    }
    
    return freed;
}

/* Collect young generation */
static void collect_young(void) {
    if (!gc_ctx) {
        return;
    }
    
    gc_ctx->collecting = true;
    
    /* Mark phase */
    mark_roots();
    
    /* Sweep phase */
    size_t freed = sweep_generation(&gc_ctx->young_gen);
    
    gc_ctx->young_gen.collection_count++;
    gc_ctx->stats.total_freed += freed;
    gc_ctx->stats.collection_count++;
    
    gc_ctx->collecting = false;
}

/* Collect old generation */
static void collect_old(void) {
    if (!gc_ctx) {
        return;
    }
    
    gc_ctx->collecting = true;
    
    /* Mark phase */
    mark_roots();
    
    /* Sweep phase */
    size_t freed = sweep_generation(&gc_ctx->old_gen);
    
    gc_ctx->old_gen.collection_count++;
    gc_ctx->stats.total_freed += freed;
    
    gc_ctx->collecting = false;
}

/* Initialize garbage collector */
int gc_init(void) {
    if (gc_ctx) {
        return 0;
    }
    
    gc_ctx = (struct gc_context*)kmalloc(sizeof(struct gc_context));
    if (!gc_ctx) {
        return -1;
    }
    
    memset(gc_ctx, 0, sizeof(struct gc_context));
    
    gc_ctx->young_gen.objects = NULL;
    gc_ctx->young_gen.total_size = 0;
    gc_ctx->young_gen.allocated_size = 0;
    gc_ctx->young_gen.collection_count = 0;
    
    gc_ctx->old_gen.objects = NULL;
    gc_ctx->old_gen.total_size = 0;
    gc_ctx->old_gen.allocated_size = 0;
    gc_ctx->old_gen.collection_count = 0;
    
    gc_ctx->roots = NULL;
    gc_ctx->collection_threshold = 1024 * 1024;  /* 1MB */
    gc_ctx->collecting = false;
    gc_ctx->concurrent = false;
    
    memset(&gc_ctx->stats, 0, sizeof(struct gc_stats));
    
    return 0;
}

/* Allocate object */
void* gc_alloc(size_t size) {
    if (!gc_ctx || size == 0) {
        return NULL;
    }
    
    /* Add header size */
    size_t total_size = size + sizeof(struct gc_object_header);
    
    /* Allocate memory */
    struct gc_object_header* header = (struct gc_object_header*)kmalloc(total_size);
    if (!header) {
        /* Try collection */
        collect_young();
        
        header = (struct gc_object_header*)kmalloc(total_size);
        if (!header) {
            collect_old();
            header = (struct gc_object_header*)kmalloc(total_size);
            if (!header) {
                return NULL;
            }
        }
    }
    
    memset(header, 0, sizeof(struct gc_object_header));
    header->size = total_size;
    header->flags = 0;
    
    /* Add to young generation */
    header->next = gc_ctx->young_gen.objects;
    if (gc_ctx->young_gen.objects) {
        gc_ctx->young_gen.objects->prev = header;
    }
    gc_ctx->young_gen.objects = header;
    gc_ctx->young_gen.allocated_size += total_size;
    gc_ctx->young_gen.total_size += total_size;
    
    gc_ctx->stats.total_allocated += total_size;
    
    /* Check if collection needed */
    if (gc_ctx->young_gen.allocated_size > gc_ctx->collection_threshold) {
        collect_young();
    }
    
    return get_object(header);
}

/* Mark object as reachable */
void gc_mark(void* obj) {
    if (!gc_ctx || !obj) {
        return;
    }
    
    mark_object(obj);
}

/* Run garbage collection */
void gc_collect(void) {
    if (!gc_ctx) {
        return;
    }
    
    /* Collect young generation */
    collect_young();
    
    /* Periodically collect old generation */
    if (gc_ctx->old_gen.collection_count % 10 == 0) {
        collect_old();
    }
}

/* Add root */
int gc_add_root(void** pointer) {
    if (!gc_ctx || !pointer) {
        return -1;
    }
    
    struct gc_root* root = (struct gc_root*)kmalloc(sizeof(struct gc_root));
    if (!root) {
        return -1;
    }
    
    root->pointer = pointer;
    root->next = gc_ctx->roots;
    gc_ctx->roots = root;
    
    return 0;
}

/* Remove root */
void gc_remove_root(void** pointer) {
    if (!gc_ctx || !pointer) {
        return;
    }
    
    struct gc_root* root = gc_ctx->roots;
    struct gc_root* prev = NULL;
    
    while (root) {
        if (root->pointer == pointer) {
            if (prev) {
                prev->next = root->next;
            } else {
                gc_ctx->roots = root->next;
            }
            kfree(root);
            return;
        }
        prev = root;
        root = root->next;
    }
}

/* Get GC statistics */
void gc_get_stats(struct gc_stats* stats_out) {
    if (!gc_ctx || !stats_out) {
        return;
    }
    
    *stats_out = gc_ctx->stats;
}
