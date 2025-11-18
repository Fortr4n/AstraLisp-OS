/* AstraLisp OS Garbage Collector Complete Implementation
 * Features:
 * - Generational GC with young/old generations
 * - Write barriers for tracking old→young references
 * - Remembered set (hash table based) for efficient tracking
 * - Card table for fine-grained tracking (512-byte cards)
 * - Mark and sweep with object promotion
 */

#include "gc.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/mm/pmm.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define GC_MARK_BIT 0x1
#define GC_WEAK_BIT 0x2
#define GC_OLD_GEN_BIT 0x4

/* Card table constants */
#define CARD_SIZE 512  /* 512-byte cards for efficient tracking */
#define CARD_TABLE_SIZE (256 * 1024)  /* Support up to 128MB heap (256K cards * 512 bytes) */
#define CARD_CLEAN 0
#define CARD_DIRTY 1

/* Remembered set constants */
#define REMEMBERED_SET_SIZE 4096  /* Hash table with 4096 buckets */

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

/* Remembered set entry (hash table node) */
struct remembered_entry {
    void* old_obj;                  /* Old generation object */
    void* young_ref;                /* Young generation reference */
    struct remembered_entry* next;  /* Hash chain */
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

    /* Write barrier support */
    uint8_t* card_table;                              /* Card table for dirty tracking */
    struct remembered_entry* remembered_set[REMEMBERED_SET_SIZE];  /* Hash table */
    size_t remembered_count;                          /* Number of entries in remembered set */
    bool write_barrier_enabled;                       /* Write barrier on/off */
};

static struct gc_context* gc_ctx = NULL;

/* ========== Utility Functions ========== */

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

/* Check if object is in old generation */
static bool is_old_generation(void* obj) {
    if (!obj || !gc_ctx) {
        return false;
    }

    struct gc_object_header* header = get_header(obj);
    if (!header) {
        return false;
    }

    return (header->flags & GC_OLD_GEN_BIT) != 0;
}

/* Check if object is in young generation */
static bool is_young_generation(void* obj) {
    return !is_old_generation(obj);
}

/* ========== Card Table Management ========== */

/* Get card index for address */
static inline size_t get_card_index(void* addr) {
    uintptr_t ptr_val = (uintptr_t)addr;
    return (ptr_val / CARD_SIZE) % CARD_TABLE_SIZE;
}

/* Mark card as dirty */
static inline void mark_card_dirty(void* addr) {
    if (!gc_ctx || !gc_ctx->card_table) {
        return;
    }

    size_t card_idx = get_card_index(addr);
    gc_ctx->card_table[card_idx] = CARD_DIRTY;
}

/* Mark card as clean */
static inline void mark_card_clean(void* addr) {
    if (!gc_ctx || !gc_ctx->card_table) {
        return;
    }

    size_t card_idx = get_card_index(addr);
    gc_ctx->card_table[card_idx] = CARD_CLEAN;
}

/* Check if card is dirty */
static inline bool is_card_dirty(void* addr) {
    if (!gc_ctx || !gc_ctx->card_table) {
        return false;
    }

    size_t card_idx = get_card_index(addr);
    return gc_ctx->card_table[card_idx] == CARD_DIRTY;
}

/* ========== Remembered Set Management ========== */

/* Hash function for remembered set */
static uint32_t hash_pointer(void* ptr) {
    uintptr_t val = (uintptr_t)ptr;
    /* Simple hash: mix bits */
    val = ((val >> 16) ^ val) * 0x45d9f3b;
    val = ((val >> 16) ^ val) * 0x45d9f3b;
    val = (val >> 16) ^ val;
    return val % REMEMBERED_SET_SIZE;
}

/* Add to remembered set */
static void remembered_set_add(void* old_obj, void* young_ref) {
    if (!gc_ctx || !old_obj || !young_ref) {
        return;
    }

    uint32_t hash = hash_pointer(old_obj);

    /* Check if already exists */
    struct remembered_entry* entry = gc_ctx->remembered_set[hash];
    while (entry) {
        if (entry->old_obj == old_obj && entry->young_ref == young_ref) {
            return;  /* Already in set */
        }
        entry = entry->next;
    }

    /* Allocate new entry */
    entry = (struct remembered_entry*)kmalloc(sizeof(struct remembered_entry));
    if (!entry) {
        return;  /* Out of memory - degraded performance but not fatal */
    }

    entry->old_obj = old_obj;
    entry->young_ref = young_ref;
    entry->next = gc_ctx->remembered_set[hash];
    gc_ctx->remembered_set[hash] = entry;
    gc_ctx->remembered_count++;
}

/* Clear remembered set */
static void remembered_set_clear(void) {
    if (!gc_ctx) {
        return;
    }

    for (size_t i = 0; i < REMEMBERED_SET_SIZE; i++) {
        struct remembered_entry* entry = gc_ctx->remembered_set[i];
        while (entry) {
            struct remembered_entry* next = entry->next;
            kfree(entry);
            entry = next;
        }
        gc_ctx->remembered_set[i] = NULL;
    }

    gc_ctx->remembered_count = 0;
}

/* Scan remembered set and mark referenced young objects */
static void scan_remembered_set(void) {
    if (!gc_ctx) {
        return;
    }

    for (size_t i = 0; i < REMEMBERED_SET_SIZE; i++) {
        struct remembered_entry* entry = gc_ctx->remembered_set[i];
        while (entry) {
            /* Mark the young object referenced by old object */
            if (entry->young_ref && is_young_generation(entry->young_ref)) {
                struct gc_object_header* header = get_header(entry->young_ref);
                if (header && !(header->flags & GC_MARK_BIT)) {
                    mark_object(entry->young_ref);  /* Forward reference to mark_object */
                }
            }
            entry = entry->next;
        }
    }
}

/* ========== Write Barrier (CRITICAL for Generational GC) ========== */

/*
 * Write barrier - MUST be called whenever an object field is modified
 * This prevents premature collection of young objects referenced by old objects
 *
 * Usage: gc_write_barrier(old_obj, old_obj->field, new_value);
 * Then: old_obj->field = new_value;
 */
void gc_write_barrier(void* obj, void** field_addr, void* new_value) {
    if (!gc_ctx || !obj || !field_addr || !gc_ctx->write_barrier_enabled) {
        return;
    }

    /* No barrier needed if GC is currently collecting (already marking) */
    if (gc_ctx->collecting) {
        return;
    }

    /* Check if this is an old→young reference */
    if (is_old_generation(obj) && new_value && is_young_generation(new_value)) {
        /* This is a critical old→young reference - track it! */

        /* Add to remembered set */
        remembered_set_add(obj, new_value);

        /* Mark card table as dirty */
        mark_card_dirty(obj);
    }
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

    /* CRITICAL: Scan remembered set to mark young objects referenced by old objects */
    /* This prevents premature collection of young objects still reachable from old gen */
    scan_remembered_set();

    /* Sweep phase */
    size_t freed = sweep_generation(&gc_ctx->young_gen);

    /* Clear remembered set after collection (will be rebuilt by write barriers) */
    remembered_set_clear();

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

    /* Initialize generations */
    gc_ctx->young_gen.objects = NULL;
    gc_ctx->young_gen.total_size = 0;
    gc_ctx->young_gen.allocated_size = 0;
    gc_ctx->young_gen.collection_count = 0;

    gc_ctx->old_gen.objects = NULL;
    gc_ctx->old_gen.total_size = 0;
    gc_ctx->old_gen.allocated_size = 0;
    gc_ctx->old_gen.collection_count = 0;

    /* Initialize roots */
    gc_ctx->roots = NULL;

    /* Initialize GC parameters */
    gc_ctx->collection_threshold = 1024 * 1024;  /* 1MB */
    gc_ctx->collecting = false;
    gc_ctx->concurrent = false;

    /* Allocate card table */
    gc_ctx->card_table = (uint8_t*)kmalloc(CARD_TABLE_SIZE);
    if (!gc_ctx->card_table) {
        kfree(gc_ctx);
        gc_ctx = NULL;
        return -1;
    }
    memset(gc_ctx->card_table, CARD_CLEAN, CARD_TABLE_SIZE);

    /* Initialize remembered set (hash table) */
    memset(gc_ctx->remembered_set, 0, sizeof(gc_ctx->remembered_set));
    gc_ctx->remembered_count = 0;

    /* Enable write barrier */
    gc_ctx->write_barrier_enabled = true;

    /* Initialize statistics */
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
