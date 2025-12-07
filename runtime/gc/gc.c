/* AstraLisp OS Garbage Collector Complete Implementation */

#include "gc.h"
#include "../lisp/objects.h"
#include "../lisp/types.h"
#include "../lisp/hashtable.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/mm/pmm.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>


#define GC_MARK_BIT 0x1
#define GC_WEAK_BIT 0x2
#define GC_OLD_GEN_BIT 0x4

/* Card table constants */
#define CARD_SIZE 512
#define CARD_TABLE_SIZE (256 * 1024)
#define CARD_CLEAN 0
#define CARD_DIRTY 1

/* Remembered set constants */
#define REMEMBERED_SET_SIZE 4096

/* GC object header (Internal) */
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

/* GC root structure (Global Roots) */
struct gc_root {
    lisp_value* pointer;
    struct gc_root* next;
};

/* Remembered set entry */
struct remembered_entry {
    void* old_obj;
    lisp_value young_ref;
    struct remembered_entry* next;
};

/* GC context */
struct gc_context {
    struct gc_generation young_gen;
    struct gc_generation old_gen;
    struct gc_root* roots;              /* Global roots */
    struct hash_table* symbol_table;    /* Registered symbol table */
    struct gc_stack_frame* stack_top;   /* Shadow stack top */
    uint32_t collection_threshold;
    bool collecting;
    bool concurrent;
    struct gc_stats stats;

    /* Write barrier support */
    uint8_t* card_table;
    struct remembered_entry* remembered_set[REMEMBERED_SET_SIZE];
    size_t remembered_count;
    bool write_barrier_enabled;
};

static struct gc_context* gc_ctx = NULL;

/* Forward declarations */
static void mark_object(lisp_value obj);

/* ========== Shadow Stack Management ========== */

void gc_push_frame(struct gc_stack_frame* frame) {
    if (!gc_ctx || !frame) return;
    frame->prev = gc_ctx->stack_top;
    gc_ctx->stack_top = frame;
}

void gc_pop_frame(void) {
    if (!gc_ctx || !gc_ctx->stack_top) return;
    gc_ctx->stack_top = gc_ctx->stack_top->prev;
}

/* ========== Utility Functions ========== */

static struct gc_object_header* get_header(void* ptr) {
    if (!ptr) return NULL;
    return (struct gc_object_header*)((uint8_t*)ptr - sizeof(struct gc_object_header));
}

static void* get_object(struct gc_object_header* header) {
    if (!header) return NULL;
    return (void*)((uint8_t*)header + sizeof(struct gc_object_header));
}

static bool is_old_generation(void* obj) {
    if (!obj || !gc_ctx) return false;
    struct gc_object_header* header = get_header(obj);
    if (!header) return false;
    return (header->flags & GC_OLD_GEN_BIT) != 0;
}

static bool is_young_generation(void* obj) {
    return !is_old_generation(obj);
}

/* ========== Card Table Management ========== */

static inline size_t get_card_index(void* addr) {
    uintptr_t ptr_val = (uintptr_t)addr;
    return (ptr_val / CARD_SIZE) % CARD_TABLE_SIZE;
}

static inline void mark_card_dirty(void* addr) {
    if (!gc_ctx || !gc_ctx->card_table) return;
    size_t card_idx = get_card_index(addr);
    gc_ctx->card_table[card_idx] = CARD_DIRTY;
}

static inline void mark_card_clean(void* addr) {
    if (!gc_ctx || !gc_ctx->card_table) return;
    size_t card_idx = get_card_index(addr);
    gc_ctx->card_table[card_idx] = CARD_CLEAN;
}

static inline bool is_card_dirty(void* addr) {
    if (!gc_ctx || !gc_ctx->card_table) return false;
    size_t card_idx = get_card_index(addr);
    return gc_ctx->card_table[card_idx] == CARD_DIRTY;
}

/* ========== Remembered Set Management ========== */

static uint32_t hash_pointer(void* ptr) {
    uintptr_t val = (uintptr_t)ptr;
    val = ((val >> 16) ^ val) * 0x45d9f3b;
    val = ((val >> 16) ^ val) * 0x45d9f3b;
    val = (val >> 16) ^ val;
    return val % REMEMBERED_SET_SIZE;
}

static void remembered_set_add(void* old_obj, lisp_value young_ref) {
    if (!gc_ctx || !old_obj) return;

    uint32_t hash = hash_pointer(old_obj);
    struct remembered_entry* entry = gc_ctx->remembered_set[hash];
    while (entry) {
        if (entry->old_obj == old_obj && entry->young_ref == young_ref) return;
        entry = entry->next;
    }

    entry = (struct remembered_entry*)kmalloc(sizeof(struct remembered_entry));
    if (!entry) return;

    entry->old_obj = old_obj;
    entry->young_ref = young_ref;
    entry->next = gc_ctx->remembered_set[hash];
    gc_ctx->remembered_set[hash] = entry;
    gc_ctx->remembered_count++;
}

static void remembered_set_clear(void) {
    if (!gc_ctx) return;
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

static void scan_remembered_set(void) {
    if (!gc_ctx) return;
    for (size_t i = 0; i < REMEMBERED_SET_SIZE; i++) {
        struct remembered_entry* entry = gc_ctx->remembered_set[i];
        while (entry) {
            if (IS_POINTER(entry->young_ref)) {
                 mark_object(entry->young_ref);
            }
            entry = entry->next;
        }
    }
}

/* ========== Write Barrier ========== */

void gc_write_barrier(lisp_value obj, lisp_value* field, lisp_value new_value) {
    if (!gc_ctx || !IS_POINTER(obj) || !field || !gc_ctx->write_barrier_enabled) return;
    if (gc_ctx->collecting) return;
    
    void* obj_ptr = PTR_VAL(obj);
    if (is_old_generation(obj_ptr) && IS_POINTER(new_value) && is_young_generation(PTR_VAL(new_value))) {
        remembered_set_add(obj_ptr, new_value);
        mark_card_dirty(obj_ptr);
    }
}

static void mark_object(lisp_value obj) {
    if (!gc_ctx || !IS_POINTER(obj)) return;
    
    void* ptr = PTR_VAL(obj);
    struct gc_object_header* header = get_header(ptr);
    if (!header) return;
    
    if (header->flags & GC_MARK_BIT) return;
    header->flags |= GC_MARK_BIT;
    
    // printf("GC: marking object %p type %d\n", header, GET_TYPE(ptr));
    
    heap_type_t type = GET_TYPE(ptr);
    switch (type) {
        case TYPE_CONS: {
            struct lisp_cons* cons = (struct lisp_cons*)ptr;
            mark_object(cons->car);
            mark_object(cons->cdr);
            break;
        }
        case TYPE_SYMBOL: {
            struct lisp_symbol* sym = (struct lisp_symbol*)ptr;
            mark_object(sym->name);
            mark_object(sym->value);
            mark_object(sym->function);
            mark_object(sym->plist);
            mark_object(sym->package);
            break;
        }
        case TYPE_VECTOR: {
            struct lisp_vector* vec = (struct lisp_vector*)ptr;
            for (uint64_t i = 0; i < vec->length; i++) {
                mark_object(vec->data[i]);
            }
            break;
        }
        case TYPE_FUNCTION: {
            struct lisp_function* func = (struct lisp_function*)ptr;
            mark_object(func->code);
            mark_object(func->env);
            mark_object(func->args);
            mark_object(func->name);
            break;
        }
        case TYPE_ENV: {
            struct lisp_env* env = (struct lisp_env*)ptr;
            mark_object(env->parent);
            if (env->table) {
                /* Iterate hash table */
                for (uint32_t i = 0; i < env->table->bucket_count; i++) {
                    struct hash_entry* entry = env->table->buckets[i];
                    while (entry) {
                        mark_object(entry->key);
                        mark_object(entry->value);
                        entry = entry->next;
                    }
                }
            }
            break;
        }
        case TYPE_BUILTIN: {
            struct lisp_builtin* b = (struct lisp_builtin*)ptr;
            mark_object(b->name);
            break;
        }
        default: break;
    }
}

static void mark_roots(void) {
    if (!gc_ctx) return;
    
    /* Mark global roots */
    struct gc_root* root = gc_ctx->roots;
    while (root) {
        if (root->pointer) {
            // printf("GC: marking root %p\n", root->pointer);
            mark_object(*root->pointer);
        }
        root = root->next;
    }

    /* Mark shadow stack roots */
    struct gc_stack_frame* frame = gc_ctx->stack_top;
    while (frame) {
        for (size_t i = 0; i < frame->count; i++) {
            if (frame->roots[i]) {
                mark_object(*frame->roots[i]);
            }
        }
        frame = frame->prev;
    }
    
    /* Mark symbol table */
    if (gc_ctx->symbol_table) {
        struct hash_table* table = gc_ctx->symbol_table;
        for (uint32_t i = 0; i < table->bucket_count; i++) {
            struct hash_entry* entry = table->buckets[i];
            while (entry) {
                mark_object(entry->key);
                mark_object(entry->value);
                entry = entry->next;
            }
        }
    }
}

/* ========== Sweeping ========== */

static size_t sweep_generation(struct gc_generation* gen) {
    if (!gen) return 0;
    
    size_t freed = 0;
    struct gc_object_header* obj = gen->objects;
    
    while (obj) {
        struct gc_object_header* next = obj->next;
        
        if (!(obj->flags & GC_MARK_BIT)) {
            /* Free */
            if (obj->prev) obj->prev->next = obj->next;
            else gen->objects = obj->next;
            if (obj->next) obj->next->prev = obj->prev;
            
            freed += obj->size;
            gen->allocated_size -= obj->size;
            
            void* ptr = get_object(obj);
            if (GET_TYPE(ptr) == TYPE_ENV) {
                struct lisp_env* env = (struct lisp_env*)ptr;
                if (env->table) ht_destroy(env->table);
            }
            
            kfree(obj);
        } else {
            /* Keep & Unmark */
            obj->flags &= ~GC_MARK_BIT;
            
            /* Promote */
            if (gen == &gc_ctx->young_gen && !(obj->flags & GC_OLD_GEN_BIT)) {
                obj->flags |= GC_OLD_GEN_BIT;
                
                /* Unlink from young */
                if (obj->prev) obj->prev->next = obj->next;
                else gen->objects = obj->next;
                if (obj->next) obj->next->prev = obj->prev;
                
                gen->allocated_size -= obj->size;
                
                /* Link to old */
                obj->prev = NULL;
                obj->next = gc_ctx->old_gen.objects;
                if (gc_ctx->old_gen.objects) gc_ctx->old_gen.objects->prev = obj;
                gc_ctx->old_gen.objects = obj;
                gc_ctx->old_gen.allocated_size += obj->size;
            }
        }
        obj = next;
    }
    return freed;
}

/* ========== Collection ========== */

static void collect_young(void) {
    if (!gc_ctx) return;
    gc_ctx->collecting = true;

    mark_roots();
    scan_remembered_set();
    size_t freed = sweep_generation(&gc_ctx->young_gen);
    remembered_set_clear();

    gc_ctx->young_gen.collection_count++;
    gc_ctx->stats.total_freed += freed;
    gc_ctx->stats.collection_count++;
    gc_ctx->collecting = false;
}

static void collect_old(void) {
    if (!gc_ctx) return;
    gc_ctx->collecting = true;
    
    mark_roots();
    size_t freed = sweep_generation(&gc_ctx->old_gen);
    
    gc_ctx->old_gen.collection_count++;
    gc_ctx->stats.total_freed += freed;
    gc_ctx->collecting = false;
}

/* ========== Initialization ========== */

int gc_init(void) {
    if (gc_ctx) return 0;

    gc_ctx = (struct gc_context*)kmalloc(sizeof(struct gc_context));
    if (!gc_ctx) return -1;

    memset(gc_ctx, 0, sizeof(struct gc_context));
    gc_ctx->collection_threshold = 1024 * 1024;
    gc_ctx->card_table = (uint8_t*)kmalloc(CARD_TABLE_SIZE);
    if (!gc_ctx->card_table) {
        kfree(gc_ctx);
        gc_ctx = NULL;
        return -1;
    }
    memset(gc_ctx->card_table, CARD_CLEAN, CARD_TABLE_SIZE);
    gc_ctx->write_barrier_enabled = true;

    return 0;
}

void* gc_alloc(size_t size) {
    if (!gc_ctx || size == 0) return NULL;
    
    size_t total_size = size + sizeof(struct gc_object_header);
    struct gc_object_header* header = (struct gc_object_header*)kmalloc(total_size);
    
    if (!header) {
        collect_young();
        header = (struct gc_object_header*)kmalloc(total_size);
        if (!header) {
            collect_old();
            header = (struct gc_object_header*)kmalloc(total_size);
            if (!header) return NULL;
        }
    }
    
    memset(header, 0, sizeof(struct gc_object_header));
    header->size = total_size;
    
    header->next = gc_ctx->young_gen.objects;
    if (gc_ctx->young_gen.objects) gc_ctx->young_gen.objects->prev = header;
    gc_ctx->young_gen.objects = header;
    gc_ctx->young_gen.allocated_size += total_size;
    gc_ctx->young_gen.total_size += total_size;
    gc_ctx->stats.total_allocated += total_size;
    
    if (gc_ctx->young_gen.allocated_size > gc_ctx->collection_threshold) {
        collect_young();
    }
    
    return get_object(header);
}

void gc_mark(lisp_value obj) {
    mark_object(obj);
}

void gc_collect(void) {
    collect_young();
    if (gc_ctx->old_gen.collection_count % 10 == 0) collect_old();
}

int gc_add_root(lisp_value* pointer) {
    if (!gc_ctx || !pointer) return -1;
    struct gc_root* root = (struct gc_root*)kmalloc(sizeof(struct gc_root));
    if (!root) return -1;
    root->pointer = pointer;
    root->next = gc_ctx->roots;
    gc_ctx->roots = root;
    return 0;
}

void gc_remove_root(lisp_value* pointer) {
    if (!gc_ctx || !pointer) return;
    struct gc_root* root = gc_ctx->roots;
    struct gc_root* prev = NULL;
    while (root) {
        if (root->pointer == pointer) {
            if (prev) prev->next = root->next;
            else gc_ctx->roots = root->next;
            kfree(root);
            return;
        }
        prev = root;
        root = root->next;
    }
}

void gc_register_symbol_table(struct hash_table* table) {
    if (gc_ctx) {
        gc_ctx->symbol_table = table;
    }
}

void gc_get_stats(struct gc_stats* stats_out) {
    if (gc_ctx && stats_out) *stats_out = gc_ctx->stats;
}

void gc_dump_heap(void) {
    if (!gc_ctx) return;
    FILE* f = fopen("heap.log", "a");
    if (!f) return;
    
    fprintf(f, "GC Heap Dump (Young Gen):\n");
    struct gc_object_header* obj = gc_ctx->young_gen.objects;
    while (obj) {
        void* ptr = get_object(obj);
        fprintf(f, "Obj %p Header %p Size %u Flags %x Type %d\n", ptr, obj, obj->size, obj->flags, GET_TYPE(ptr));
        obj = obj->next;
    }
    fprintf(f, "GC Heap Dump (Old Gen):\n");
    obj = gc_ctx->old_gen.objects;
    while (obj) {
        void* ptr = get_object(obj);
        fprintf(f, "Obj %p Header %p Size %u Flags %x Type %d\n", ptr, obj, obj->size, obj->flags, GET_TYPE(ptr));
        obj = obj->next;
    }
    fclose(f);
}
