/* AstraLisp OS Kernel Heap Implementation (Buddy Allocator) */

#include "heap.h"
#include "pmm.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define HEAP_START 0x2000000
#define HEAP_SIZE 0x1000000  /* 16MB heap */
#define MIN_BLOCK_SIZE 16
#define MAX_ORDER 20

/* Maximum single allocation size (prevents integer overflow attacks) */
#define MAX_ALLOC_SIZE (HEAP_SIZE / 2)

/* Magic numbers for heap corruption detection */
#define HEAP_MAGIC_ALLOCATED 0xA110CA7E
#define HEAP_MAGIC_FREE 0xDEADBEEF

/* Block header with corruption detection */
struct block_header {
    uint32_t magic;       /* Magic number for corruption detection */
    uint32_t size:31;     /* Block size */
    uint32_t free:1;      /* Free flag */
    struct block_header* next;
    struct block_header* prev;
    uint32_t checksum;    /* Simple checksum for additional validation */
};

static uint8_t* heap_start = NULL;
static size_t heap_used = 0;
static bool buddy_initialized = false;

/* Forward declarations */
void* buddy_alloc(size_t size);
void buddy_free(void* ptr, size_t size);
int buddy_init(void* start, size_t size, uint32_t min_order, uint32_t max_order);

/* Calculate checksum for block header (simple XOR checksum) */
static inline uint32_t heap_calc_checksum(struct block_header* block) {
    return (uint32_t)((uintptr_t)block ^ block->size ^ block->magic);
}

/* Validate block header integrity */
static inline bool heap_validate_block(struct block_header* block) {
    if (!block) return false;

    /* Check magic number */
    if (block->free) {
        if (block->magic != HEAP_MAGIC_FREE) return false;
    } else {
        if (block->magic != HEAP_MAGIC_ALLOCATED) return false;
    }

    /* Validate checksum */
    if (block->checksum != heap_calc_checksum(block)) return false;

    /* Check block is within heap bounds */
    uintptr_t block_addr = (uintptr_t)block;
    uintptr_t heap_end = (uintptr_t)heap_start + HEAP_SIZE;
    if (block_addr < (uintptr_t)heap_start || block_addr >= heap_end) return false;

    /* Check size is reasonable */
    if (block->size > HEAP_SIZE) return false;

    return true;
}

/* Update block header after modification */
static inline void heap_update_block(struct block_header* block) {
    block->magic = block->free ? HEAP_MAGIC_FREE : HEAP_MAGIC_ALLOCATED;
    block->checksum = heap_calc_checksum(block);
}

/* Check for integer overflow in addition */
static inline bool would_overflow_add(size_t a, size_t b) {
    return (a + b) < a;
}

/* Initialize kernel heap */
int heap_init(void) {
    /* Get a large chunk of memory from PMM for the heap */
    /* For Phase 1, we'll ask for 16MB */
    size_t heap_size = 16 * 1024 * 1024;
    void* heap_mem_phys = pmm_alloc_multiple(heap_size / 4096);
    
    if (!heap_mem_phys) {
        return -1;
    }
    
    /* Convert to virtual address for access */
    void* heap_mem_virt = P2V((uintptr_t)heap_mem_phys);
    
    /* Initialize Buddy Allocator with this memory */
    /* Min order 4 (16 bytes), Max order 24 (16MB) */
    if (buddy_init(heap_mem_virt, heap_size, 4, 24) != 0) {
        return -1;
    }
    
    return 0;
}

struct alloc_header {
    size_t size;
    uint32_t magic;
};
#define ALLOC_MAGIC 0xB00B1E5

/* Allocate memory using appropriate allocator */
void* kmalloc(size_t size) {
    if (size == 0) return NULL;

    if (!buddy_initialized) return NULL;

    size_t total_size = size + sizeof(struct alloc_header);
    
    /* Allocate from buddy */
    void* ptr = buddy_alloc(total_size);
    if (!ptr) return NULL;
    
    /* Setup header */
    struct alloc_header* header = (struct alloc_header*)ptr;
    header->size = total_size;
    header->magic = ALLOC_MAGIC;
    
    return (void*)(header + 1);
}

/* Free memory */
void kfree(void* ptr) {
    if (!ptr) return;
    
    struct alloc_header* header = (struct alloc_header*)ptr - 1;
    
    if (header->magic != ALLOC_MAGIC) {
        /* Corruption or invalid pointer */
        return;
    }
    
    /* Invalidate magic to detect double-free */
    header->magic = 0;
    
    buddy_free((void*)header, header->size);
}

/* Reallocate memory with validation */
void* krealloc(void* ptr, size_t size) {
    /* If ptr is NULL, act like kmalloc */
    if (!ptr) {
        return kmalloc(size);
    }

    /* If size is 0, act like kfree */
    if (size == 0) {
        kfree(ptr);
        return NULL;
    }

    /* Validate pointer */
    uintptr_t ptr_addr = (uintptr_t)ptr;
    uintptr_t heap_end = (uintptr_t)heap_start + HEAP_SIZE;

    if (ptr_addr < (uintptr_t)heap_start + sizeof(struct block_header) ||
        ptr_addr >= heap_end) {
        return NULL;  /* Invalid pointer */
    }

    /* Get block header */
    struct block_header* block = (struct block_header*)((uint8_t*)ptr - sizeof(struct block_header));

    /* Validate block */
    if (!heap_validate_block(block) || block->free) {
        return NULL;  /* Corrupted or already freed */
    }

    /* Check if current block is large enough */
    size_t current_data_size = block->size - sizeof(struct block_header);

    if (current_data_size >= size) {
        return ptr;  /* Already large enough, no need to reallocate */
    }

    /* Need to allocate new block */
    void* new_ptr = kmalloc(size);
    if (!new_ptr) {
        return NULL;  /* Allocation failed, original pointer still valid */
    }

    /* Copy old data to new block */
    memcpy(new_ptr, ptr, current_data_size);

    /* Free old block */
    kfree(ptr);

    return new_ptr;
}

/* Allocate aligned memory with overflow protection */
void* kmalloc_aligned(size_t size, size_t alignment) {
    /* Validate inputs */
    if (size == 0 || alignment == 0) {
        return NULL;
    }

    /* Alignment must be power of 2 */
    if ((alignment & (alignment - 1)) != 0) {
        return NULL;  /* Not a power of 2 */
    }

    /* Check for overflow in total_size calculation */
    if (would_overflow_add(size, alignment) ||
        would_overflow_add(size + alignment, sizeof(struct block_header))) {
        return NULL;  /* Integer overflow */
    }

    size_t total_size = size + alignment + sizeof(struct block_header);

    /* Allocate memory */
    void* ptr = kmalloc(total_size);
    if (!ptr) {
        return NULL;
    }

    /* Calculate aligned address */
    uintptr_t addr = (uintptr_t)ptr;
    uintptr_t aligned_addr = (addr + alignment - 1) & ~(alignment - 1);

    /* If already aligned, return as-is */
    if (aligned_addr == addr) {
        return ptr;
    }

    /* Ensure there's space to store original pointer */
    if (aligned_addr < addr + sizeof(void*)) {
        aligned_addr += alignment;
    }

    /* Store original pointer before aligned address for later freeing */
    *((void**)(aligned_addr - sizeof(void*))) = ptr;

    return (void*)aligned_addr;
}

/*
 * ============================================================================
 * BUDDY ALLOCATOR IMPLEMENTATION
 * ============================================================================
 */

#define BUDDY_MAX_ORDER 30
#define BUDDY_MIN_ORDER 4   /* Minimum block size = 2^4 = 16 bytes */

/* Free block node in buddy system */
struct buddy_free_block {
    struct buddy_free_block* next;  /* Next free block in this order */
    struct buddy_free_block* prev;  /* Previous free block in this order */
};

/* Buddy allocator state */
struct buddy_allocator {
    void* memory_start;                     /* Start of managed memory */
    size_t memory_size;                     /* Total size of managed memory */
    uint32_t min_order;                     /* Minimum block order */
    uint32_t max_order;                     /* Maximum block order */
    struct buddy_free_block* free_lists[BUDDY_MAX_ORDER + 1];  /* Free lists per order */
    uint8_t* bitmap;                        /* Allocation bitmap */
    size_t bitmap_size;                     /* Size of bitmap in bytes */
};

static struct buddy_allocator buddy_alloc_state;
/* buddy_initialized is defined at the top of the file */

/* Calculate order (log2) for a size */
static uint32_t buddy_size_to_order(size_t size) {
    if (size == 0) return 0;

    uint32_t order = 0;
    size_t block_size = 1;

    while (block_size < size) {
        block_size <<= 1;
        order++;
    }

    return order;
}

/* Get buddy address for a block */
static void* buddy_get_buddy(void* block, uint32_t order) {
    uintptr_t block_addr = (uintptr_t)block;
    uintptr_t start_addr = (uintptr_t)buddy_alloc_state.memory_start;
    uintptr_t offset = block_addr - start_addr;
    uintptr_t buddy_offset = offset ^ (1UL << order);
    return (void*)(start_addr + buddy_offset);
}

/* Check if block is allocated in bitmap */
static bool buddy_is_allocated(void* block, uint32_t order) {
    uintptr_t block_addr = (uintptr_t)block;
    uintptr_t start_addr = (uintptr_t)buddy_alloc_state.memory_start;
    uintptr_t offset = block_addr - start_addr;
    size_t bit_index = offset >> order;

    if (bit_index / 8 >= buddy_alloc_state.bitmap_size) {
        return true;  /* Out of bounds - treat as allocated */
    }

    return (buddy_alloc_state.bitmap[bit_index / 8] & (1 << (bit_index % 8))) != 0;
}

/* Mark block as allocated in bitmap */
static void buddy_mark_allocated(void* block, uint32_t order) {
    uintptr_t block_addr = (uintptr_t)block;
    uintptr_t start_addr = (uintptr_t)buddy_alloc_state.memory_start;
    uintptr_t offset = block_addr - start_addr;
    size_t bit_index = offset >> order;

    if (bit_index / 8 < buddy_alloc_state.bitmap_size) {
        buddy_alloc_state.bitmap[bit_index / 8] |= (1 << (bit_index % 8));
    }
}

/* Mark block as free in bitmap */
static void buddy_mark_free(void* block, uint32_t order) {
    uintptr_t block_addr = (uintptr_t)block;
    uintptr_t start_addr = (uintptr_t)buddy_alloc_state.memory_start;
    uintptr_t offset = block_addr - start_addr;
    size_t bit_index = offset >> order;

    if (bit_index / 8 < buddy_alloc_state.bitmap_size) {
        buddy_alloc_state.bitmap[bit_index / 8] &= ~(1 << (bit_index % 8));
    }
}

/* Remove block from free list */
static void buddy_remove_from_free_list(struct buddy_free_block* block, uint32_t order) {
    if (!block || order > BUDDY_MAX_ORDER) {
        return;
    }

    if (block->prev) {
        block->prev->next = block->next;
    } else {
        buddy_alloc_state.free_lists[order] = block->next;
    }

    if (block->next) {
        block->next->prev = block->prev;
    }

    block->next = NULL;
    block->prev = NULL;
}

/* Add block to free list */
static void buddy_add_to_free_list(void* ptr, uint32_t order) {
    if (!ptr || order > BUDDY_MAX_ORDER) {
        return;
    }

    struct buddy_free_block* block = (struct buddy_free_block*)ptr;
    block->next = buddy_alloc_state.free_lists[order];
    block->prev = NULL;

    if (buddy_alloc_state.free_lists[order]) {
        buddy_alloc_state.free_lists[order]->prev = block;
    }

    buddy_alloc_state.free_lists[order] = block;
}

/* Initialize buddy allocator */
int buddy_init(void* start, size_t size, uint32_t min_order, uint32_t max_order) {
    if (!start || size == 0 || min_order > max_order || max_order > BUDDY_MAX_ORDER) {
        return -1;
    }

    /* Calculate bitmap size */
    size_t max_blocks = size >> min_order;
    size_t bitmap_size = (max_blocks + 7) / 8;
    
    /* Align bitmap size to next block boundary (min_order) to keep alignment */
    size_t align_mask = (1 << min_order) - 1;
    bitmap_size = (bitmap_size + align_mask) & ~align_mask;

    if (bitmap_size >= size) {
        return -1; /* Not enough memory for bitmap */
    }

    /* Carve bitmap from the start of memory */
    buddy_alloc_state.bitmap = (uint8_t*)start;
    buddy_alloc_state.bitmap_size = bitmap_size;
    
    /* Clear bitmap */
    memset(buddy_alloc_state.bitmap, 0, buddy_alloc_state.bitmap_size);

    /* Adjust managed memory to exclude bitmap */
    buddy_alloc_state.memory_start = (void*)((uintptr_t)start + bitmap_size);
    buddy_alloc_state.memory_size = size - bitmap_size;
    buddy_alloc_state.min_order = min_order;
    buddy_alloc_state.max_order = max_order;

    /* Initialize free lists */
    for (uint32_t i = 0; i <= BUDDY_MAX_ORDER; i++) {
        buddy_alloc_state.free_lists[i] = NULL;
    }

    /* Add initial block(s) to free lists */
    /* Since size might not be power of 2, we need to add multiple blocks */
    /* We add the largest power of 2 blocks that fit until memory is covered */
    
    /* Find largest order that fits in remaining size */
    size_t remaining_size = buddy_alloc_state.memory_size;
    uintptr_t current_addr = (uintptr_t)buddy_alloc_state.memory_start;
    
    while (remaining_size >= (1UL << min_order)) {
        uint32_t order = max_order;
        while ((1UL << order) > remaining_size) {
            order--;
        }
        
        /* Add block to free list */
        buddy_add_to_free_list((void*)current_addr, order);
        
        current_addr += (1UL << order);
        remaining_size -= (1UL << order);
    }

    buddy_initialized = true;
    return 0;
}

/* Allocate block of specific order */
void* buddy_alloc_order(uint32_t order) {
    if (!buddy_initialized || order > buddy_alloc_state.max_order || order < buddy_alloc_state.min_order) {
        return NULL;
    }

    /* Find smallest available block >= requested order */
    uint32_t current_order = order;
    while (current_order <= buddy_alloc_state.max_order) {
        if (buddy_alloc_state.free_lists[current_order]) {
            /* Found free block - remove from list */
            struct buddy_free_block* block = buddy_alloc_state.free_lists[current_order];
            buddy_remove_from_free_list(block, current_order);

            /* Split block down to requested order */
            while (current_order > order) {
                current_order--;
                void* buddy = (void*)((uintptr_t)block + (1UL << current_order));
                buddy_add_to_free_list(buddy, current_order);
            }

            /* Mark as allocated */
            buddy_mark_allocated(block, order);

            return (void*)block;
        }
        current_order++;
    }

    return NULL;  /* Out of memory */
}

/* Free block of specific order */
void buddy_free_order(void* ptr, uint32_t order) {
    if (!buddy_initialized || !ptr || order > buddy_alloc_state.max_order || order < buddy_alloc_state.min_order) {
        return;
    }

    /* Validate pointer is within managed memory */
    uintptr_t ptr_addr = (uintptr_t)ptr;
    uintptr_t start_addr = (uintptr_t)buddy_alloc_state.memory_start;
    uintptr_t end_addr = start_addr + buddy_alloc_state.memory_size;

    if (ptr_addr < start_addr || ptr_addr >= end_addr) {
        return;  /* Invalid pointer */
    }

    /* Mark as free */
    buddy_mark_free(ptr, order);

    /* Coalesce with buddy if possible */
    uint32_t current_order = order;
    void* current_block = ptr;

    while (current_order < buddy_alloc_state.max_order) {
        void* buddy = buddy_get_buddy(current_block, current_order);
        uintptr_t buddy_addr = (uintptr_t)buddy;

        /* Check if buddy is valid and free */
        if (buddy_addr < start_addr || buddy_addr >= end_addr) {
            break;  /* Buddy out of bounds */
        }

        if (buddy_is_allocated(buddy, current_order)) {
            break;  /* Buddy is allocated */
        }

        /* Buddy is free - remove it from free list and merge */
        buddy_remove_from_free_list((struct buddy_free_block*)buddy, current_order);

        /* Determine which block comes first (use lower address as merged block) */
        if ((uintptr_t)buddy < (uintptr_t)current_block) {
            current_block = buddy;
        }

        current_order++;
    }

    /* Add merged block to free list */
    buddy_add_to_free_list(current_block, current_order);
}

/* Allocate memory using buddy allocator */
void* buddy_alloc(size_t size) {
    if (size == 0) {
        return NULL;
    }

    uint32_t order = buddy_size_to_order(size);
    if (order < buddy_alloc_state.min_order) {
        order = buddy_alloc_state.min_order;
    }

    return buddy_alloc_order(order);
}

/* Free memory using buddy allocator */
void buddy_free(void* ptr, size_t size) {
    if (!ptr || size == 0) {
        return;
    }

    uint32_t order = buddy_size_to_order(size);
    if (order < buddy_alloc_state.min_order) {
        order = buddy_alloc_state.min_order;
    }

    buddy_free_order(ptr, order);
}

/*
 * ============================================================================
 * SLAB ALLOCATOR IMPLEMENTATION
 * ============================================================================
 */

#define SLAB_MAGIC 0x51AB51AB
#define SLAB_OBJECTS_PER_SLAB 64

/* Slab structure */
struct slab {
    uint32_t magic;                 /* Magic number for validation */
    struct slab* next;              /* Next slab in cache */
    struct slab_cache* cache;       /* Parent cache */
    uint32_t free_count;            /* Number of free objects */
    uint32_t obj_count;             /* Total objects in slab */
    void* free_list;                /* Head of free object list */
    uint8_t* objects;               /* Start of object area */
};

/* Slab cache structure */
struct slab_cache {
    char name[32];                  /* Cache name */
    size_t obj_size;                /* Size of each object */
    size_t align;                   /* Alignment requirement */
    struct slab* slabs_full;        /* Fully allocated slabs */
    struct slab* slabs_partial;     /* Partially allocated slabs */
    struct slab* slabs_free;        /* Empty slabs */
    uint32_t slab_count;            /* Total number of slabs */
    struct slab_cache* next;        /* Next cache in global list */
};

static struct slab_cache* slab_cache_list = NULL;

/* Create slab cache */
struct slab_cache* slab_cache_create(const char* name, size_t obj_size, size_t align) {
    if (!name || obj_size == 0 || align == 0) {
        return NULL;
    }

    /* Alignment must be power of 2 */
    if ((align & (align - 1)) != 0) {
        return NULL;
    }

    /* Allocate cache structure */
    struct slab_cache* cache = (struct slab_cache*)kmalloc(sizeof(struct slab_cache));
    if (!cache) {
        return NULL;
    }

    /* Initialize cache */
    memset(cache, 0, sizeof(struct slab_cache));
    strncpy(cache->name, name, sizeof(cache->name) - 1);
    cache->obj_size = obj_size;
    cache->align = align;

    /* Add to global cache list */
    cache->next = slab_cache_list;
    slab_cache_list = cache;

    return cache;
}

/* Allocate new slab for cache */
static struct slab* slab_allocate_slab(struct slab_cache* cache) {
    if (!cache) {
        return NULL;
    }

    /* Calculate slab size */
    size_t obj_size_aligned = (cache->obj_size + cache->align - 1) & ~(cache->align - 1);
    size_t slab_size = sizeof(struct slab) + (obj_size_aligned * SLAB_OBJECTS_PER_SLAB);

    /* Allocate slab using buddy allocator if available, otherwise kmalloc */
    void* slab_mem = buddy_initialized ? buddy_alloc(slab_size) : kmalloc(slab_size);
    if (!slab_mem) {
        return NULL;
    }

    struct slab* slab = (struct slab*)slab_mem;
    slab->magic = SLAB_MAGIC;
    slab->next = NULL;
    slab->cache = cache;
    slab->free_count = SLAB_OBJECTS_PER_SLAB;
    slab->obj_count = SLAB_OBJECTS_PER_SLAB;
    slab->objects = (uint8_t*)slab_mem + sizeof(struct slab);

    /* Build free list */
    slab->free_list = NULL;
    for (uint32_t i = 0; i < SLAB_OBJECTS_PER_SLAB; i++) {
        void* obj = slab->objects + (i * obj_size_aligned);
        *((void**)obj) = slab->free_list;
        slab->free_list = obj;
    }

    cache->slab_count++;
    return slab;
}

/* Allocate object from slab cache */
void* slab_alloc(struct slab_cache* cache) {
    if (!cache) {
        return NULL;
    }

    struct slab* slab = NULL;

    /* Try partial slabs first */
    if (cache->slabs_partial) {
        slab = cache->slabs_partial;
    } else if (cache->slabs_free) {
        /* Use free slab */
        slab = cache->slabs_free;
        cache->slabs_free = slab->next;
        slab->next = cache->slabs_partial;
        cache->slabs_partial = slab;
    } else {
        /* Allocate new slab */
        slab = slab_allocate_slab(cache);
        if (!slab) {
            return NULL;
        }
        slab->next = cache->slabs_partial;
        cache->slabs_partial = slab;
    }

    /* Validate slab */
    if (slab->magic != SLAB_MAGIC || !slab->free_list || slab->free_count == 0) {
        return NULL;  /* Corrupted slab */
    }

    /* Allocate object from free list */
    void* obj = slab->free_list;
    slab->free_list = *((void**)obj);
    slab->free_count--;

    /* Move to full list if completely allocated */
    if (slab->free_count == 0) {
        /* Remove from partial list */
        cache->slabs_partial = slab->next;
        /* Add to full list */
        slab->next = cache->slabs_full;
        cache->slabs_full = slab;
    }

    return obj;
}

/* Free object back to slab cache */
void slab_free(struct slab_cache* cache, void* obj) {
    if (!cache || !obj) {
        return;
    }

    /* Find which slab owns this object */
    struct slab* slab = NULL;
    struct slab* prev = NULL;
    bool found_in_partial = false;

    /* Search partial slabs */
    slab = cache->slabs_partial;
    prev = NULL;
    while (slab) {
        if (slab->magic != SLAB_MAGIC) {
            return;  /* Corrupted slab */
        }

        uintptr_t obj_addr = (uintptr_t)obj;
        uintptr_t slab_start = (uintptr_t)slab->objects;
        size_t obj_size_aligned = (cache->obj_size + cache->align - 1) & ~(cache->align - 1);
        uintptr_t slab_end = slab_start + (obj_size_aligned * slab->obj_count);

        if (obj_addr >= slab_start && obj_addr < slab_end) {
            found_in_partial = true;
            break;
        }

        prev = slab;
        slab = slab->next;
    }

    /* Search full slabs if not found in partial */
    if (!found_in_partial) {
        slab = cache->slabs_full;
        prev = NULL;
        while (slab) {
            if (slab->magic != SLAB_MAGIC) {
                return;  /* Corrupted slab */
            }

            uintptr_t obj_addr = (uintptr_t)obj;
            uintptr_t slab_start = (uintptr_t)slab->objects;
            size_t obj_size_aligned = (cache->obj_size + cache->align - 1) & ~(cache->align - 1);
            uintptr_t slab_end = slab_start + (obj_size_aligned * slab->obj_count);

            if (obj_addr >= slab_start && obj_addr < slab_end) {
                /* Remove from full list */
                if (prev) {
                    prev->next = slab->next;
                } else {
                    cache->slabs_full = slab->next;
                }

                /* Add to partial list */
                slab->next = cache->slabs_partial;
                cache->slabs_partial = slab;
                break;
            }

            prev = slab;
            slab = slab->next;
        }
    }

    if (!slab) {
        return;  /* Object not found in any slab */
    }

    /* Add object back to free list */
    *((void**)obj) = slab->free_list;
    slab->free_list = obj;
    slab->free_count++;

    /* Move to free list if completely empty */
    if (slab->free_count == slab->obj_count) {
        /* Remove from partial list */
        if (found_in_partial) {
            if (prev) {
                prev->next = slab->next;
            } else {
                cache->slabs_partial = slab->next;
            }
        }

        /* Add to free list */
        slab->next = cache->slabs_free;
        cache->slabs_free = slab;
    }
}

/* Destroy slab cache */
void slab_cache_destroy(struct slab_cache* cache) {
    if (!cache) {
        return;
    }

    /* Free all slabs */
    struct slab* slab;

    /* Free full slabs */
    slab = cache->slabs_full;
    while (slab) {
        struct slab* next = slab->next;
        if (buddy_initialized) {
            size_t obj_size_aligned = (cache->obj_size + cache->align - 1) & ~(cache->align - 1);
            size_t slab_size = sizeof(struct slab) + (obj_size_aligned * SLAB_OBJECTS_PER_SLAB);
            buddy_free(slab, slab_size);
        } else {
            kfree(slab);
        }
        slab = next;
    }

    /* Free partial slabs */
    slab = cache->slabs_partial;
    while (slab) {
        struct slab* next = slab->next;
        if (buddy_initialized) {
            size_t obj_size_aligned = (cache->obj_size + cache->align - 1) & ~(cache->align - 1);
            size_t slab_size = sizeof(struct slab) + (obj_size_aligned * SLAB_OBJECTS_PER_SLAB);
            buddy_free(slab, slab_size);
        } else {
            kfree(slab);
        }
        slab = next;
    }

    /* Free empty slabs */
    slab = cache->slabs_free;
    while (slab) {
        struct slab* next = slab->next;
        if (buddy_initialized) {
            size_t obj_size_aligned = (cache->obj_size + cache->align - 1) & ~(cache->align - 1);
            size_t slab_size = sizeof(struct slab) + (obj_size_aligned * SLAB_OBJECTS_PER_SLAB);
            buddy_free(slab, slab_size);
        } else {
            kfree(slab);
        }
        slab = next;
    }

    /* Remove from global list */
    struct slab_cache* prev = NULL;
    struct slab_cache* current = slab_cache_list;

    while (current) {
        if (current == cache) {
            if (prev) {
                prev->next = current->next;
            } else {
                slab_cache_list = current->next;
            }
            break;
        }
        prev = current;
        current = current->next;
    }

    kfree(cache);
}
