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
    /* Allocate heap from PMM */
    heap_start = (uint8_t*)pmm_alloc();
    if (!heap_start) {
        return -1;
    }

    /* Initialize first block */
    struct block_header* first = (struct block_header*)heap_start;
    first->size = HEAP_SIZE - sizeof(struct block_header);
    first->free = 1;
    first->next = NULL;
    first->prev = NULL;
    heap_update_block(first);  /* Set magic and checksum */

    heap_used = sizeof(struct block_header);

    return 0;
}

/* Allocate memory with comprehensive overflow and corruption protection */
void* kmalloc(size_t size) {
    /* Validate input size */
    if (size == 0) {
        return NULL;
    }

    /* Check for maximum allocation size (prevents overflow attacks) */
    if (size > MAX_ALLOC_SIZE) {
        return NULL;  /* Allocation too large */
    }

    /* Check for integer overflow when adding header size */
    if (would_overflow_add(size, sizeof(struct block_header))) {
        return NULL;  /* Integer overflow detected */
    }

    size_t total_size = size + sizeof(struct block_header);

    /* Check for overflow when aligning */
    if (would_overflow_add(total_size, 7)) {
        return NULL;  /* Integer overflow in alignment */
    }

    /* Align to 8 bytes */
    total_size = (total_size + 7) & ~7UL;

    /* Ensure total size doesn't exceed heap size */
    if (total_size > HEAP_SIZE) {
        return NULL;  /* Allocation exceeds heap size */
    }

    /* Find free block using first-fit strategy */
    struct block_header* current = (struct block_header*)heap_start;

    while (current) {
        /* Validate block integrity before using */
        if (!heap_validate_block(current)) {
            /* Heap corruption detected! */
            return NULL;
        }

        if (current->free && current->size >= total_size) {
            /* Found suitable block */

            /* Check if we should split this block */
            size_t split_threshold = total_size + sizeof(struct block_header) + MIN_BLOCK_SIZE;

            /* Check for overflow in split calculation */
            if (!would_overflow_add(total_size, sizeof(struct block_header)) &&
                !would_overflow_add(total_size + sizeof(struct block_header), MIN_BLOCK_SIZE)) {

                if (current->size >= split_threshold) {
                    /* Split block - create new free block after allocated one */
                    struct block_header* new_block = (struct block_header*)((uint8_t*)current + total_size);

                    /* Validate new block address is within heap */
                    uintptr_t new_block_addr = (uintptr_t)new_block;
                    uintptr_t heap_end = (uintptr_t)heap_start + HEAP_SIZE;

                    if (new_block_addr + sizeof(struct block_header) <= heap_end) {
                        new_block->size = current->size - total_size;
                        new_block->free = 1;
                        new_block->next = current->next;
                        new_block->prev = current;
                        heap_update_block(new_block);

                        if (current->next) {
                            current->next->prev = new_block;
                        }

                        current->size = total_size;
                        current->next = new_block;
                    }
                }
            }

            /* Mark block as allocated */
            current->free = 0;
            heap_update_block(current);  /* Update magic and checksum */

            /* Update heap usage statistics */
            heap_used += current->size;

            /* Return pointer to data (skip header) */
            return (void*)((uint8_t*)current + sizeof(struct block_header));
        }

        current = current->next;
    }

    return NULL;  /* Out of memory - no suitable block found */
}

/* Free memory with double-free protection and validation */
void kfree(void* ptr) {
    if (!ptr) {
        return;
    }

    /* Validate pointer is within heap bounds */
    uintptr_t ptr_addr = (uintptr_t)ptr;
    uintptr_t heap_end = (uintptr_t)heap_start + HEAP_SIZE;

    if (ptr_addr < (uintptr_t)heap_start + sizeof(struct block_header) ||
        ptr_addr >= heap_end) {
        /* Invalid pointer - not in heap! */
        return;
    }

    /* Get block header */
    struct block_header* block = (struct block_header*)((uint8_t*)ptr - sizeof(struct block_header));

    /* Validate block integrity */
    if (!heap_validate_block(block)) {
        /* Heap corruption or invalid block! */
        return;
    }

    /* Check for double-free */
    if (block->free) {
        /* Double-free detected! This is a serious bug. */
        return;
    }

    /* Mark block as free */
    block->free = 1;
    heap_used -= block->size;
    heap_update_block(block);  /* Update magic and checksum */

    /* Merge with next block if it's free (coalescing) */
    if (block->next && heap_validate_block(block->next) && block->next->free) {
        block->size += block->next->size;
        block->next = block->next->next;
        if (block->next) {
            block->next->prev = block;
        }
        heap_update_block(block);
    }

    /* Merge with previous block if it's free (coalescing) */
    if (block->prev && heap_validate_block(block->prev) && block->prev->free) {
        block->prev->size += block->size;
        block->prev->next = block->next;
        if (block->next) {
            block->next->prev = block->prev;
        }
        heap_update_block(block->prev);
    }
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
