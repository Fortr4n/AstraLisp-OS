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

/* Block header */
struct block_header {
    uint32_t size:31;
    uint32_t free:1;
    struct block_header* next;
    struct block_header* prev;
};

static uint8_t* heap_start = NULL;
static size_t heap_used = 0;

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
    
    heap_used = sizeof(struct block_header);
    
    return 0;
}

/* Allocate memory */
void* kmalloc(size_t size) {
    if (size == 0) {
        return NULL;
    }
    
    /* Add header size */
    size_t total_size = size + sizeof(struct block_header);
    
    /* Align to 8 bytes */
    total_size = (total_size + 7) & ~7;
    
    /* Find free block */
    struct block_header* current = (struct block_header*)heap_start;
    
    while (current) {
        if (current->free && current->size >= total_size) {
            /* Found suitable block */
            if (current->size >= total_size + sizeof(struct block_header) + MIN_BLOCK_SIZE) {
                /* Split block */
                struct block_header* new_block = (struct block_header*)((uint8_t*)current + total_size);
                new_block->size = current->size - total_size;
                new_block->free = 1;
                new_block->next = current->next;
                new_block->prev = current;
                
                if (current->next) {
                    current->next->prev = new_block;
                }
                
                current->size = total_size;
                current->next = new_block;
            }
            
            current->free = 0;
            heap_used += current->size;
            
            return (void*)((uint8_t*)current + sizeof(struct block_header));
        }
        
        current = current->next;
    }
    
    return NULL;  /* Out of memory */
}

/* Free memory */
void kfree(void* ptr) {
    if (!ptr) {
        return;
    }
    
    struct block_header* block = (struct block_header*)((uint8_t*)ptr - sizeof(struct block_header));
    
    if (block->free) {
        return;  /* Already free */
    }
    
    block->free = 1;
    heap_used -= block->size;
    
    /* Merge with next block if free */
    if (block->next && block->next->free) {
        block->size += block->next->size;
        block->next = block->next->next;
        if (block->next) {
            block->next->prev = block;
        }
    }
    
    /* Merge with previous block if free */
    if (block->prev && block->prev->free) {
        block->prev->size += block->size;
        block->prev->next = block->next;
        if (block->next) {
            block->next->prev = block->prev;
        }
    }
}

/* Reallocate memory */
void* krealloc(void* ptr, size_t size) {
    if (!ptr) {
        return kmalloc(size);
    }
    
    if (size == 0) {
        kfree(ptr);
        return NULL;
    }
    
    struct block_header* block = (struct block_header*)((uint8_t*)ptr - sizeof(struct block_header));
    
    if (block->size - sizeof(struct block_header) >= size) {
        return ptr;  /* Already large enough */
    }
    
    void* new_ptr = kmalloc(size);
    if (!new_ptr) {
        return NULL;
    }
    
    memcpy(new_ptr, ptr, block->size - sizeof(struct block_header));
    kfree(ptr);
    
    return new_ptr;
}

/* Allocate aligned memory */
void* kmalloc_aligned(size_t size, size_t alignment) {
    size_t total_size = size + alignment + sizeof(struct block_header);
    void* ptr = kmalloc(total_size);
    
    if (!ptr) {
        return NULL;
    }
    
    uintptr_t addr = (uintptr_t)ptr;
    uintptr_t aligned_addr = (addr + alignment - 1) & ~(alignment - 1);
    
    if (aligned_addr == addr) {
        return ptr;
    }
    
    /* Store original pointer before aligned address */
    *((void**)(aligned_addr - sizeof(void*))) = ptr;
    
    return (void*)aligned_addr;
}
