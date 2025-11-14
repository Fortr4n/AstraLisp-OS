/* AstraLisp OS Physical Memory Manager Implementation */

#include "pmm.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define PAGE_SIZE 4096
#define FRAMES_PER_BYTE 8
#define FRAME_SIZE PAGE_SIZE

/* Bitmap for tracking free frames */
static uint8_t* frame_bitmap = NULL;
static size_t frame_bitmap_size = 0;
static size_t total_frames = 0;
static size_t used_frames = 0;
static uintptr_t memory_start = 0;
static uintptr_t memory_end = 0;

/* Set frame bit */
static inline void set_frame(uintptr_t frame_addr) {
    uintptr_t frame = (frame_addr - memory_start) / FRAME_SIZE;
    size_t idx = frame / FRAMES_PER_BYTE;
    uint8_t off = frame % FRAMES_PER_BYTE;
    frame_bitmap[idx] |= (1 << off);
}

/* Clear frame bit */
static inline void clear_frame(uintptr_t frame_addr) {
    uintptr_t frame = (frame_addr - memory_start) / FRAME_SIZE;
    size_t idx = frame / FRAMES_PER_BYTE;
    uint8_t off = frame % FRAMES_PER_BYTE;
    frame_bitmap[idx] &= ~(1 << off);
}

/* Test frame bit */
static inline bool test_frame(uintptr_t frame_addr) {
    uintptr_t frame = (frame_addr - memory_start) / FRAME_SIZE;
    size_t idx = frame / FRAMES_PER_BYTE;
    uint8_t off = frame % FRAMES_PER_BYTE;
    return (frame_bitmap[idx] & (1 << off)) != 0;
}

/* Find first free frame */
static uintptr_t first_frame(void) {
    for (size_t i = 0; i < total_frames; i++) {
        size_t idx = i / FRAMES_PER_BYTE;
        uint8_t off = i % FRAMES_PER_BYTE;
        if (!(frame_bitmap[idx] & (1 << off))) {
            return memory_start + (i * FRAME_SIZE);
        }
    }
    return 0;  /* No free frame */
}

/* Initialize physical memory manager */
int pmm_init(void* multiboot_info) {
    /* For now, use a simple fixed memory region */
    /* In a real implementation, we'd parse multiboot memory map */
    
    memory_start = 0x1000000;  /* Start after kernel */
    memory_end = 0x10000000;   /* 256MB */
    
    total_frames = (memory_end - memory_start) / FRAME_SIZE;
    frame_bitmap_size = total_frames / FRAMES_PER_BYTE;
    
    /* Allocate bitmap (for now, use a static buffer) */
    /* In real implementation, allocate from early memory */
    static uint8_t bitmap_buffer[1024 * 1024];  /* 1MB bitmap */
    if (frame_bitmap_size > sizeof(bitmap_buffer)) {
        return -1;
    }
    
    frame_bitmap = bitmap_buffer;
    memset(frame_bitmap, 0, frame_bitmap_size);
    
    /* Mark kernel memory as used */
    /* This is simplified - real implementation would mark all used regions */
    
    used_frames = 0;
    
    return 0;
}

/* Allocate a page frame */
void* pmm_alloc(void) {
    uintptr_t frame = first_frame();
    if (frame == 0) {
        return NULL;  /* Out of memory */
    }
    
    set_frame(frame);
    used_frames++;
    
    return (void*)frame;
}

/* Free a page frame */
void pmm_free(void* frame) {
    if (frame == NULL) {
        return;
    }
    
    uintptr_t frame_addr = (uintptr_t)frame;
    
    if (frame_addr < memory_start || frame_addr >= memory_end) {
        return;  /* Invalid frame */
    }
    
    if (!test_frame(frame_addr)) {
        return;  /* Already free */
    }
    
    clear_frame(frame_addr);
    used_frames--;
}

/* Get number of free pages */
size_t pmm_get_free_count(void) {
    return total_frames - used_frames;
}

/* Get number of used pages */
size_t pmm_get_used_count(void) {
    return used_frames;
}
