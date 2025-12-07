/* AstraLisp OS Physical Memory Manager Implementation - Production Grade */

#include "pmm.h"
#include "../multiboot2.h"
#include "../sync/spinlock.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* Linker symbols */
extern char kernel_start[];
extern char kernel_end[];

#define PAGE_SIZE 4096
#define FRAMES_PER_BYTE 8
#define FRAME_SIZE PAGE_SIZE

/* Bitmap for tracking free frames */
static uint8_t* frame_bitmap = NULL;
static size_t frame_bitmap_size = 0;
static size_t total_frames = 0;
static size_t used_frames = 0;
static size_t reserved_frames = 0;
static uintptr_t highest_address = 0;

static spinlock_t pmm_lock;

/* Memory regions */
#define MAX_MEMORY_REGIONS 64
struct memory_region {
    uint64_t base;
    uint64_t length;
    uint32_t type;
};
static struct memory_region regions[MAX_MEMORY_REGIONS];
static int region_count = 0;

/* Helper to align up */
static inline uintptr_t align_up(uintptr_t val, uintptr_t align) {
    return (val + align - 1) & ~(align - 1);
}

/* Helper to align down */
static inline uintptr_t align_down(uintptr_t val, uintptr_t align) {
    return val & ~(align - 1);
}

/* Set frame bit (mark as used) */
static inline void set_frame(uintptr_t frame_idx) {
    size_t idx = frame_idx / FRAMES_PER_BYTE;
    uint8_t off = frame_idx % FRAMES_PER_BYTE;
    frame_bitmap[idx] |= (1 << off);
}

/* Clear frame bit (mark as free) */
static inline void clear_frame(uintptr_t frame_idx) {
    size_t idx = frame_idx / FRAMES_PER_BYTE;
    uint8_t off = frame_idx % FRAMES_PER_BYTE;
    frame_bitmap[idx] &= ~(1 << off);
}

/* Test frame bit */
static inline bool test_frame(uintptr_t frame_idx) {
    size_t idx = frame_idx / FRAMES_PER_BYTE;
    uint8_t off = frame_idx % FRAMES_PER_BYTE;
    return (frame_bitmap[idx] & (1 << off)) != 0;
}

/* Initialize physical memory manager */
int pmm_init(void* multiboot_info_ptr) {
    spinlock_init(&pmm_lock);
    
    struct multiboot_info* mbi = (struct multiboot_info*)multiboot_info_ptr;
    if (!mbi) return -1;

    struct multiboot_tag* tag;
    for (tag = (struct multiboot_tag*)(mbi->tags);
         tag->type != MULTIBOOT_TAG_TYPE_END;
         tag = (struct multiboot_tag*)((uint8_t*)tag + ((tag->size + 7) & ~7))) {
        
        if (tag->type == MULTIBOOT_TAG_TYPE_MMAP) {
            struct multiboot_tag_mmap* mmap = (struct multiboot_tag_mmap*)tag;
            struct multiboot_mmap_entry* entry;
            
            for (entry = mmap->entries;
                 (uint8_t*)entry < (uint8_t*)mmap + mmap->size;
                 entry = (struct multiboot_mmap_entry*)((uint8_t*)entry + mmap->entry_size)) {
                
                if (region_count < MAX_MEMORY_REGIONS) {
                    regions[region_count].base = entry->addr;
                    regions[region_count].length = entry->len;
                    regions[region_count].type = entry->type;
                    region_count++;
                    
                    if (entry->type == MULTIBOOT_MEMORY_AVAILABLE) {
                        uint64_t end = entry->addr + entry->len;
                        if (end > highest_address) {
                            highest_address = (uintptr_t)end;
                        }
                    }
                }
            }
        }
    }

    if (highest_address == 0) return -1;

    total_frames = highest_address / PAGE_SIZE;
    frame_bitmap_size = align_up(total_frames / FRAMES_PER_BYTE, PAGE_SIZE);

    /* Allocate Bitmap */
    uintptr_t bitmap_phys = 0;
    uintptr_t k_start = (uintptr_t)kernel_start;
    uintptr_t k_end = (uintptr_t)kernel_end;
    
    /* Ensure kernel end is aligned */
    if (k_end == 0) k_end = 0x2000000; /* Fallback if symbols missing */
    k_end = align_up(k_end, PAGE_SIZE);

    for (int i = 0; i < region_count; i++) {
        if (regions[i].type == MULTIBOOT_MEMORY_AVAILABLE) {
            /* Check if region is large enough */
            if (regions[i].length >= frame_bitmap_size) {
                uintptr_t r_start = regions[i].base;
                uintptr_t r_end = regions[i].base + regions[i].length;
                
                /* Avoid kernel overlap */
                if (r_start < k_end && r_end > k_start) {
                    /* Overlap. Try to place after kernel. */
                    if (r_end > k_end + frame_bitmap_size) {
                        bitmap_phys = k_end;
                        break;
                    }
                } else {
                    bitmap_phys = r_start;
                    break;
                }
            }
        }
    }

    if (bitmap_phys == 0) return -1;

    frame_bitmap = (uint8_t*)P2V(bitmap_phys);
    
    /* init logic */
    memset(frame_bitmap, 0xFF, frame_bitmap_size); /* Mark all used */
    used_frames = total_frames;
    
    /* Free available regions */
    for (int i = 0; i < region_count; i++) {
        if (regions[i].type == MULTIBOOT_MEMORY_AVAILABLE) {
            uintptr_t start = align_up(regions[i].base, PAGE_SIZE) / PAGE_SIZE;
            uintptr_t end = align_down(regions[i].base + regions[i].length, PAGE_SIZE) / PAGE_SIZE;
            for (uintptr_t f = start; f < end; f++) {
                if (f < total_frames) { /* Boundary check */
                    clear_frame(f);
                    used_frames--;
                }
            }
        }
    }
    
    /* Mark bitmap as used */
    uintptr_t b_start = bitmap_phys / PAGE_SIZE;
    uintptr_t b_end = (bitmap_phys + frame_bitmap_size + PAGE_SIZE - 1) / PAGE_SIZE;
    for (uintptr_t f = b_start; f < b_end; f++) {
        if (!test_frame(f)) {
             set_frame(f);
             used_frames++;
        }
    }
    
    /* Mark kernel as used */
    uintptr_t kern_start_f = k_start / PAGE_SIZE;
    uintptr_t kern_end_f = (k_end + PAGE_SIZE - 1) / PAGE_SIZE;
    
    for (uintptr_t f = kern_start_f; f < kern_end_f; f++) {
        if (!test_frame(f)) {
             set_frame(f);
             used_frames++;
        }
    }
    
    return 0;
}

void* pmm_alloc(void) {
    spinlock_acquire(&pmm_lock);
    static size_t last_search = 0;
    
    for (size_t i = last_search; i < total_frames; i++) {
        if (!test_frame(i)) {
            set_frame(i);
            used_frames++;
            last_search = i + 1;
            spinlock_release(&pmm_lock);
            return (void*)(i * PAGE_SIZE);
        }
    }
    
    if (last_search > 0) {
        for (size_t i = 0; i < last_search; i++) {
            if (!test_frame(i)) {
                set_frame(i);
                used_frames++;
                last_search = i + 1;
                spinlock_release(&pmm_lock);
                return (void*)(i * PAGE_SIZE);
            }
        }
    }
    
    spinlock_release(&pmm_lock);
    return NULL;
}

void pmm_free(void* frame) {
    if (!frame) return;
    spinlock_acquire(&pmm_lock);
    uintptr_t f = (uintptr_t)frame / PAGE_SIZE;
    if (f < total_frames && test_frame(f)) {
        clear_frame(f);
        used_frames--;
    }
    spinlock_release(&pmm_lock);
}

void* pmm_alloc_multiple(size_t pages) {
    if (pages == 0) return NULL;
    if (pages == 1) return pmm_alloc();
    
    spinlock_acquire(&pmm_lock);
    
    for (size_t i = 0; i < total_frames - pages; i++) {
        if (!test_frame(i)) {
            bool found = true;
            for (size_t j = 1; j < pages; j++) {
                if (test_frame(i + j)) {
                    found = false;
                    i += j;
                    break;
                }
            }
            if (found) {
                for (size_t j = 0; j < pages; j++) {
                    set_frame(i + j);
                    used_frames++;
                }
                spinlock_release(&pmm_lock);
                return (void*)(i * PAGE_SIZE);
            }
        }
    }
    spinlock_release(&pmm_lock);
    return NULL;
}

size_t pmm_get_free_count(void) {
    return total_frames - used_frames;
}

size_t pmm_get_used_count(void) {
    return used_frames;
}
