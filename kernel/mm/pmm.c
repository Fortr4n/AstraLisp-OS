/* AstraLisp OS Physical Memory Manager Implementation - Production Grade */

#include "pmm.h"
#include "../multiboot2.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define PAGE_SIZE 4096
#define FRAMES_PER_BYTE 8
#define FRAME_SIZE PAGE_SIZE

#ifdef TEST_MODE
#include <stdio.h>
#endif

/* Bitmap for tracking free frames */
static uint8_t* frame_bitmap = NULL;
static size_t frame_bitmap_size = 0;
static size_t total_frames = 0;
static size_t used_frames = 0;
static size_t reserved_frames = 0;
static uintptr_t highest_address = 0;

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

#ifdef TEST_MODE
#include <stdio.h>
#endif

/* Initialize physical memory manager */
int pmm_init(void* multiboot_info_ptr) {
    struct multiboot_info* mbi = (struct multiboot_info*)multiboot_info_ptr;
    
    if (!mbi) {
#ifdef TEST_MODE
        printf("PMM: mbi is NULL\n");
#endif
        return -1;
    }

    /* 1. Parse Multiboot tags to find memory map */
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
#ifdef TEST_MODE
                    printf("PMM: Region %d: Base %llx, Len %llx, Type %d\n", 
                           region_count, regions[region_count].base, regions[region_count].length, regions[region_count].type);
#endif
                    region_count++;
                    
                    /* Track highest address to size the bitmap */
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

    if (highest_address == 0) {
#ifdef TEST_MODE
        printf("PMM: No memory found (highest_address=0)\n");
#endif
        return -1; /* No memory found */
    }

    /* 2. Calculate bitmap size */
    total_frames = highest_address / PAGE_SIZE;
    frame_bitmap_size = align_up(total_frames / FRAMES_PER_BYTE, PAGE_SIZE);
#ifdef TEST_MODE
    printf("PMM: Total frames: %zu, Bitmap size: %zu\n", total_frames, frame_bitmap_size);
#endif

    /* 3. Find a place to put the bitmap */
    /* We need a contiguous block of free memory large enough for the bitmap */
    uintptr_t bitmap_phys = 0;
    for (int i = 0; i < region_count; i++) {
        if (regions[i].type == MULTIBOOT_MEMORY_AVAILABLE) {
            if (regions[i].length >= frame_bitmap_size) {
                /* Use the start of this region, but ensure it doesn't overlap kernel */
                /* Assuming kernel is loaded at 0x1000000 and is < 16MB for now */
                /* TODO: Get actual kernel end from linker symbol */
                uintptr_t kernel_end = 0x2000000; 
                
                uintptr_t region_start = regions[i].base;
                uintptr_t region_end = regions[i].base + regions[i].length;

                if (region_start < kernel_end) {
                    /* If region starts before kernel end, skip past kernel */
                    if (region_end > kernel_end + frame_bitmap_size) {
                         bitmap_phys = kernel_end;
                         break;
                    }
                } else {
                    bitmap_phys = region_start;
                    break;
                }
            }
        }
    }

    if (bitmap_phys == 0) {
#ifdef TEST_MODE
        printf("PMM: Could not allocate bitmap\n");
#endif
        return -1; /* Could not allocate bitmap */
    }

    frame_bitmap = (uint8_t*)P2V(bitmap_phys);
#ifdef TEST_MODE
    printf("PMM: Bitmap allocated at %p (phys %llx)\n", frame_bitmap, (uint64_t)bitmap_phys);
#endif
    
    /* 4. Initialize bitmap: Mark ALL as used first */
    memset(frame_bitmap, 0xFF, frame_bitmap_size);
    used_frames = total_frames;

    /* 5. Mark available regions as free */
    for (int i = 0; i < region_count; i++) {
        if (regions[i].type == MULTIBOOT_MEMORY_AVAILABLE) {
            uintptr_t start_frame = align_up(regions[i].base, PAGE_SIZE) / PAGE_SIZE;
            uintptr_t end_frame = align_down(regions[i].base + regions[i].length, PAGE_SIZE) / PAGE_SIZE;
            
            for (uintptr_t f = start_frame; f < end_frame; f++) {
                if (f < total_frames) {
                    clear_frame(f);
                    used_frames--;
                }
            }
        }
    }

    /* 6. Mark bitmap itself as used */
    uintptr_t bitmap_start_frame = bitmap_phys / PAGE_SIZE;
    uintptr_t bitmap_end_frame = (bitmap_phys + frame_bitmap_size + PAGE_SIZE - 1) / PAGE_SIZE;
    for (uintptr_t f = bitmap_start_frame; f < bitmap_end_frame; f++) {
        set_frame(f);
        used_frames++;
        reserved_frames++;
    }

    /* 7. Mark kernel memory as used (0x0 to 0x2000000 approx) */
    /* TODO: Use linker symbols for exact kernel range */
    uintptr_t kernel_start_frame = 0;
    uintptr_t kernel_end_frame = 0x2000000 / PAGE_SIZE;
    for (uintptr_t f = kernel_start_frame; f < kernel_end_frame; f++) {
        if (!test_frame(f)) {
            set_frame(f);
            used_frames++;
            reserved_frames++;
        }
    }

    return 0;
}

/* Allocate a page frame */
void* pmm_alloc(void) {
    /* Simple first-fit search */
    /* Optimization: maintain a 'last_search' index to avoid rescanning from 0 */
    static size_t last_search = 0;
    
    for (size_t i = last_search; i < total_frames; i++) {
        if (!test_frame(i)) {
            set_frame(i);
            used_frames++;
            last_search = i + 1;
            return (void*)(i * PAGE_SIZE);
        }
    }
    
    /* Wrap around if not found */
    if (last_search > 0) {
        for (size_t i = 0; i < last_search; i++) {
            if (!test_frame(i)) {
                set_frame(i);
                used_frames++;
                last_search = i + 1;
                return (void*)(i * PAGE_SIZE);
            }
        }
    }

    return NULL; /* Out of memory */
}

/* Free a page frame */
void pmm_free(void* frame) {
    if (!frame) return;
    
    uintptr_t addr = (uintptr_t)frame;
    uintptr_t f = addr / PAGE_SIZE;
    
    if (f >= total_frames) return;
    
    if (test_frame(f)) {
        clear_frame(f);
        used_frames--;
    }
}

/* Allocate multiple contiguous page frames */
void* pmm_alloc_multiple(size_t pages) {
    if (pages == 0) return NULL;
    if (pages == 1) return pmm_alloc();

    /* Brute force search for contiguous block */
    /* TODO: Optimize with buddy allocator or free lists */
    
    for (size_t i = 0; i < total_frames - pages; i++) {
        if (!test_frame(i)) {
            /* Found a free frame, check if next 'pages-1' are also free */
            bool found = true;
            for (size_t j = 1; j < pages; j++) {
                if (test_frame(i + j)) {
                    found = false;
                    i += j; /* Skip ahead */
                    break;
                }
            }
            
            if (found) {
                /* Mark all as used */
                for (size_t j = 0; j < pages; j++) {
                    set_frame(i + j);
                    used_frames++;
                }
                return (void*)(i * PAGE_SIZE);
            }
        }
    }
    
    return NULL;
}

/* Get number of free pages */
size_t pmm_get_free_count(void) {
    return total_frames - used_frames;
}

/* Get number of used pages */
size_t pmm_get_used_count(void) {
    return used_frames;
}

