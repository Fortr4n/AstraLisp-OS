/* AstraLisp OS Virtual Memory Manager Implementation */

#include "vmm.h"
#include "pmm.h"
#include "hal/mmu.h"
#include <stddef.h>
#include <string.h>

static void* current_pagedir = NULL;

/* Initialize virtual memory manager */
int vmm_init(void) {
    /* Create kernel page directory */
    current_pagedir = vmm_create_pagedir();
    if (!current_pagedir) {
        return -1;
    }
    
    /* Identity map first 4MB for kernel */
    for (uintptr_t addr = 0; addr < 0x400000; addr += PAGE_SIZE) {
        vmm_map_page(current_pagedir, addr, addr, PAGE_PRESENT | PAGE_WRITABLE);
    }
    
    /* Switch to kernel page directory */
    vmm_switch_pagedir(current_pagedir);
    
    return 0;
}

/* Create a new page directory */
void* vmm_create_pagedir(void) {
    /* Allocate page for page directory */
    void* pagedir = pmm_alloc();
    if (!pagedir) {
        return NULL;
    }
    
    /* Clear page directory */
    memset(pagedir, 0, PAGE_SIZE);
    
    return pagedir;
}

/* Destroy a page directory */
void vmm_destroy_pagedir(void* pagedir) {
    if (pagedir && pagedir != current_pagedir) {
        /* Unmap all pages and free page directory */
        pmm_free(pagedir);
    }
}

/* Map a page */
int vmm_map_page(void* pagedir, uintptr_t virt, uintptr_t phys, uint32_t flags) {
    /* Use MMU functions */
    return map_page(virt, phys, flags);
}

/* Unmap a page */
int vmm_unmap_page(void* pagedir, uintptr_t virt) {
    return unmap_page(virt);
}

/* Switch page directory */
void vmm_switch_pagedir(void* pagedir) {
    current_pagedir = pagedir;
    /* In PowerISA, we'd set the page table base register */
    /* For now, this is a placeholder */
    invalidate_tlb();
}

/* Get current page directory */
void* vmm_get_current_pagedir(void) {
    return current_pagedir;
}
