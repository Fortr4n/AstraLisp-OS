/* AstraLisp OS Virtual Memory Manager */

#ifndef VMM_H
#define VMM_H

#include <stdint.h>
#include <stdbool.h>

/* Page flags (PowerISA-specific) */
#define PAGE_PRESENT        0x00000001
#define PAGE_WRITABLE       0x00000002
#define PAGE_USER           0x00000004
#define PAGE_LARGE          0x00000008  /* 64KB page instead of 4KB */
#define PAGE_CACHE_DISABLE  0x00000010
#define PAGE_NO_EXECUTE     0x00000020

/* SLB (Segment Lookaside Buffer) flags */
#define SLB_KERNEL          0x00000001  /* Kernel segment */
#define SLB_USER            0x00000002  /* User segment */
#define SLB_EXECUTE         0x00000004  /* Execute permission */

/* Initialize virtual memory manager */
int vmm_init(void);

/* Create/Clone/Destroy Pagedir */
void* vmm_create_pagedir(void);
void* vmm_clone_pagedir(void* src_pagedir);
void vmm_destroy_pagedir(void* pagedir);

/* Map a page */
int vmm_map_page(void* pagedir, uintptr_t virt, uintptr_t phys, uint32_t flags);

/* Unmap a page */
int vmm_unmap_page(void* pagedir, uintptr_t virt);

/* Switch page directory */
void vmm_switch_pagedir(void* pagedir);

/* Get current page directory */
void* vmm_get_current_pagedir(void);

#endif /* VMM_H */
