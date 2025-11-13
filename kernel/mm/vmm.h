/* AstraLisp OS Virtual Memory Manager */

#ifndef VMM_H
#define VMM_H

#include <stdint.h>
#include <stdbool.h>

/* Initialize virtual memory manager */
int vmm_init(void);

/* Create a new page directory */
void* vmm_create_pagedir(void);

/* Destroy a page directory */
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
