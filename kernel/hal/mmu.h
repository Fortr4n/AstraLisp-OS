/* AstraLisp OS MMU Operations */

#ifndef MMU_H
#define MMU_H

#include <stdint.h>
#include <stdbool.h>

/* Page size */
#define PAGE_SIZE 4096

/* Page flags */
#define PAGE_PRESENT  (1 << 0)
#define PAGE_WRITABLE (1 << 1)
#define PAGE_USER     (1 << 2)
#define PAGE_PWT      (1 << 3)
#define PAGE_PCD      (1 << 4)
#define PAGE_SIZE_2M  (1 << 7)
#define PAGE_GLOBAL   (1 << 8)

/* Map a page */
int map_page(uintptr_t virt, uintptr_t phys, uint32_t flags);

/* Unmap a page */
int unmap_page(uintptr_t virt);

/* Set page flags */
int set_page_flags(uintptr_t virt, uint32_t flags);

/* Get physical address from virtual */
uintptr_t virt_to_phys(uintptr_t virt);

/* Invalidate TLB */
void invalidate_tlb(void);
void invalidate_tlb_page(uintptr_t virt);

/* Initialize MMU */
int mmu_init(void);

#endif /* MMU_H */
