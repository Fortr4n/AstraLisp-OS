/* AstraLisp OS Physical Memory Manager */

#ifndef PMM_H
#define PMM_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Initialize physical memory manager */
int pmm_init(void* multiboot_info);

/* Allocate a page frame */
void* pmm_alloc(void);

/* Free a page frame */
void pmm_free(void* frame);

/* Get number of free pages */
size_t pmm_get_free_count(void);

/* Get number of used pages */
size_t pmm_get_used_count(void);

#endif /* PMM_H */
