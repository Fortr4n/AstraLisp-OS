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

/* Allocate multiple contiguous page frames */
void* pmm_alloc_multiple(size_t pages);

/* Get number of free pages */
size_t pmm_get_free_count(void);

/* Get number of used pages */
size_t pmm_get_used_count(void);

#ifdef TEST_MODE
#include <stdio.h>
extern void* test_p2v(uintptr_t phys);
#define P2V(x) test_p2v(x)
#else
#define P2V(x) ((void*)(x))
#endif

#endif /* PMM_H */
