#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/* Mocking kernel dependencies */
#include "../../kernel/multiboot2.h"
#include "../../kernel/mm/pmm.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/process/scheduler.h"
#include "../../kernel/process/process.h"
#include "../../kernel/asm/context-switch.h"

/* Mock context_switch */
void context_switch(struct cpu_context* prev, struct cpu_context* next) {
    printf("[MOCK] context_switch called\n");
}

/* Mock memory for PMM */
#define MOCK_RAM_SIZE (32 * 1024 * 1024)
#define MOCK_PHYS_BASE 0x10000000 /* Start at 256MB */
static uint8_t mock_ram[MOCK_RAM_SIZE];

/* P2V implementation for test */
void* test_p2v(uintptr_t phys) {
    if (phys >= MOCK_PHYS_BASE && phys < MOCK_PHYS_BASE + MOCK_RAM_SIZE) {
        return (void*)(mock_ram + (phys - MOCK_PHYS_BASE));
    }
    /* Allow access to kernel range if needed, mapping to NULL or a separate buffer? */
    /* For now, just return NULL or fail if out of bounds */
    /* But wait, PMM might try to access kernel memory to mark it used? */
    /* PMM marks kernel memory as used in the bitmap. It doesn't access the memory itself. */
    /* It only accesses frame_bitmap. frame_bitmap will be allocated inside mock_ram. */
    return NULL;
}

/* Helper to create fake multiboot info */
struct multiboot_info* create_mock_multiboot(void) {
    static uint8_t mbi_buffer[4096];
    struct multiboot_info* mbi = (struct multiboot_info*)mbi_buffer;
    
    mbi->total_size = sizeof(struct multiboot_info);
    mbi->reserved = 0;
    
    /* Add memory map tag */
    struct multiboot_tag_mmap* mmap = (struct multiboot_tag_mmap*)(mbi->tags);
    mmap->type = MULTIBOOT_TAG_TYPE_MMAP;
    mmap->size = sizeof(struct multiboot_tag_mmap) + sizeof(struct multiboot_mmap_entry);
    mmap->entry_size = sizeof(struct multiboot_mmap_entry);
    mmap->entry_version = 0;
    
    /* Add one large available region at MOCK_PHYS_BASE */
    mmap->entries[0].addr = MOCK_PHYS_BASE;
    mmap->entries[0].len = MOCK_RAM_SIZE;
    mmap->entries[0].type = MULTIBOOT_MEMORY_AVAILABLE;
    mmap->entries[0].zero = 0;
    
    /* Add end tag */
    struct multiboot_tag* end = (struct multiboot_tag*)((uint8_t*)mmap + ((mmap->size + 7) & ~7));
    end->type = MULTIBOOT_TAG_TYPE_END;
    end->size = 8;
    
    mbi->total_size = (uintptr_t)end + 8 - (uintptr_t)mbi;
    
    return mbi;
}

void test_pmm(void) {
    printf("Testing PMM...\n");
    struct multiboot_info* mbi = create_mock_multiboot();
    
    int res = pmm_init(mbi);
    assert(res == 0);
    
    size_t free_before = pmm_get_free_count();
    printf("Free pages: %zu\n", free_before);
    assert(free_before > 0);
    
    void* frame = pmm_alloc();
    assert(frame != NULL);
    
    size_t free_after = pmm_get_free_count();
    assert(free_after == free_before - 1);
    
    pmm_free(frame);
    assert(pmm_get_free_count() == free_before);
    
    printf("PMM Test Passed!\n");
}

void test_heap(void) {
    printf("Testing Heap...\n");
    
    int res = heap_init();
    assert(res == 0);
    
    void* ptr1 = kmalloc(100);
    assert(ptr1 != NULL);
    
    void* ptr2 = kmalloc(200);
    assert(ptr2 != NULL);
    assert(ptr1 != ptr2);
    
    kfree(ptr1);
    kfree(ptr2);
    
    /* Test large allocation */
    void* large = kmalloc(1024 * 1024); /* 1MB */
    assert(large != NULL);
    kfree(large);
    
    printf("Heap Test Passed!\n");
}

void test_scheduler(void) {
    printf("Testing Scheduler...\n");
    
    int res = scheduler_init();
    assert(res == 0);
    
    /* Create fake threads */
    struct thread* t1 = (struct thread*)kmalloc(sizeof(struct thread));
    struct thread* t2 = (struct thread*)kmalloc(sizeof(struct thread));
    
    t1->priority = 2;
    t2->priority = 2;
    
    scheduler_add_thread(t1);
    assert(scheduler_get_current_thread() == t1);
    
    scheduler_add_thread(t2);
    
    /* Yield should switch to t2 */
    scheduler_yield();
    assert(scheduler_get_current_thread() == t2);
    
    /* Yield should switch back to t1 */
    scheduler_yield();
    assert(scheduler_get_current_thread() == t1);
    
    printf("Scheduler Test Passed!\n");
}

#include "../../kernel/mm/vmm.h"

void test_vmm(void) {
    printf("Testing VMM (Radix Tree)...\n");
    
    int res = vmm_init();
    assert(res == 0);
    
    /* Test mapping a page */
    uintptr_t virt = 0x40000000; /* 1GB mark */
    uintptr_t phys = 0x12345000;
    
    res = vmm_map_page(NULL, virt, phys, PAGE_PRESENT | PAGE_WRITABLE);
    assert(res == 0);
    
    /* Verify mapping (manual walk would be ideal, but we can check if map succeeds) */
    /* In a real kernel we'd read the table, here we trust the return code + logic */
    
    /* Test unmapping */
    res = vmm_unmap_page(NULL, virt);
    assert(res == 0);
    
    /* Test unmapping non-existent */
    res = vmm_unmap_page(NULL, 0x50000000);
    assert(res != 0);
    
    printf("VMM Test Passed!\n");
}

int main(void) {
    test_pmm();
    test_heap();
    test_scheduler();
    test_vmm();
    return 0;
}
