/* AstraLisp OS Kernel Main Entry Point */

#include <stdint.h>
#include <stddef.h>
#include "hal/serial.h"
#include "hal/vga.h"
#include "mm/pmm.h"
#include "mm/vmm.h"
#include "mm/heap.h"
#include "interrupt/idt.h"
#include "process/scheduler.h"
#include "process/process.h"
#include "../runtime/runtime.h"
#include "lisp/kernel-lisp.h"

/* Forward declarations */
extern void interrupt_handler(uint32_t interrupt_number, void* stack_frame);
extern void exception_handler(uint32_t exception_number, void* stack_frame);
extern void* syscall_handler(uint32_t syscall_number, void* args);

/* Multiboot 2 structures */
struct multiboot_tag {
    uint32_t type;
    uint32_t size;
};

struct multiboot_info {
    uint32_t total_size;
    uint32_t reserved;
    struct multiboot_tag tags[];
};

/* Kernel panic handler */
void kernel_panic(const char* message) {
    serial_puts("KERNEL PANIC: ");
    serial_puts(message);
    serial_puts("\n");
    
    vga_puts("KERNEL PANIC: ");
    vga_puts(message);
    vga_puts("\n");
    
    /* Halt */
    for (;;) {
        __asm__ volatile ("nop");
    }
}

/* Early printf implementation */
void early_printf(const char* format, ...) {
    /* Simple implementation for now */
    serial_puts(format);
}

/* Kernel main entry point */
void kernel_main(uint32_t magic, struct multiboot_info* mbi) {
    /* Initialize serial console first */
    serial_init();
    serial_puts("AstraLisp OS Kernel Starting...\n");
    
    /* Initialize VGA */
    vga_init();
    vga_puts("AstraLisp OS Kernel Starting...\n");
    
    /* Verify multiboot magic */
    if (magic != 0x36D76289) {
        kernel_panic("Invalid multiboot magic number");
    }
    
    early_printf("Multiboot info at: 0x%p\n", mbi);
    
    /* Parse multiboot tags */
    if (mbi) {
        uint32_t total_size = mbi->total_size;
        struct multiboot_tag* tag = mbi->tags;
        
        while ((uintptr_t)tag < (uintptr_t)mbi + total_size) {
            switch (tag->type) {
                case 0: /* End tag */
                    goto tags_done;
                case 6: /* Memory map */
                    early_printf("Memory map tag found\n");
                    break;
                case 8: /* Boot device */
                    early_printf("Boot device tag found\n");
                    break;
                case 2: /* Command line */
                    early_printf("Command line tag found\n");
                    break;
                default:
                    early_printf("Unknown tag: %u\n", tag->type);
                    break;
            }
            
            /* Move to next tag (aligned to 8 bytes) */
            tag = (struct multiboot_tag*)((uintptr_t)tag + ((tag->size + 7) & ~7));
        }
    }
    
tags_done:
    early_printf("Initializing memory management...\n");
    
    /* Initialize physical memory manager */
    if (pmm_init(mbi) != 0) {
        kernel_panic("Failed to initialize PMM");
    }
    
    /* Initialize virtual memory manager */
    if (vmm_init() != 0) {
        kernel_panic("Failed to initialize VMM");
    }
    
    /* Initialize kernel heap */
    if (heap_init() != 0) {
        kernel_panic("Failed to initialize heap");
    }
    
    early_printf("Initializing Lisp runtime...\n");
    
    /* Initialize Lisp runtime */
    if (runtime_init() != 0) {
        kernel_panic("Failed to initialize Lisp runtime");
    }
    
    /* Initialize kernel Lisp interface */
    if (kernel_lisp_init() != 0) {
        kernel_panic("Failed to initialize kernel Lisp interface");
    }
    
    if (kernel_lisp_register_functions() != 0) {
        kernel_panic("Failed to register kernel Lisp functions");
    }
    
    early_printf("Initializing interrupt system...\n");
    
    /* Initialize interrupt descriptor table */
    if (idt_init() != 0) {
        kernel_panic("Failed to initialize IDT");
    }
    
    /* Enable interrupts */
    __asm__ volatile ("mfmsr %r0; ori %r0, %r0, 0x8000; mtmsr %r0" ::: "r0");
    
    early_printf("Initializing scheduler...\n");
    
    /* Initialize scheduler */
    if (scheduler_init() != 0) {
        kernel_panic("Failed to initialize scheduler");
    }
    
    /* Create initial kernel thread */
    early_printf("Creating initial kernel thread...\n");
    
    early_printf("Kernel initialization complete!\n");
    early_printf("Entering main loop...\n");
    
    /* Main kernel loop */
    for (;;) {
        /* Run scheduler */
        scheduler_tick();
        
        /* Idle */
        __asm__ volatile ("nop");
    }
}
