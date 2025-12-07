#include "drivers/opal/opal.h"
#include "arch/ppc64/xive.h"

#include "drivers/opal/opal.h"
#include "arch/ppc64/xive.h"
#include "mm/pmm.h"
#include "mm/vmm.h"
#include "mm/heap.h"
#include "multiboot2.h"
#include "../runtime/lisp/gc.h"      /* Lisp Headers */
#include "../runtime/lisp/evaluator.h"
#include "../runtime/lisp/reader.h"
#include "../runtime/lisp/drivers.h"

/* Helper to convert serial char to Lisp reader input? 
   No, we'll just buffer a line. */

/* Kernel main entry point */
/* r3 = magic, r4 = ptr (MBI or FDT) */
void kernel_main(uint64_t magic, void* addr) {
    /* PowerPC Pre-Init Check */
    /* Verify we are in Hypervisor mode, etc. (Done in ASM) */

    /* If we have FDT (Linux Boot Protocol), r3 is FDT. 
       If Multiboot2, r3 is Magic (0x36d76289), r4 is MBI. */
    
    void* mb_info = NULL;
    void* fdt_ptr = NULL;
    
    if (magic == 0x36d76289) {
        mb_info = addr;
        /* Need to find FDT inside MBI or assume none? */
    } else {
        /* Assume Linux/Skiboot protocol: r3 (magic arg) is actually FDT? */
        /* wait, Linux entry: r3=FDT, r4=0, r5=0. */
        /* So if magic looks like a pointer (aligned, kernel base?), it might be FDT */
        /* For safety, let's treat 'magic' as FDT if it's not the Multiboot magic number */
        fdt_ptr = (void*)magic;
    }
    
    /* Initialize OPAL if FDT is present */
    /* If we have no FDT, we might be on bare metal without OPAL (e.g. QEMU -kernel) */
    /* but checking FDT is safe. */
    if (fdt_ptr) {
        if (opal_init(fdt_ptr) == 0) {
            opal_puts("OPAL Initialized.\n");
            
             /* Initialize XIVE Interrupt Controller */
            if (xive_init(fdt_ptr) != 0) {
                opal_puts("XIVE Init Failed.\n");
            }
        }
    }
    
    /* Initialize PMM */
    /* PMM currently expects MBI. If we only have FDT, PMM will fail. */
    /* TODO: update PMM to support FDT. For now, pass MBI. */
    if (pmm_init(mb_info) != 0) {
        /* If PMM fails, we can't allocate memory. Critical. */
        /* Using opal_puts if avail */
        if (fdt_ptr) opal_puts("PMM Failed (MBI missing?)\n");
        for(;;);
    }
    
    /* Initialize VMM (Radix) */
    if (vmm_init() != 0) {
        if (fdt_ptr) opal_puts("VMM Failed\n");
        for(;;);
    }
    
    /* Initialize Heap */
    if (heap_init() != 0) {
         if (fdt_ptr) opal_puts("Heap Failed\n");
         for(;;);
    }
    
    /* Initialize Lisp Runtime */
    if (fdt_ptr) opal_puts("Initializing AstraLisp Runtime...\n");
    
    if (gc_init() != 0) {
         /* panic */
         for(;;);
    }
    
    if (evaluator_init() != 0) {
         for(;;);
    }
    
    drivers_init(); /* Init Lisp drivers */
    
    if (fdt_ptr) opal_puts("Entering REPL...\n");
    
    /* REPL */
    char input_buffer[256];
    int pos = 0;
    
    for (;;) {
        /* Check OPAL console for input */
        int64_t rc;
        char c;
        // rc = opal_console_read(0, &len, buf);
        // This is a blocking/async quirk in OPAL. 
        // Need to poll.
        // Simplified:
        // if (opal_poll_char(&c)) { ... }
        
        /* For now, just idle loop */
         __asm__ volatile ("or 27,27,27"); /* yield */
    }
}

