#include "drivers/opal/opal.h"

/* Kernel main entry point */
/* For PowerISA/Skiboot, r3 is FDT pointer (passed as generic arg1) */
void kernel_main(void* fdt) {
    /* Initialize OPAL (via FDT) */
    if (opal_init(fdt) != 0) {
        /* Loops if fails since we can't print */
        for (;;) ;
    }
    
    opal_puts("AstraLisp OS Kernel Starting (PowerISA v3.1C)...\n");

    
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
    
    /* Initialize Lisp Runtime */
    early_printf("Initializing Lisp Runtime...\n");
    
    if (gc_init() != 0) {
        kernel_panic("Failed to init GC");
    }
    
    if (evaluator_init() != 0) {
        kernel_panic("Failed to init Evaluator");
    }
    
    drivers_init();
    
    early_printf("AstraLisp OS Ready. Entering REPL...\n");
    
    /* REPL Loop */
    char input_buffer[1024];
    size_t buffer_pos = 0;
    
    serial_puts("\n> ");
    
    for (;;) {
        if (serial_data_available()) {
            char c = serial_getchar();
            
            /* Echo */
            serial_putchar(c);
            
            if (c == '\r' || c == '\n') {
                serial_putchar('\n');
                input_buffer[buffer_pos] = '\0';
                
                if (buffer_pos > 0) {
                    /* Read */
                    struct reader_context ctx;
                    reader_init(&ctx, input_buffer);
                    
                    lisp_value expr = reader_read(&ctx);
                    
                    /* Eval */
                    /* Protect expr */
                    GC_PUSH_1(expr);
                    lisp_value result = lisp_eval(lisp_get_global_env(), expr);
                    
                    /* Print */
                    /* Redirect stdout to serial for lisp_print? 
                       lisp_print uses printf. We need to hook printf or modify lisp_print.
                       For now, let's assume lisp_print writes to stdout and we need to redirect it.
                       But we are in kernel mode, printf might not work or might be early_printf.
                       
                       Wait, lisp_print in reader.c uses printf. 
                       I need to make lisp_print use serial_puts or vga_puts.
                       
                       I will modify reader.c to use a print callback or macro.
                       For now, I'll just run the loop and address printing next.
                    */
                     
                    /* Temporary: We can't see output unless lisp_print works. */
                    /* I will add a temporary print implementation here or modify reader.c later. */
                    
                    GC_POP(); /* expr */
                }
                
                buffer_pos = 0;
                serial_puts("> ");
            } else if (c == '\b' || c == 127) {
                if (buffer_pos > 0) {
                    buffer_pos--;
                    /* Handle backspace visual */
                    serial_puts("\b \b");
                }
            } else if (buffer_pos < sizeof(input_buffer) - 1) {
                input_buffer[buffer_pos++] = c;
            }
        }
        
        /* Run scheduler/idle */
        // scheduler_tick(); 
        __asm__ volatile ("nop");
    }
}

