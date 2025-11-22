/* AstraLisp OS Error Handling */

#include "error.h"
#include "lisp_io.h"
#include <stddef.h>
#include <stdlib.h>

/* Global error stack */
struct error_handler* error_stack_top = NULL;
const char* lisp_last_error_msg = NULL;

void lisp_push_handler(struct error_handler* handler) {

    handler->prev = error_stack_top;
    error_stack_top = handler;
}

void lisp_pop_handler(void) {
    if (error_stack_top) {
        error_stack_top = error_stack_top->prev;
    }
}

void lisp_error(const char* msg) {
    lisp_last_error_msg = msg;
    printf("\nDEBUG: lisp_error called with: %s\n", msg);
    LISP_PUTS("\nError: ");
    LISP_PUTS(msg);
    LISP_PUTS("\n");


    
    if (error_stack_top) {
        /* Jump to handler */
        longjmp(error_stack_top->buf, 1);
    } else {
        /* Unhandled error */
        LISP_PUTS("Fatal: Unhandled error in Lisp runtime.\n");
        #ifdef KERNEL
            /* Kernel panic or halt */
            /* For now, just hang or return to main loop if possible? 
               We can't easily return. We should probably panic. */
             // kernel_panic("Unhandled Lisp Error"); 
             /* Since we don't have kernel_panic header here, infinite loop */
             for(;;) __asm__ volatile("nop");
        #else
            exit(1);
        #endif
    }
}
