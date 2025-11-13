/* AstraLisp OS System REPL Implementation */

#include "repl.h"
#include "../../runtime/runtime.h"
#include "../../kernel/hal/serial.h"
#include <stddef.h>
#include <string.h>

static char prompt[] = "> ";

/* Initialize REPL */
int repl_init(void) {
    if (runtime_init() != 0) {
        return -1;
    }
    return 0;
}

/* Run REPL */
void repl_run(void) {
    char buffer[1024];
    
    while (1) {
        serial_puts(prompt);
        
        /* Read input (placeholder) */
        buffer[0] = '\0';
        
        if (strlen(buffer) > 0) {
            struct lisp_object* expr = runtime_read(buffer);
            if (expr) {
                struct lisp_object* result = runtime_eval(expr);
                runtime_print(result);
                serial_puts("\n");
            }
        }
    }
}

/* Evaluate Lisp expression */
void* repl_eval(const char* input) {
    if (!input) {
        return NULL;
    }
    
    struct lisp_object* expr = runtime_read(input);
    if (!expr) {
        return NULL;
    }
    
    return runtime_eval(expr);
}
