/* AstraLisp OS System REPL Complete Implementation */

#include "repl.h"
#include "../../runtime/runtime.h"
#include "../../runtime/lisp/reader.h"
#include "../../runtime/lisp/evaluator.h"
#include "../../kernel/hal/serial.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

static char prompt[] = "astralisp> ";
static bool repl_initialized = false;

/* Initialize REPL */
int repl_init(void) {
    if (repl_initialized) {
        return 0;
    }
    
    if (runtime_init() != 0) {
        return -1;
    }
    
    repl_initialized = true;
    return 0;
}

/* Read line from input */
static int read_line(char* buffer, size_t size) {
    if (!buffer || size == 0) {
        return -1;
    }
    
    size_t pos = 0;
    while (pos < size - 1) {
        int c = serial_getchar();
        if (c < 0) {
            continue;
        }
        
        if (c == '\n' || c == '\r') {
            buffer[pos] = '\0';
            return (int)pos;
        } else if (c == '\b' || c == 127) {
            /* Backspace */
            if (pos > 0) {
                pos--;
                serial_putchar('\b');
                serial_putchar(' ');
                serial_putchar('\b');
            }
        } else if (c >= 32 && c < 127) {
            buffer[pos++] = (char)c;
            serial_putchar(c);
        }
    }
    
    buffer[pos] = '\0';
    return (int)pos;
}

/* Run REPL */
void repl_run(void) {
    if (!repl_initialized) {
        if (repl_init() != 0) {
            serial_puts("Failed to initialize REPL\n");
            return;
        }
    }
    
    char buffer[1024];
    
    while (1) {
        serial_puts(prompt);
        
        int len = read_line(buffer, sizeof(buffer));
        if (len <= 0) {
            continue;
        }
        
        if (strlen(buffer) == 0) {
            continue;
        }
        
        /* Read expression */
        struct lisp_object* expr = runtime_read(buffer);
        if (!expr) {
            serial_puts("Error: Failed to read expression\n");
            continue;
        }
        
        /* Evaluate */
        struct lisp_object* result = runtime_eval(expr);
        if (result) {
            runtime_print(result);
            serial_puts("\n");
            lisp_decref(result);
        } else {
            serial_puts("Error: Evaluation failed\n");
        }
        
        lisp_decref(expr);
    }
}

/* Evaluate Lisp expression */
void* repl_eval(const char* input) {
    if (!repl_initialized || !input) {
        return NULL;
    }
    
    struct lisp_object* expr = runtime_read(input);
    if (!expr) {
        return NULL;
    }
    
    return runtime_eval(expr);
}
