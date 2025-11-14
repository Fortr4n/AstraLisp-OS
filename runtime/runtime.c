/* AstraLisp OS Lisp Runtime Complete Implementation */

#include "runtime.h"
#include "lisp/reader.h"
#include "lisp/evaluator.h"
#include "lisp/macro.h"
#include "../kernel/mm/heap.h"
#include "../kernel/hal/serial.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

static bool runtime_initialized = false;
static struct lisp_environment* global_env = NULL;
static struct macro_expander* macro_expander = NULL;

/* Initialize runtime */
int runtime_init(void) {
    if (runtime_initialized) {
        return 0;
    }
    
    if (gc_init() != 0) {
        return -1;
    }
    
    if (evaluator_init() != 0) {
        return -1;
    }
    
    if (macro_init() != 0) {
        return -1;
    }
    
    global_env = env_create(NULL);
    if (!global_env) {
        return -1;
    }
    
    macro_expander = macro_expander_create(global_env);
    if (!macro_expander) {
        return -1;
    }
    
    runtime_initialized = true;
    return 0;
}

/* Evaluate Lisp expression */
struct lisp_object* runtime_eval(struct lisp_object* expr) {
    if (!runtime_initialized || !expr) {
        return NULL;
    }
    
    /* Expand macros */
    struct lisp_object* expanded = macro_expand(macro_expander, expr);
    if (!expanded) {
        return NULL;
    }
    
    /* Evaluate */
    struct lisp_object* result = lisp_eval(expanded, global_env);
    
    if (expanded != expr) {
        lisp_decref(expanded);
    }
    
    return result;
}

/* Read Lisp expression from string */
struct lisp_object* runtime_read(const char* input) {
    if (!runtime_initialized || !input) {
        return NULL;
    }
    
    struct reader_context ctx;
    if (reader_init(&ctx, input) != 0) {
        return NULL;
    }
    
    return reader_read(&ctx);
}

/* Print Lisp object */
void runtime_print(struct lisp_object* obj) {
    if (!obj) {
        return;
    }
    
    lisp_print(obj);
}
