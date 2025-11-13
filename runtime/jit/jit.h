/* AstraLisp OS JIT Compiler */

#ifndef JIT_H
#define JIT_H

#include <stdint.h>
#include <stddef.h>

/* JIT compilation context */
struct jit_context {
    void* code_buffer;
    size_t code_size;
    size_t code_capacity;
};

/* Initialize JIT compiler */
int jit_init(void);

/* Compile Lisp function to native code */
void* jit_compile_function(void* lisp_function);

/* Execute compiled code */
void* jit_execute(void* compiled_code, void* args);

/* Free compiled code */
void jit_free(void* compiled_code);

#endif /* JIT_H */
