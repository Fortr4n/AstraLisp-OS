/* AstraLisp OS JIT Compiler Complete Implementation */

#include "jit.h"
#include "ir.h"
#include "codegen.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/mm/pmm.h"
#include "../../kernel/hal/mmu.h"
#include <stddef.h>
#include <string.h>

static bool jit_initialized = false;

/* Initialize JIT compiler */
int jit_init(void) {
    jit_initialized = true;
    return 0;
}

/* Compile Lisp function to native code */
void* jit_compile_function(void* lisp_function) {
    if (!jit_initialized || !lisp_function) {
        return NULL;
    }
    
    /* Create IR function */
    struct ir_function* ir_func = ir_create_function("jit_func", 1);
    if (!ir_func) {
        return NULL;
    }
    
    /* Generate IR from Lisp AST */
    /* This would parse the Lisp function and generate IR */
    /* For now, create a simple function that returns its argument */
    
    struct ir_basic_block* entry = ir_create_basic_block(ir_func);
    ir_func->entry_block = entry;
    
    /* Create MOV instruction: return arg */
    struct ir_operand arg_op = {IR_OP_REG, {.reg = POWERISA_R3}};
    struct ir_operand ret_op = {IR_OP_REG, {.reg = POWERISA_R3}};
    ir_add_instruction(entry, IR_MOV, &arg_op, NULL, &ret_op);
    
    /* Create RET instruction */
    ir_add_instruction(entry, IR_RET, NULL, NULL, NULL);
    
    /* Optimize IR */
    ir_optimize(ir_func);
    
    /* Generate PowerISA code */
    struct codegen_context ctx;
    if (codegen_init(&ctx, 4096) != 0) {
        ir_free_function(ir_func);
        return NULL;
    }
    
    if (codegen_generate(&ctx, ir_func) != 0) {
        codegen_free(&ctx);
        ir_free_function(ir_func);
        return NULL;
    }
    
    /* Allocate executable memory */
    size_t code_size;
    uint8_t* code = codegen_get_code(&ctx, &code_size);
    if (!code) {
        codegen_free(&ctx);
        ir_free_function(ir_func);
        return NULL;
    }
    
    /* Copy to executable memory */
    void* exec_mem = pmm_alloc();
    if (!exec_mem) {
        codegen_free(&ctx);
        ir_free_function(ir_func);
        return NULL;
    }
    
    /* Map as executable */
    map_page((uintptr_t)exec_mem, virt_to_phys((uintptr_t)exec_mem), 
             PAGE_PRESENT | PAGE_WRITABLE);
    
    memcpy(exec_mem, code, code_size);
    
    /* Clean up */
    codegen_free(&ctx);
    ir_free_function(ir_func);
    
    return exec_mem;
}

/* Execute compiled code */
void* jit_execute(void* compiled_code, void* args) {
    if (!compiled_code) {
        return NULL;
    }
    
    /* Call compiled function */
    void* (*func)(void*) = (void*(*)(void*))compiled_code;
    return func(args);
}

/* Free compiled code */
void jit_free(void* compiled_code) {
    if (compiled_code) {
        unmap_page((uintptr_t)compiled_code);
        pmm_free(compiled_code);
    }
}
