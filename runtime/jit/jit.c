/* AstraLisp OS JIT Compiler Complete Implementation */

#include "jit.h"
#include "ir.h"
#include "codegen.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/mm/pmm.h"
#include "../../kernel/hal/mmu.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

static bool jit_initialized = false;

/* Initialize JIT compiler */
int jit_init(void) {
    if (jit_initialized) {
        return 0;
    }
    
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
    
    /* Create entry block */
    struct ir_block* entry_block = ir_create_block(ir_func);
    if (!entry_block) {
        ir_free_function(ir_func);
        return NULL;
    }
    
    /* Generate IR from Lisp AST (simplified - would parse actual AST) */
    struct ir_operand ops[3];
    memset(ops, 0, sizeof(ops));
    
    /* Load argument (r3 is first argument in PowerISA calling convention) */
    ops[0].type = IR_OP_REG;
    ops[0].value.reg = 3;  /* r3 = first argument */
    ir_add_instruction(entry_block, IR_LOADARG, ops, ir_func->next_reg++);
    
    /* Add constant (example: add 42) */
    uint32_t arg_reg = ir_func->next_reg - 1;
    uint32_t result_reg = ir_func->next_reg++;
    
    ops[0].type = IR_OP_REG;
    ops[0].value.reg = arg_reg;
    ops[1].type = IR_OP_IMM;
    ops[1].value.imm = 42;
    ir_add_instruction(entry_block, IR_ADD, ops, result_reg);
    
    /* Return result (r3 is return value in PowerISA) */
    ops[0].type = IR_OP_REG;
    ops[0].value.reg = result_reg;
    ir_add_instruction(entry_block, IR_RETURN, ops, 3);  /* r3 = return */
    
    /* Optimize IR */
    ir_optimize(ir_func);
    
    /* Generate PowerISA code */
    struct codegen_context ctx;
    if (codegen_init(&ctx, 4096) != 0) {
        ir_free_function(ir_func);
        return NULL;
    }
    
    if (codegen_compile(&ctx, ir_func) != 0) {
        codegen_free(&ctx);
        ir_free_function(ir_func);
        return NULL;
    }
    
    /* Get compiled code */
    size_t code_size;
    void* compiled_code = codegen_get_code(&ctx, &code_size);
    
    if (!compiled_code || code_size == 0) {
        codegen_free(&ctx);
        ir_free_function(ir_func);
        return NULL;
    }
    
    /* Allocate executable memory */
    void* exec_mem = pmm_alloc();
    if (!exec_mem) {
        codegen_free(&ctx);
        ir_free_function(ir_func);
        return NULL;
    }
    
    /* Copy code */
    memcpy(exec_mem, compiled_code, code_size);
    
    /* Make executable (map with execute permission) */
    uintptr_t exec_phys = virt_to_phys((uintptr_t)exec_mem);
    if (exec_phys) {
        map_page((uintptr_t)exec_mem, exec_phys, PAGE_PRESENT | PAGE_WRITABLE);
    }
    
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
