/* AstraLisp OS JIT Compiler - PowerISA Code Generation */

#ifndef CODEGEN_H
#define CODEGEN_H

#include <stdint.h>
#include <stddef.h>
#include "ir.h"

/* PowerISA register allocation */
struct register_allocator {
    bool reg_used[32];  /* GPR usage */
    bool fpr_used[32];   /* FPR usage */
    uint32_t* reg_map;   /* IR reg -> PowerISA reg */
    uint32_t next_gpr;
    uint32_t next_fpr;
    uint32_t stack_offset;
};

/* Code generation context */
struct codegen_context {
    uint8_t* code_buffer;
    size_t code_size;
    size_t code_capacity;
    struct register_allocator allocator;
    uint32_t* label_addresses;
    uint32_t label_count;
};

/* Initialize code generator */
int codegen_init(struct codegen_context* ctx, size_t initial_capacity);

/* Generate code from IR function */
int codegen_compile(struct codegen_context* ctx, struct ir_function* func);

/* Get generated code */
void* codegen_get_code(struct codegen_context* ctx, size_t* size);

/* Free code generator */
void codegen_free(struct codegen_context* ctx);

/* Emit PowerISA instruction helpers */
void codegen_emit_li(struct codegen_context* ctx, uint32_t reg, int32_t imm);
void codegen_emit_addi(struct codegen_context* ctx, uint32_t dst, uint32_t src, int32_t imm);
void codegen_emit_add(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2);
void codegen_emit_sub(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2);
void codegen_emit_mul(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2);
void codegen_emit_div(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2);
void codegen_emit_ld(struct codegen_context* ctx, uint32_t dst, uint32_t base, int32_t offset);
void codegen_emit_std(struct codegen_context* ctx, uint32_t src, uint32_t base, int32_t offset);
void codegen_emit_cmp(struct codegen_context* ctx, uint32_t src1, uint32_t src2);
void codegen_emit_beq(struct codegen_context* ctx, uint32_t label);
void codegen_emit_bne(struct codegen_context* ctx, uint32_t label);
void codegen_emit_b(struct codegen_context* ctx, uint32_t label);
void codegen_emit_bl(struct codegen_context* ctx, uint32_t label);
void codegen_emit_blr(struct codegen_context* ctx);

#endif /* CODEGEN_H */
