/* AstraLisp OS JIT Compiler - PowerISA Code Generation */

#ifndef CODEGEN_H
#define CODEGEN_H

#include <stdint.h>
#include <stddef.h>
#include "ir.h"

/* PowerISA register allocation */
#define POWERISA_R3  3   /* Return value, argument 1 */
#define POWERISA_R4  4   /* Argument 2 */
#define POWERISA_R5  5   /* Argument 3 */
#define POWERISA_R6  6   /* Argument 4 */
#define POWERISA_R7  7   /* Argument 5 */
#define POWERISA_R8  8   /* Argument 6 */
#define POWERISA_R9  9   /* Argument 7 */
#define POWERISA_R10 10  /* Argument 8 */
#define POWERISA_R11 11  /* Used for addressing */
#define POWERISA_R12 12  /* Used for addressing */
#define POWERISA_R13 13  /* Small data area pointer */
#define POWERISA_R14 14  /* Link register (saved) */
#define POWERISA_R15 15  /* Stack pointer */
#define POWERISA_R16 16  /* General purpose */
#define POWERISA_R31 31  /* General purpose */

/* Code generation context */
struct codegen_context {
    uint8_t* code_buffer;
    size_t code_size;
    size_t code_capacity;
    uint32_t* label_addresses;
    uint32_t label_count;
    uint32_t* register_map;
    uint32_t next_register;
    uint32_t stack_offset;
    struct ir_function* ir_func;
};

/* Initialize code generator */
int codegen_init(struct codegen_context* ctx, size_t initial_capacity);

/* Generate code from IR function */
int codegen_generate(struct codegen_context* ctx, struct ir_function* ir_func);

/* Get generated code */
uint8_t* codegen_get_code(struct codegen_context* ctx, size_t* code_size);

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
void codegen_emit_bl(struct codegen_context* ctx, void* target);
void codegen_emit_blr(struct codegen_context* ctx);

#endif /* CODEGEN_H */
