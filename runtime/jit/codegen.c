/* AstraLisp OS JIT Compiler - PowerISA Code Generation Implementation */

#include "codegen.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/mm/pmm.h"
#include "../../kernel/hal/mmu.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define CODEGEN_INITIAL_CAPACITY 4096
#define CODEGEN_GROWTH_FACTOR 2

/* Emit 32-bit instruction */
static void codegen_emit32(struct codegen_context* ctx, uint32_t instruction) {
    if (ctx->code_size + 4 > ctx->code_capacity) {
        size_t new_capacity = ctx->code_capacity * CODEGEN_GROWTH_FACTOR;
        uint8_t* new_buffer = (uint8_t*)kmalloc(new_capacity);
        if (!new_buffer) {
            return;
        }
        
        memcpy(new_buffer, ctx->code_buffer, ctx->code_size);
        kfree(ctx->code_buffer);
        ctx->code_buffer = new_buffer;
        ctx->code_capacity = new_capacity;
    }
    
    *((uint32_t*)(ctx->code_buffer + ctx->code_size)) = instruction;
    ctx->code_size += 4;
}

/* PowerISA instruction encoding helpers */
static uint32_t encode_r_form(uint32_t opcode, uint32_t rt, uint32_t ra, uint32_t rb, uint32_t xo, uint32_t rc) {
    return (opcode << 26) | (rt << 21) | (ra << 16) | (rb << 11) | (xo << 1) | rc;
}

static uint32_t encode_d_form(uint32_t opcode, uint32_t rt, uint32_t ra, int32_t d) {
    return (opcode << 26) | (rt << 21) | (ra << 16) | ((uint32_t)d & 0xFFFF);
}

static uint32_t encode_i_form(uint32_t opcode, uint32_t rt, uint32_t ra, int32_t si) {
    return (opcode << 26) | (rt << 21) | (ra << 16) | ((uint32_t)si & 0xFFFF);
}

/* Emit LI (Load Immediate) */
void codegen_emit_li(struct codegen_context* ctx, uint32_t reg, int32_t imm) {
    if (imm >= -32768 && imm <= 32767) {
        /* Use addi with r0 */
        codegen_emit32(ctx, encode_d_form(14, reg, 0, imm));  /* ADDI */
    } else {
        /* Load high 16 bits */
        int32_t high = (imm >> 16) & 0xFFFF;
        int32_t low = imm & 0xFFFF;
        codegen_emit32(ctx, encode_d_form(15, reg, 0, high));  /* ADDIS */
        if (low != 0) {
            codegen_emit32(ctx, encode_d_form(14, reg, reg, low));  /* ADDI */
        }
    }
}

/* Emit ADDI */
void codegen_emit_addi(struct codegen_context* ctx, uint32_t dst, uint32_t src, int32_t imm) {
    codegen_emit32(ctx, encode_d_form(14, dst, src, imm));
}

/* Emit ADD */
void codegen_emit_add(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    codegen_emit32(ctx, encode_r_form(31, dst, src1, src2, 266, 0));  /* ADD */
}

/* Emit SUB */
void codegen_emit_sub(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    codegen_emit32(ctx, encode_r_form(31, dst, src1, src2, 40, 0));  /* SUBF */
}

/* Emit MUL */
void codegen_emit_mul(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    codegen_emit32(ctx, encode_r_form(31, dst, src1, src2, 235, 0));  /* MULLW */
}

/* Emit DIV */
void codegen_emit_div(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    codegen_emit32(ctx, encode_r_form(31, dst, src1, src2, 491, 0));  /* DIVW */
}

/* Emit LD (Load Doubleword) */
void codegen_emit_ld(struct codegen_context* ctx, uint32_t dst, uint32_t base, int32_t offset) {
    codegen_emit32(ctx, encode_d_form(58, dst, base, offset));
}

/* Emit STD (Store Doubleword) */
void codegen_emit_std(struct codegen_context* ctx, uint32_t src, uint32_t base, int32_t offset) {
    codegen_emit32(ctx, encode_d_form(62, src, base, offset));
}

/* Emit CMP */
void codegen_emit_cmp(struct codegen_context* ctx, uint32_t src1, uint32_t src2) {
    codegen_emit32(ctx, encode_r_form(31, 0, src1, src2, 0, 0));  /* CMP */
}

/* Emit BEQ */
void codegen_emit_beq(struct codegen_context* ctx, uint32_t label) {
    /* Placeholder for label - will be patched later */
    codegen_emit32(ctx, encode_i_form(18, 0, 0, 0));  /* BC with EQ */
}

/* Emit BNE */
void codegen_emit_bne(struct codegen_context* ctx, uint32_t label) {
    codegen_emit32(ctx, encode_i_form(18, 0, 0, 0));  /* BC with NE */
}

/* Emit BL (Branch and Link) */
void codegen_emit_bl(struct codegen_context* ctx, void* target) {
    /* Calculate offset */
    int32_t offset = (int32_t)((uintptr_t)target - (uintptr_t)(ctx->code_buffer + ctx->code_size + 4));
    codegen_emit32(ctx, encode_i_form(18, 0, 1, offset >> 2));  /* BL */
}

/* Emit BLR (Branch to Link Register) */
void codegen_emit_blr(struct codegen_context* ctx) {
    codegen_emit32(ctx, encode_r_form(19, 0, 0, 0, 16, 0));  /* BLR */
}

/* Initialize code generator */
int codegen_init(struct codegen_context* ctx, size_t initial_capacity) {
    if (!ctx) {
        return -1;
    }
    
    ctx->code_capacity = initial_capacity > 0 ? initial_capacity : CODEGEN_INITIAL_CAPACITY;
    ctx->code_buffer = (uint8_t*)kmalloc(ctx->code_capacity);
    if (!ctx->code_buffer) {
        return -1;
    }
    
    ctx->code_size = 0;
    ctx->label_addresses = NULL;
    ctx->label_count = 0;
    ctx->register_map = NULL;
    ctx->next_register = POWERISA_R3;
    ctx->stack_offset = 0;
    ctx->ir_func = NULL;
    
    return 0;
}

/* Generate function prologue */
static void codegen_prologue(struct codegen_context* ctx, uint32_t local_count) {
    /* Save link register */
    codegen_emit_std(ctx, POWERISA_R14, POWERISA_R15, -8);
    
    /* Allocate stack frame */
    int32_t frame_size = (local_count * 8 + 15) & ~15;
    if (frame_size > 0) {
        codegen_emit_addi(ctx, POWERISA_R15, POWERISA_R15, -frame_size);
    }
    
    ctx->stack_offset = frame_size;
}

/* Generate function epilogue */
static void codegen_epilogue(struct codegen_context* ctx) {
    /* Restore stack frame */
    if (ctx->stack_offset > 0) {
        codegen_emit_addi(ctx, POWERISA_R15, POWERISA_R15, ctx->stack_offset);
    }
    
    /* Restore link register */
    codegen_emit_ld(ctx, POWERISA_R14, POWERISA_R15, -8);
    
    /* Return */
    codegen_emit_blr(ctx);
}

/* Generate code from IR instruction */
static void codegen_instruction(struct codegen_context* ctx, struct ir_instruction* instr) {
    switch (instr->opcode) {
        case IR_MOV:
            if (instr->operands[0].type == IR_OP_IMM) {
                codegen_emit_li(ctx, instr->operands[1].value.reg, instr->operands[0].value.imm);
            } else {
                codegen_emit_addi(ctx, instr->operands[1].value.reg, 
                                 instr->operands[0].value.reg, 0);
            }
            break;
            
        case IR_ADD:
            codegen_emit_add(ctx, instr->operands[2].value.reg,
                            instr->operands[0].value.reg, instr->operands[1].value.reg);
            break;
            
        case IR_SUB:
            codegen_emit_sub(ctx, instr->operands[2].value.reg,
                            instr->operands[0].value.reg, instr->operands[1].value.reg);
            break;
            
        case IR_MUL:
            codegen_emit_mul(ctx, instr->operands[2].value.reg,
                           instr->operands[0].value.reg, instr->operands[1].value.reg);
            break;
            
        case IR_DIV:
            codegen_emit_div(ctx, instr->operands[2].value.reg,
                            instr->operands[0].value.reg, instr->operands[1].value.reg);
            break;
            
        case IR_LOAD:
            codegen_emit_ld(ctx, instr->operands[1].value.reg,
                           instr->operands[0].value.mem.base_reg,
                           instr->operands[0].value.mem.offset);
            break;
            
        case IR_STORE:
            codegen_emit_std(ctx, instr->operands[0].value.reg,
                            instr->operands[1].value.mem.base_reg,
                            instr->operands[1].value.mem.offset);
            break;
            
        case IR_CMP:
            codegen_emit_cmp(ctx, instr->operands[0].value.reg, instr->operands[1].value.reg);
            break;
            
        case IR_JZ:
            codegen_emit_beq(ctx, instr->operands[0].value.label);
            break;
            
        case IR_JNZ:
            codegen_emit_bne(ctx, instr->operands[0].value.label);
            break;
            
        case IR_RET:
            codegen_epilogue(ctx);
            break;
            
        default:
            break;
    }
}

/* Generate code from IR function */
int codegen_generate(struct codegen_context* ctx, struct ir_function* ir_func) {
    if (!ctx || !ir_func) {
        return -1;
    }
    
    ctx->ir_func = ir_func;
    
    /* Generate prologue */
    codegen_prologue(ctx, ir_func->local_count);
    
    /* Generate code for each basic block */
    struct ir_basic_block* block = ir_func->entry_block;
    while (block) {
        /* Set label address */
        if (block->block_id < ctx->label_count) {
            ctx->label_addresses[block->block_id] = ctx->code_size;
        }
        
        /* Generate code for each instruction */
        struct ir_instruction* instr = block->instructions;
        while (instr) {
            codegen_instruction(ctx, instr);
            instr = instr->next;
        }
        
        /* Move to next block */
        if (block->successor_count > 0) {
            block = block->successors[0];
        } else {
            break;
        }
    }
    
    /* Patch label references */
    /* This would iterate through instructions and patch branch targets */
    
    /* Make code executable */
    invalidate_tlb();
    
    return 0;
}

/* Get generated code */
uint8_t* codegen_get_code(struct codegen_context* ctx, size_t* code_size) {
    if (!ctx) {
        return NULL;
    }
    
    if (code_size) {
        *code_size = ctx->code_size;
    }
    
    return ctx->code_buffer;
}

/* Free code generator */
void codegen_free(struct codegen_context* ctx) {
    if (!ctx) {
        return;
    }
    
    if (ctx->code_buffer) {
        kfree(ctx->code_buffer);
    }
    
    if (ctx->label_addresses) {
        kfree(ctx->label_addresses);
    }
    
    if (ctx->register_map) {
        kfree(ctx->register_map);
    }
}
