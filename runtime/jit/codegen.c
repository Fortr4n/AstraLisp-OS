/* AstraLisp OS JIT Compiler - PowerISA Code Generation Implementation */

#include "codegen.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/mm/pmm.h"
#include "../../kernel/hal/mmu.h"
#include <stddef.h>
#include <string.h>
#include <stdint.h>

#define CODEGEN_INITIAL_CAPACITY 4096

/* Emit 32-bit instruction */
static void emit_instruction(struct codegen_context* ctx, uint32_t instruction) {
    if (!ctx) {
        return;
    }
    
    /* Grow buffer if needed */
    if (ctx->code_size + 4 > ctx->code_capacity) {
        size_t new_capacity = ctx->code_capacity * 2;
        uint8_t* new_buffer = (uint8_t*)krealloc(ctx->code_buffer, new_capacity);
        if (!new_buffer) {
            return;
        }
        ctx->code_buffer = new_buffer;
        ctx->code_capacity = new_capacity;
    }
    
    /* Write instruction (big-endian for PowerISA) */
    ctx->code_buffer[ctx->code_size++] = (instruction >> 24) & 0xFF;
    ctx->code_buffer[ctx->code_size++] = (instruction >> 16) & 0xFF;
    ctx->code_buffer[ctx->code_size++] = (instruction >> 8) & 0xFF;
    ctx->code_buffer[ctx->code_size++] = instruction & 0xFF;
}

/* Allocate register */
static uint32_t allocate_register(struct codegen_context* ctx) {
    for (uint32_t i = 3; i < 32; i++) {  /* Skip r0-r2 (special purpose) */
        if (!ctx->allocator.reg_used[i]) {
            ctx->allocator.reg_used[i] = true;
            return i;
        }
    }
    
    /* Spill to stack */
    uint32_t stack_slot = ctx->allocator.stack_offset;
    ctx->allocator.stack_offset += 8;
    return 0xFFFFFFFF;  /* Indicates stack slot */
}

/* Free register */
static void free_register(struct codegen_context* ctx, uint32_t reg) {
    if (reg < 32) {
        ctx->allocator.reg_used[reg] = false;
    }
}

/* Map IR register to PowerISA register */
static uint32_t map_register(struct codegen_context* ctx, uint32_t ir_reg) {
    if (ir_reg < 32 && ctx->allocator.reg_map[ir_reg] != 0xFFFFFFFF) {
        return ctx->allocator.reg_map[ir_reg];
    }
    
    uint32_t pisa_reg = allocate_register(ctx);
    if (pisa_reg != 0xFFFFFFFF) {
        if (ir_reg < 32) {
            ctx->allocator.reg_map[ir_reg] = pisa_reg;
        }
    }
    
    return pisa_reg;
}

/* Initialize code generator */
int codegen_init(struct codegen_context* ctx, size_t initial_capacity) {
    if (!ctx) {
        return -1;
    }
    
    if (initial_capacity == 0) {
        initial_capacity = CODEGEN_INITIAL_CAPACITY;
    }
    
    ctx->code_buffer = (uint8_t*)kmalloc(initial_capacity);
    if (!ctx->code_buffer) {
        return -1;
    }
    
    ctx->code_size = 0;
    ctx->code_capacity = initial_capacity;
    
    memset(&ctx->allocator, 0, sizeof(struct register_allocator));
    ctx->allocator.reg_map = (uint32_t*)kmalloc(sizeof(uint32_t) * 256);
    if (!ctx->allocator.reg_map) {
        kfree(ctx->code_buffer);
        return -1;
    }
    
    for (uint32_t i = 0; i < 256; i++) {
        ctx->allocator.reg_map[i] = 0xFFFFFFFF;
    }
    
    ctx->label_addresses = NULL;
    ctx->label_count = 0;
    
    return 0;
}

/* Emit LI (Load Immediate) */
void codegen_emit_li(struct codegen_context* ctx, uint32_t reg, int32_t imm) {
    if (!ctx || reg >= 32) {
        return;
    }
    
    /* LI is actually ADDI rD, r0, SIMM */
    uint32_t instruction = 0x38000000;  /* ADDI opcode */
    instruction |= (reg << 21);
    instruction |= (imm & 0xFFFF);
    
    emit_instruction(ctx, instruction);
}

/* Emit ADDI */
void codegen_emit_addi(struct codegen_context* ctx, uint32_t dst, uint32_t src, int32_t imm) {
    if (!ctx || dst >= 32 || src >= 32) {
        return;
    }
    
    uint32_t instruction = 0x38000000;  /* ADDI opcode */
    instruction |= (dst << 21);
    instruction |= (src << 16);
    instruction |= (imm & 0xFFFF);
    
    emit_instruction(ctx, instruction);
}

/* Emit ADD */
void codegen_emit_add(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    if (!ctx || dst >= 32 || src1 >= 32 || src2 >= 32) {
        return;
    }
    
    uint32_t instruction = 0x7C000214;  /* ADD opcode */
    instruction |= (dst << 21);
    instruction |= (src1 << 16);
    instruction |= (src2 << 11);
    
    emit_instruction(ctx, instruction);
}

/* Emit SUB */
void codegen_emit_sub(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    if (!ctx || dst >= 32 || src1 >= 32 || src2 >= 32) {
        return;
    }
    
    uint32_t instruction = 0x7C000050;  /* SUBF opcode */
    instruction |= (dst << 21);
    instruction |= (src1 << 16);
    instruction |= (src2 << 11);
    
    emit_instruction(ctx, instruction);
}

/* Emit MUL */
void codegen_emit_mul(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    if (!ctx || dst >= 32 || src1 >= 32 || src2 >= 32) {
        return;
    }
    
    uint32_t instruction = 0x7C0001D6;  /* MULLW opcode */
    instruction |= (dst << 21);
    instruction |= (src1 << 16);
    instruction |= (src2 << 11);
    
    emit_instruction(ctx, instruction);
}

/* Emit DIV */
void codegen_emit_div(struct codegen_context* ctx, uint32_t dst, uint32_t src1, uint32_t src2) {
    if (!ctx || dst >= 32 || src1 >= 32 || src2 >= 32) {
        return;
    }
    
    uint32_t instruction = 0x7C0003D6;  /* DIVW opcode */
    instruction |= (dst << 21);
    instruction |= (src1 << 16);
    instruction |= (src2 << 11);
    
    emit_instruction(ctx, instruction);
}

/* Emit LD (Load Doubleword) */
void codegen_emit_ld(struct codegen_context* ctx, uint32_t dst, uint32_t base, int32_t offset) {
    if (!ctx || dst >= 32 || base >= 32) {
        return;
    }
    
    uint32_t instruction = 0xE8000000;  /* LD opcode */
    instruction |= (dst << 21);
    instruction |= (base << 16);
    instruction |= (offset & 0xFFFC);
    
    emit_instruction(ctx, instruction);
}

/* Emit STD (Store Doubleword) */
void codegen_emit_std(struct codegen_context* ctx, uint32_t src, uint32_t base, int32_t offset) {
    if (!ctx || src >= 32 || base >= 32) {
        return;
    }
    
    uint32_t instruction = 0xF8000000;  /* STD opcode */
    instruction |= (src << 21);
    instruction |= (base << 16);
    instruction |= (offset & 0xFFFC);
    
    emit_instruction(ctx, instruction);
}

/* Emit CMP */
void codegen_emit_cmp(struct codegen_context* ctx, uint32_t src1, uint32_t src2) {
    if (!ctx || src1 >= 32 || src2 >= 32) {
        return;
    }
    
    uint32_t instruction = 0x7C000000;  /* CMP opcode */
    instruction |= (src1 << 16);
    instruction |= (src2 << 11);
    
    emit_instruction(ctx, instruction);
}

/* Emit BEQ */
void codegen_emit_beq(struct codegen_context* ctx, uint32_t label) {
    if (!ctx) {
        return;
    }
    
    /* Placeholder for label - will be patched later */
    uint32_t instruction = 0x41820000;  /* BEQ opcode */
    emit_instruction(ctx, instruction);
}

/* Emit BNE */
void codegen_emit_bne(struct codegen_context* ctx, uint32_t label) {
    if (!ctx) {
        return;
    }
    
    uint32_t instruction = 0x40820000;  /* BNE opcode */
    emit_instruction(ctx, instruction);
}

/* Emit B (Branch) */
void codegen_emit_b(struct codegen_context* ctx, uint32_t label) {
    if (!ctx) {
        return;
    }
    
    uint32_t instruction = 0x48000000;  /* B opcode */
    emit_instruction(ctx, instruction);
}

/* Emit BL (Branch and Link) */
void codegen_emit_bl(struct codegen_context* ctx, uint32_t label) {
    if (!ctx) {
        return;
    }
    
    uint32_t instruction = 0x48000001;  /* BL opcode */
    emit_instruction(ctx, instruction);
}

/* Emit BLR (Branch to Link Register) */
void codegen_emit_blr(struct codegen_context* ctx) {
    if (!ctx) {
        return;
    }
    
    uint32_t instruction = 0x4E800020;  /* BLR opcode */
    emit_instruction(ctx, instruction);
}

/* Generate function prologue */
static void generate_prologue(struct codegen_context* ctx, uint32_t local_count) {
    /* Save link register */
    codegen_emit_std(ctx, 31, 1, -8);  /* std r31, -8(r1) */
    
    /* Allocate stack frame */
    if (local_count > 0) {
        int32_t frame_size = (local_count * 8 + 15) & ~15;
        codegen_emit_addi(ctx, 1, 1, -frame_size);  /* addi r1, r1, -frame_size */
    }
}

/* Generate function epilogue */
static void generate_epilogue(struct codegen_context* ctx) {
    /* Restore link register */
    codegen_emit_ld(ctx, 31, 1, -8);  /* ld r31, -8(r1) */
    
    /* Restore stack */
    codegen_emit_addi(ctx, 1, 1, 0);  /* Would restore actual frame size */
    
    /* Return */
    codegen_emit_blr(ctx);
}

/* Generate code from IR instruction */
static void generate_instruction(struct codegen_context* ctx, struct ir_instruction* inst) {
    if (!ctx || !inst) {
        return;
    }
    
    uint32_t dst_reg = map_register(ctx, inst->result_reg);
    
    switch (inst->opcode) {
        case IR_MOV:
            if (inst->operands[0].type == IR_OP_IMM) {
                codegen_emit_li(ctx, dst_reg, (int32_t)inst->operands[0].value.imm);
            } else if (inst->operands[0].type == IR_OP_REG) {
                uint32_t src_reg = map_register(ctx, inst->operands[0].value.reg);
                codegen_emit_addi(ctx, dst_reg, src_reg, 0);  /* Move via addi rD, rS, 0 */
            }
            break;
            
        case IR_ADD:
            {
                uint32_t src1 = map_register(ctx, inst->operands[0].value.reg);
                uint32_t src2 = map_register(ctx, inst->operands[1].value.reg);
                codegen_emit_add(ctx, dst_reg, src1, src2);
            }
            break;
            
        case IR_SUB:
            {
                uint32_t src1 = map_register(ctx, inst->operands[0].value.reg);
                uint32_t src2 = map_register(ctx, inst->operands[1].value.reg);
                codegen_emit_sub(ctx, dst_reg, src1, src2);
            }
            break;
            
        case IR_MUL:
            {
                uint32_t src1 = map_register(ctx, inst->operands[0].value.reg);
                uint32_t src2 = map_register(ctx, inst->operands[1].value.reg);
                codegen_emit_mul(ctx, dst_reg, src1, src2);
            }
            break;
            
        case IR_DIV:
            {
                uint32_t src1 = map_register(ctx, inst->operands[0].value.reg);
                uint32_t src2 = map_register(ctx, inst->operands[1].value.reg);
                codegen_emit_div(ctx, dst_reg, src1, src2);
            }
            break;
            
        case IR_LOAD:
            {
                uint32_t base_reg = map_register(ctx, inst->operands[0].value.mem.base_reg);
                int32_t offset = inst->operands[0].value.mem.offset;
                codegen_emit_ld(ctx, dst_reg, base_reg, offset);
            }
            break;
            
        case IR_STORE:
            {
                uint32_t src_reg = map_register(ctx, inst->operands[0].value.reg);
                uint32_t base_reg = map_register(ctx, inst->operands[1].value.mem.base_reg);
                int32_t offset = inst->operands[1].value.mem.offset;
                codegen_emit_std(ctx, src_reg, base_reg, offset);
            }
            break;
            
        case IR_CMP:
            {
                uint32_t src1 = map_register(ctx, inst->operands[0].value.reg);
                uint32_t src2 = map_register(ctx, inst->operands[1].value.reg);
                codegen_emit_cmp(ctx, src1, src2);
            }
            break;
            
        case IR_JZ:
            codegen_emit_beq(ctx, inst->operands[0].value.label);
            break;
            
        case IR_JNZ:
            codegen_emit_bne(ctx, inst->operands[0].value.label);
            break;
            
        case IR_JMP:
            codegen_emit_b(ctx, inst->operands[0].value.label);
            break;
            
        case IR_RET:
            codegen_emit_blr(ctx);
            break;
            
        default:
            break;
    }
}

/* Generate code from IR function */
int codegen_compile(struct codegen_context* ctx, struct ir_function* func) {
    if (!ctx || !func) {
        return -1;
    }
    
    /* Allocate label address array */
    ctx->label_addresses = (uint32_t*)kmalloc(sizeof(uint32_t) * func->block_count);
    if (!ctx->label_addresses) {
        return -1;
    }
    ctx->label_count = func->block_count;
    
    /* Generate prologue */
    generate_prologue(ctx, func->local_count);
    
    /* Generate code for each block */
    struct ir_block* block = func->entry_block;
    while (block) {
        /* Mark label address */
        ctx->label_addresses[block->block_id] = ctx->code_size;
        
        /* Generate instructions */
        struct ir_instruction* inst = block->instructions;
        while (inst) {
            generate_instruction(ctx, inst);
            inst = inst->next;
        }
        
        /* Move to next block */
        if (block->successor_count > 0) {
            block = block->successors[0];
        } else {
            break;
        }
    }
    
    /* Generate epilogue */
    generate_epilogue(ctx);
    
    return 0;
}

/* Get generated code */
void* codegen_get_code(struct codegen_context* ctx, size_t* size) {
    if (!ctx) {
        return NULL;
    }
    
    if (size) {
        *size = ctx->code_size;
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
    
    if (ctx->allocator.reg_map) {
        kfree(ctx->allocator.reg_map);
    }
    
    if (ctx->label_addresses) {
        kfree(ctx->label_addresses);
    }
}
