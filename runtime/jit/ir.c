/* AstraLisp OS JIT Compiler - IR Implementation */

#include "ir.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>

/* Create IR function */
struct ir_function* ir_create_function(const char* name, uint32_t arg_count) {
    if (!name) {
        return NULL;
    }
    
    struct ir_function* func = (struct ir_function*)kmalloc(sizeof(struct ir_function));
    if (!func) {
        return NULL;
    }
    
    func->name = (char*)kmalloc(strlen(name) + 1);
    if (!func->name) {
        kfree(func);
        return NULL;
    }
    
    strcpy(func->name, name);
    func->arg_count = arg_count;
    func->local_count = 0;
    func->entry_block = NULL;
    func->blocks = NULL;
    func->block_count = 0;
    func->next_label = 1;
    
    return func;
}

/* Create basic block */
struct ir_basic_block* ir_create_basic_block(struct ir_function* func) {
    if (!func) {
        return NULL;
    }
    
    struct ir_basic_block* block = (struct ir_basic_block*)kmalloc(sizeof(struct ir_basic_block));
    if (!block) {
        return NULL;
    }
    
    block->block_id = func->block_count++;
    block->instructions = NULL;
    block->successors = NULL;
    block->successor_count = 0;
    block->predecessors = NULL;
    block->predecessor_count = 0;
    block->visited = false;
    
    /* Add to function's block list */
    if (func->blocks) {
        struct ir_basic_block* last = func->blocks;
        while (last->next) {
            last = last->next;
        }
        last->next = block;
    } else {
        func->blocks = block;
    }
    
    return block;
}

/* Add instruction to basic block */
struct ir_instruction* ir_add_instruction(struct ir_basic_block* block, ir_opcode_t opcode,
                                          struct ir_operand* op1, struct ir_operand* op2, struct ir_operand* op3) {
    if (!block) {
        return NULL;
    }
    
    struct ir_instruction* instr = (struct ir_instruction*)kmalloc(sizeof(struct ir_instruction));
    if (!instr) {
        return NULL;
    }
    
    instr->opcode = opcode;
    if (op1) {
        instr->operands[0] = *op1;
    } else {
        memset(&instr->operands[0], 0, sizeof(struct ir_operand));
    }
    if (op2) {
        instr->operands[1] = *op2;
    } else {
        memset(&instr->operands[1], 0, sizeof(struct ir_operand));
    }
    if (op3) {
        instr->operands[2] = *op3;
    } else {
        memset(&instr->operands[2], 0, sizeof(struct ir_operand));
    }
    instr->label = 0;
    instr->next = NULL;
    instr->prev = NULL;
    
    /* Add to block */
    if (block->instructions) {
        instr->prev = block->instructions;
        while (block->instructions->next) {
            block->instructions = block->instructions->next;
        }
        block->instructions->next = instr;
        block->instructions = block->instructions->prev;
    } else {
        block->instructions = instr;
    }
    
    return instr;
}

/* Optimize IR function */
int ir_optimize(struct ir_function* func) {
    if (!func) {
        return -1;
    }
    
    /* Constant folding */
    struct ir_basic_block* block = func->blocks;
    while (block) {
        struct ir_instruction* instr = block->instructions;
        while (instr) {
            if (instr->opcode == IR_ADD && 
                instr->operands[0].type == IR_OP_IMM &&
                instr->operands[1].type == IR_OP_IMM) {
                /* Fold constant addition */
                int64_t result = instr->operands[0].value.imm + instr->operands[1].value.imm;
                instr->opcode = IR_MOV;
                instr->operands[0].type = IR_OP_IMM;
                instr->operands[0].value.imm = result;
                instr->operands[1].type = IR_OP_NONE;
            }
            instr = instr->next;
        }
        block = block->next;
    }
    
    /* Dead code elimination */
    /* Would remove unused instructions */
    
    return 0;
}

/* Free IR function */
void ir_free_function(struct ir_function* func) {
    if (!func) {
        return;
    }
    
    if (func->name) {
        kfree(func->name);
    }
    
    struct ir_basic_block* block = func->blocks;
    while (block) {
        struct ir_instruction* instr = block->instructions;
        while (instr) {
            struct ir_instruction* next = instr->next;
            kfree(instr);
            instr = next;
        }
        
        if (block->successors) {
            kfree(block->successors);
        }
        if (block->predecessors) {
            kfree(block->predecessors);
        }
        
        struct ir_basic_block* next_block = block->next;
        kfree(block);
        block = next_block;
    }
    
    kfree(func);
}
