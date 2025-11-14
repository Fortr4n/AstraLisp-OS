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
    func->next_reg = arg_count;
    func->next_label = 1;
    
    return func;
}

/* Create basic block */
struct ir_block* ir_create_block(struct ir_function* func) {
    if (!func) {
        return NULL;
    }
    
    struct ir_block* block = (struct ir_block*)kmalloc(sizeof(struct ir_block));
    if (!block) {
        return NULL;
    }
    
    memset(block, 0, sizeof(struct ir_block));
    block->block_id = func->block_count++;
    block->instructions = NULL;
    block->predecessors = NULL;
    block->predecessor_count = 0;
    block->successors = NULL;
    block->successor_count = 0;
    block->visited = false;
    
    /* Add to function */
    block->next = func->blocks;
    func->blocks = block;
    
    if (!func->entry_block) {
        func->entry_block = block;
    }
    
    return block;
}

/* Add instruction to block */
struct ir_instruction* ir_add_instruction(struct ir_block* block, ir_opcode_t opcode,
                                           const struct ir_operand* ops, uint32_t result_reg) {
    if (!block) {
        return NULL;
    }
    
    struct ir_instruction* inst = (struct ir_instruction*)kmalloc(sizeof(struct ir_instruction));
    if (!inst) {
        return NULL;
    }
    
    inst->opcode = opcode;
    inst->result_reg = result_reg;
    inst->label_id = 0;
    inst->next = NULL;
    inst->prev = NULL;
    
    if (ops) {
        for (int i = 0; i < 3; i++) {
            inst->operands[i] = ops[i];
        }
    } else {
        memset(inst->operands, 0, sizeof(inst->operands));
    }
    
    /* Add to block */
    if (!block->instructions) {
        block->instructions = inst;
    } else {
        struct ir_instruction* last = block->instructions;
        while (last->next) {
            last = last->next;
        }
        last->next = inst;
        inst->prev = last;
    }
    
    return inst;
}

/* Add edge between blocks */
void ir_add_edge(struct ir_block* from, struct ir_block* to) {
    if (!from || !to) {
        return;
    }
    
    /* Add to successors of from */
    for (uint32_t i = 0; i < from->successor_count; i++) {
        if (from->successors[i] == to) {
            return;  /* Already added */
        }
    }
    
    from->successors = (struct ir_block**)krealloc(from->successors,
                                                    sizeof(struct ir_block*) * (from->successor_count + 1));
    if (from->successors) {
        from->successors[from->successor_count++] = to;
    }
    
    /* Add to predecessors of to */
    to->predecessors = (struct ir_block**)krealloc(to->predecessors,
                                                  sizeof(struct ir_block*) * (to->predecessor_count + 1));
    if (to->predecessors) {
        to->predecessors[to->predecessor_count++] = from;
    }
}

/* Dead code elimination */
static void ir_eliminate_dead_code(struct ir_function* func) {
    if (!func) {
        return;
    }
    
    /* Mark all registers as unused */
    bool* used = (bool*)kmalloc(sizeof(bool) * 256);
    if (!used) {
        return;
    }
    memset(used, 0, sizeof(bool) * 256);
    
    /* Mark argument registers as used */
    for (uint32_t i = 0; i < func->arg_count; i++) {
        used[i] = true;
    }
    
    /* Traverse blocks in reverse order */
    struct ir_block* block = func->blocks;
    while (block) {
        struct ir_instruction* inst = block->instructions;
        while (inst) {
            struct ir_instruction* next = inst->next;
            
            /* Check if result is used */
            if (inst->result_reg != 0 && !used[inst->result_reg]) {
                /* Remove instruction */
                if (inst->prev) {
                    inst->prev->next = inst->next;
                } else {
                    block->instructions = inst->next;
                }
                if (inst->next) {
                    inst->next->prev = inst->prev;
                }
                kfree(inst);
            } else {
                /* Mark operands as used */
                for (int i = 0; i < 3; i++) {
                    if (inst->operands[i].type == IR_OP_REG) {
                        used[inst->operands[i].value.reg] = true;
                    }
                }
            }
            
            inst = next;
        }
        
        block = block->next;
    }
    
    kfree(used);
}

/* Constant folding */
static void ir_constant_fold(struct ir_function* func) {
    if (!func) {
        return;
    }
    
    struct ir_block* block = func->blocks;
    while (block) {
        struct ir_instruction* inst = block->instructions;
        while (inst) {
            /* Try to fold binary operations with constants */
            if (inst->opcode == IR_ADD || inst->opcode == IR_SUB ||
                inst->opcode == IR_MUL || inst->opcode == IR_DIV) {
                if (inst->operands[0].type == IR_OP_IMM &&
                    inst->operands[1].type == IR_OP_IMM) {
                    int64_t result = 0;
                    int64_t a = inst->operands[0].value.imm;
                    int64_t b = inst->operands[1].value.imm;
                    
                    switch (inst->opcode) {
                        case IR_ADD: result = a + b; break;
                        case IR_SUB: result = a - b; break;
                        case IR_MUL: result = a * b; break;
                        case IR_DIV: if (b != 0) result = a / b; break;
                        default: break;
                    }
                    
                    /* Replace with MOV immediate */
                    inst->opcode = IR_MOV;
                    inst->operands[0].type = IR_OP_IMM;
                    inst->operands[0].value.imm = result;
                    inst->operands[1].type = IR_OP_REG;
                    inst->operands[1].value.reg = 0;
                    inst->operands[2].type = IR_OP_REG;
                    inst->operands[2].value.reg = 0;
                }
            }
            
            inst = inst->next;
        }
        
        block = block->next;
    }
}

/* Optimize IR */
int ir_optimize(struct ir_function* func) {
    if (!func) {
        return -1;
    }
    
    /* Run optimization passes */
    ir_constant_fold(func);
    ir_eliminate_dead_code(func);
    
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
    
    struct ir_block* block = func->blocks;
    while (block) {
        struct ir_block* next_block = block->next;
        
        struct ir_instruction* inst = block->instructions;
        while (inst) {
            struct ir_instruction* next_inst = inst->next;
            kfree(inst);
            inst = next_inst;
        }
        
        if (block->predecessors) {
            kfree(block->predecessors);
        }
        if (block->successors) {
            kfree(block->successors);
        }
        
        kfree(block);
        block = next_block;
    }
    
    kfree(func);
}
