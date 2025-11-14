/* AstraLisp OS JIT Compiler - Intermediate Representation */

#ifndef IR_H
#define IR_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* IR instruction types */
typedef enum {
    IR_NOP,
    IR_MOV,
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_MOD,
    IR_AND,
    IR_OR,
    IR_XOR,
    IR_SHL,
    IR_SHR,
    IR_CMP,
    IR_JMP,
    IR_JZ,
    IR_JNZ,
    IR_CALL,
    IR_RET,
    IR_LOAD,
    IR_STORE,
    IR_PHI,
    IR_ALLOCA
} ir_opcode_t;

/* IR operand types */
typedef enum {
    IR_OP_REG,
    IR_OP_IMM,
    IR_OP_MEM,
    IR_OP_LABEL,
    IR_OP_NONE
} ir_operand_type_t;

/* IR operand */
struct ir_operand {
    ir_operand_type_t type;
    union {
        uint32_t reg;
        int64_t imm;
        struct {
            uint32_t base_reg;
            int32_t offset;
        } mem;
        uint32_t label;
    } value;
};

/* IR instruction */
struct ir_instruction {
    ir_opcode_t opcode;
    struct ir_operand operands[3];
    uint32_t label;
    struct ir_instruction* next;
    struct ir_instruction* prev;
};

/* IR basic block */
struct ir_basic_block {
    uint32_t block_id;
    struct ir_instruction* instructions;
    struct ir_basic_block** successors;
    uint32_t successor_count;
    struct ir_basic_block** predecessors;
    uint32_t predecessor_count;
    bool visited;
};

/* IR function */
struct ir_function {
    char* name;
    uint32_t arg_count;
    uint32_t local_count;
    struct ir_basic_block* entry_block;
    struct ir_basic_block* blocks;
    uint32_t block_count;
    uint32_t next_label;
    struct ir_basic_block* next;
};

/* Create IR function */
struct ir_function* ir_create_function(const char* name, uint32_t arg_count);

/* Add instruction to basic block */
struct ir_instruction* ir_add_instruction(struct ir_basic_block* block, ir_opcode_t opcode,
                                          struct ir_operand* op1, struct ir_operand* op2, struct ir_operand* op3);

/* Create basic block */
struct ir_basic_block* ir_create_basic_block(struct ir_function* func);

/* Optimize IR function */
int ir_optimize(struct ir_function* func);

/* Free IR function */
void ir_free_function(struct ir_function* func);

#endif /* IR_H */
