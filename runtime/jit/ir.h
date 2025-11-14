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
    IR_ALLOCA,
    IR_LOADARG,
    IR_RETURN
} ir_opcode_t;

/* IR operand types */
typedef enum {
    IR_OP_REG,
    IR_OP_IMM,
    IR_OP_MEM,
    IR_OP_LABEL
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
    uint32_t result_reg;
    uint32_t label_id;
    struct ir_instruction* next;
    struct ir_instruction* prev;
};

/* IR basic block */
struct ir_block {
    uint32_t block_id;
    struct ir_instruction* instructions;
    struct ir_block** predecessors;
    uint32_t predecessor_count;
    struct ir_block** successors;
    uint32_t successor_count;
    uint32_t live_in[64];
    uint32_t live_out[64];
    bool visited;
    struct ir_block* next;
};

/* IR function */
struct ir_function {
    char* name;
    uint32_t arg_count;
    uint32_t local_count;
    struct ir_block* entry_block;
    struct ir_block* blocks;
    uint32_t block_count;
    uint32_t next_reg;
    uint32_t next_label;
};

/* Create IR function */
struct ir_function* ir_create_function(const char* name, uint32_t arg_count);

/* Add instruction to block */
struct ir_instruction* ir_add_instruction(struct ir_block* block, ir_opcode_t opcode,
                                           const struct ir_operand* ops, uint32_t result_reg);

/* Create basic block */
struct ir_block* ir_create_block(struct ir_function* func);

/* Add edge between blocks */
void ir_add_edge(struct ir_block* from, struct ir_block* to);

/* Optimize IR */
int ir_optimize(struct ir_function* func);

/* Free IR function */
void ir_free_function(struct ir_function* func);

#endif /* IR_H */
