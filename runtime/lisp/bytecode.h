/* AstraLisp OS Bytecode Instruction Set
 * 
 * Stack-based bytecode VM for fast interpreted execution.
 * Complements the JIT compiler for hot code paths.
 */

#ifndef BYTECODE_H
#define BYTECODE_H

#include "tagged.h"
#include <stdint.h>
#include <stddef.h>

/* ========== Bytecode Opcodes ========== */

typedef enum {
    /* Stack manipulation */
    BC_NOP          = 0x00,  /* No operation */
    BC_POP          = 0x01,  /* Discard top of stack */
    BC_DUP          = 0x02,  /* Duplicate top of stack */
    BC_SWAP         = 0x03,  /* Swap top two stack values */
    BC_ROT          = 0x04,  /* Rotate top three: (a b c) -> (b c a) */
    
    /* Constants and immediates */
    BC_PUSH_NIL     = 0x10,  /* Push NIL */
    BC_PUSH_T       = 0x11,  /* Push T */
    BC_PUSH_FIXNUM  = 0x12,  /* Push 64-bit fixnum (8-byte operand) */
    BC_PUSH_CONST   = 0x13,  /* Push constant from pool (2-byte index) */
    BC_PUSH_CHAR    = 0x14,  /* Push character (4-byte operand) */
    
    /* Local variable access */
    BC_LOAD_LOCAL   = 0x20,  /* Load local variable (1-byte index) */
    BC_STORE_LOCAL  = 0x21,  /* Store to local variable (1-byte index) */
    BC_LOAD_LOCAL_W = 0x22,  /* Load local variable (2-byte index) */
    BC_STORE_LOCAL_W= 0x23,  /* Store to local variable (2-byte index) */
    
    /* Upvalue/closure access */
    BC_LOAD_UPVALUE = 0x24,  /* Load upvalue (1-byte index) */
    BC_STORE_UPVALUE= 0x25,  /* Store to upvalue (1-byte index) */
    BC_CLOSE_UPVALUE= 0x26,  /* Close upvalue on stack */
    
    /* Global variable access */
    BC_LOAD_GLOBAL  = 0x28,  /* Load global (2-byte constant index for symbol) */
    BC_STORE_GLOBAL = 0x29,  /* Store to global (2-byte constant index) */
    BC_DEFINE_GLOBAL= 0x2A,  /* Define global (2-byte constant index) */
    
    /* Arithmetic operations (pop 2, push 1) */
    BC_ADD          = 0x30,  /* a + b */
    BC_SUB          = 0x31,  /* a - b */
    BC_MUL          = 0x32,  /* a * b */
    BC_DIV          = 0x33,  /* a / b */
    BC_MOD          = 0x34,  /* a % b */
    BC_NEG          = 0x35,  /* -a (unary) */
    
    /* Bitwise operations */
    BC_AND          = 0x38,  /* a & b */
    BC_OR           = 0x39,  /* a | b */
    BC_XOR          = 0x3A,  /* a ^ b */
    BC_NOT          = 0x3B,  /* ~a */
    BC_SHL          = 0x3C,  /* a << b */
    BC_SHR          = 0x3D,  /* a >> b (arithmetic) */
    
    /* Comparison operations (pop 2, push T/NIL) */
    BC_EQ           = 0x40,  /* a == b (eq) */
    BC_EQUAL        = 0x41,  /* a equal b (structural) */
    BC_LT           = 0x42,  /* a < b */
    BC_LE           = 0x43,  /* a <= b */
    BC_GT           = 0x44,  /* a > b */
    BC_GE           = 0x45,  /* a >= b */
    BC_ZEROP        = 0x46,  /* a == 0 */
    BC_NULLP        = 0x47,  /* a is NIL */
    
    /* Control flow */
    BC_JUMP         = 0x50,  /* Unconditional jump (2-byte signed offset) */
    BC_JUMP_IF_NIL  = 0x51,  /* Jump if top is NIL (pop, 2-byte offset) */
    BC_JUMP_IF_NOT_NIL = 0x52, /* Jump if top is not NIL (pop, 2-byte offset) */
    BC_JUMP_IF_FALSE= 0x53,  /* Jump if NIL (no pop) */
    BC_JUMP_W       = 0x54,  /* Wide jump (4-byte signed offset) */
    
    /* Function calls */
    BC_CALL         = 0x60,  /* Call function (1-byte argc) */
    BC_CALL_W       = 0x61,  /* Call function (2-byte argc) */
    BC_TAIL_CALL    = 0x62,  /* Tail call optimization (1-byte argc) */
    BC_RETURN       = 0x63,  /* Return from function */
    BC_APPLY        = 0x64,  /* Apply function to list */
    
    /* Closure creation */
    BC_MAKE_CLOSURE = 0x68,  /* Create closure (2-byte function index) */
    BC_CAPTURE_LOCAL= 0x69,  /* Capture local as upvalue (1-byte local idx) */
    BC_CAPTURE_UPVALUE = 0x6A, /* Capture upvalue (1-byte upvalue idx) */
    
    /* List operations */
    BC_CONS         = 0x70,  /* (cons a b) - pop 2, push cons */
    BC_CAR          = 0x71,  /* (car a) - pop 1, push car */
    BC_CDR          = 0x72,  /* (cdr a) - pop 1, push cdr */
    BC_SET_CAR      = 0x73,  /* (set-car! pair val) */
    BC_SET_CDR      = 0x74,  /* (set-cdr! pair val) */
    BC_LIST         = 0x75,  /* Create list (1-byte count) */
    
    /* Type checks (pop 1, push T/NIL) */
    BC_CONSP        = 0x78,  /* Is cons? */
    BC_SYMBOLP      = 0x79,  /* Is symbol? */
    BC_NUMBERP      = 0x7A,  /* Is fixnum? */
    BC_STRINGP      = 0x7B,  /* Is string? */
    BC_FUNCTIONP    = 0x7C,  /* Is function? */
    BC_ATOMP        = 0x7D,  /* Is atom (not cons)? */
    
    /* Error handling */
    BC_PUSH_HANDLER = 0x80,  /* Push error handler (2-byte offset to handler) */
    BC_POP_HANDLER  = 0x81,  /* Pop error handler */
    BC_THROW        = 0x82,  /* Throw error (top of stack is error value) */
    
    /* Debugging */
    BC_DEBUG_PRINT  = 0xF0,  /* Print top of stack (for debugging) */
    BC_BREAKPOINT   = 0xF1,  /* Debugger breakpoint */
    
    /* Sentinel */
    BC_HALT         = 0xFF   /* Stop VM execution */
} bc_opcode_t;

/* ========== Bytecode Chunk ========== */

/* Debug information for a bytecode range */
struct bc_line_info {
    uint32_t offset;        /* Bytecode offset */
    uint32_t line;          /* Source line number */
};

/* Upvalue descriptor for closures */
struct bc_upvalue_desc {
    uint8_t is_local;       /* 1 if captures local, 0 if captures upvalue */
    uint8_t index;          /* Index of local or upvalue to capture */
};

/* Function prototype (template for closures) */
struct bc_function {
    char* name;             /* Function name (for debugging) */
    uint8_t* code;          /* Bytecode instructions */
    size_t code_length;     /* Length of bytecode */
    lisp_value* constants;  /* Constant pool */
    size_t constant_count;  /* Number of constants */
    struct bc_upvalue_desc* upvalues; /* Upvalue descriptors */
    size_t upvalue_count;   /* Number of upvalues */
    uint8_t arity;          /* Expected argument count */
    uint8_t local_count;    /* Number of local variables */
    bool is_variadic;       /* Accepts rest args? */
    
    /* Debug info */
    struct bc_line_info* lines;
    size_t line_count;
    char* source_file;
};

/* Bytecode chunk - compilation unit */
struct bc_chunk {
    struct bc_function* main_function;  /* Entry function */
    struct bc_function** functions;     /* All function prototypes */
    size_t function_count;
    
    /* String table for symbol names */
    char** strings;
    size_t string_count;
};

/* ========== Bytecode API ========== */

/* Create empty bytecode chunk */
struct bc_chunk* bc_chunk_create(void);

/* Free bytecode chunk */
void bc_chunk_free(struct bc_chunk* chunk);

/* Create function prototype */
struct bc_function* bc_function_create(const char* name, uint8_t arity);

/* Free function prototype */
void bc_function_free(struct bc_function* func);

/* Emit bytecode to function */
void bc_emit(struct bc_function* func, uint8_t byte);
void bc_emit_op(struct bc_function* func, bc_opcode_t op);
void bc_emit_byte(struct bc_function* func, uint8_t byte);
void bc_emit_short(struct bc_function* func, uint16_t value);
void bc_emit_int(struct bc_function* func, uint32_t value);
void bc_emit_long(struct bc_function* func, int64_t value);

/* Add constant to function, return index */
uint16_t bc_add_constant(struct bc_function* func, lisp_value value);

/* Add debug line info */
void bc_add_line(struct bc_function* func, uint32_t line);

/* Patch jump offset at given bytecode position */
void bc_patch_jump(struct bc_function* func, size_t offset);

/* Get current bytecode offset */
size_t bc_current_offset(struct bc_function* func);

/* Disassemble bytecode for debugging */
void bc_disassemble(struct bc_function* func);
void bc_disassemble_instruction(struct bc_function* func, size_t offset);

/* Get opcode name */
const char* bc_opcode_name(bc_opcode_t op);

#endif /* BYTECODE_H */
