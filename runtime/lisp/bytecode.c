/* AstraLisp OS Bytecode Implementation
 * 
 * Bytecode chunk management, emission, and disassembly.
 */

#include "bytecode.h"
#include "../gc/gc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Kernel compatibility */
#ifdef KERNEL
extern void* kmalloc(size_t size);
extern void kfree(void* ptr);
#else
#define kmalloc malloc
#define kfree free
#endif

/* ========== Opcode Names ========== */

static const char* opcode_names[] = {
    [BC_NOP]            = "NOP",
    [BC_POP]            = "POP",
    [BC_DUP]            = "DUP",
    [BC_SWAP]           = "SWAP",
    [BC_ROT]            = "ROT",
    [BC_PUSH_NIL]       = "PUSH_NIL",
    [BC_PUSH_T]         = "PUSH_T",
    [BC_PUSH_FIXNUM]    = "PUSH_FIXNUM",
    [BC_PUSH_CONST]     = "PUSH_CONST",
    [BC_PUSH_CHAR]      = "PUSH_CHAR",
    [BC_LOAD_LOCAL]     = "LOAD_LOCAL",
    [BC_STORE_LOCAL]    = "STORE_LOCAL",
    [BC_LOAD_LOCAL_W]   = "LOAD_LOCAL_W",
    [BC_STORE_LOCAL_W]  = "STORE_LOCAL_W",
    [BC_LOAD_UPVALUE]   = "LOAD_UPVALUE",
    [BC_STORE_UPVALUE]  = "STORE_UPVALUE",
    [BC_CLOSE_UPVALUE]  = "CLOSE_UPVALUE",
    [BC_LOAD_GLOBAL]    = "LOAD_GLOBAL",
    [BC_STORE_GLOBAL]   = "STORE_GLOBAL",
    [BC_DEFINE_GLOBAL]  = "DEFINE_GLOBAL",
    [BC_ADD]            = "ADD",
    [BC_SUB]            = "SUB",
    [BC_MUL]            = "MUL",
    [BC_DIV]            = "DIV",
    [BC_MOD]            = "MOD",
    [BC_NEG]            = "NEG",
    [BC_AND]            = "AND",
    [BC_OR]             = "OR",
    [BC_XOR]            = "XOR",
    [BC_NOT]            = "NOT",
    [BC_SHL]            = "SHL",
    [BC_SHR]            = "SHR",
    [BC_EQ]             = "EQ",
    [BC_EQUAL]          = "EQUAL",
    [BC_LT]             = "LT",
    [BC_LE]             = "LE",
    [BC_GT]             = "GT",
    [BC_GE]             = "GE",
    [BC_ZEROP]          = "ZEROP",
    [BC_NULLP]          = "NULLP",
    [BC_JUMP]           = "JUMP",
    [BC_JUMP_IF_NIL]    = "JUMP_IF_NIL",
    [BC_JUMP_IF_NOT_NIL]= "JUMP_IF_NOT_NIL",
    [BC_JUMP_IF_FALSE]  = "JUMP_IF_FALSE",
    [BC_JUMP_W]         = "JUMP_W",
    [BC_CALL]           = "CALL",
    [BC_CALL_W]         = "CALL_W",
    [BC_TAIL_CALL]      = "TAIL_CALL",
    [BC_RETURN]         = "RETURN",
    [BC_APPLY]          = "APPLY",
    [BC_MAKE_CLOSURE]   = "MAKE_CLOSURE",
    [BC_CAPTURE_LOCAL]  = "CAPTURE_LOCAL",
    [BC_CAPTURE_UPVALUE]= "CAPTURE_UPVALUE",
    [BC_CONS]           = "CONS",
    [BC_CAR]            = "CAR",
    [BC_CDR]            = "CDR",
    [BC_SET_CAR]        = "SET_CAR",
    [BC_SET_CDR]        = "SET_CDR",
    [BC_LIST]           = "LIST",
    [BC_CONSP]          = "CONSP",
    [BC_SYMBOLP]        = "SYMBOLP",
    [BC_NUMBERP]        = "NUMBERP",
    [BC_STRINGP]        = "STRINGP",
    [BC_FUNCTIONP]      = "FUNCTIONP",
    [BC_ATOMP]          = "ATOMP",
    [BC_PUSH_HANDLER]   = "PUSH_HANDLER",
    [BC_POP_HANDLER]    = "POP_HANDLER",
    [BC_THROW]          = "THROW",
    [BC_DEBUG_PRINT]    = "DEBUG_PRINT",
    [BC_BREAKPOINT]     = "BREAKPOINT",
    [BC_HALT]           = "HALT"
};

const char* bc_opcode_name(bc_opcode_t op) {
    if (op < sizeof(opcode_names)/sizeof(opcode_names[0]) && opcode_names[op]) {
        return opcode_names[op];
    }
    return "UNKNOWN";
}

/* ========== Chunk Creation ========== */

struct bc_chunk* bc_chunk_create(void) {
    struct bc_chunk* chunk = (struct bc_chunk*)kmalloc(sizeof(struct bc_chunk));
    if (!chunk) return NULL;
    
    memset(chunk, 0, sizeof(struct bc_chunk));
    return chunk;
}

void bc_chunk_free(struct bc_chunk* chunk) {
    if (!chunk) return;
    
    for (size_t i = 0; i < chunk->function_count; i++) {
        bc_function_free(chunk->functions[i]);
    }
    kfree(chunk->functions);
    
    for (size_t i = 0; i < chunk->string_count; i++) {
        kfree(chunk->strings[i]);
    }
    kfree(chunk->strings);
    
    kfree(chunk);
}

/* ========== Function Creation ========== */

#define INITIAL_CODE_CAPACITY 256
#define INITIAL_CONST_CAPACITY 16

struct bc_function* bc_function_create(const char* name, uint8_t arity) {
    struct bc_function* func = (struct bc_function*)kmalloc(sizeof(struct bc_function));
    if (!func) return NULL;
    
    memset(func, 0, sizeof(struct bc_function));
    
    if (name) {
        size_t len = strlen(name);
        func->name = (char*)kmalloc(len + 1);
        if (func->name) {
            memcpy(func->name, name, len + 1);
        }
    }
    
    func->arity = arity;
    
    /* Allocate initial code buffer */
    func->code = (uint8_t*)kmalloc(INITIAL_CODE_CAPACITY);
    if (!func->code) {
        kfree(func->name);
        kfree(func);
        return NULL;
    }
    
    /* Allocate initial constant pool */
    func->constants = (lisp_value*)kmalloc(INITIAL_CONST_CAPACITY * sizeof(lisp_value));
    if (!func->constants) {
        kfree(func->code);
        kfree(func->name);
        kfree(func);
        return NULL;
    }
    
    return func;
}

void bc_function_free(struct bc_function* func) {
    if (!func) return;
    
    kfree(func->name);
    kfree(func->code);
    kfree(func->constants);
    kfree(func->upvalues);
    kfree(func->lines);
    kfree(func->source_file);
    kfree(func);
}

/* ========== Bytecode Emission ========== */

static void ensure_code_capacity(struct bc_function* func, size_t additional) {
    size_t capacity = INITIAL_CODE_CAPACITY;
    while (capacity < func->code_length + additional) {
        capacity *= 2;
    }
    
    if (capacity > INITIAL_CODE_CAPACITY) {
        uint8_t* new_code = (uint8_t*)kmalloc(capacity);
        if (new_code) {
            memcpy(new_code, func->code, func->code_length);
            kfree(func->code);
            func->code = new_code;
        }
    }
}

void bc_emit(struct bc_function* func, uint8_t byte) {
    ensure_code_capacity(func, 1);
    func->code[func->code_length++] = byte;
}

void bc_emit_op(struct bc_function* func, bc_opcode_t op) {
    bc_emit(func, (uint8_t)op);
}

void bc_emit_byte(struct bc_function* func, uint8_t byte) {
    bc_emit(func, byte);
}

void bc_emit_short(struct bc_function* func, uint16_t value) {
    bc_emit(func, (value >> 8) & 0xFF);  /* Big-endian */
    bc_emit(func, value & 0xFF);
}

void bc_emit_int(struct bc_function* func, uint32_t value) {
    bc_emit(func, (value >> 24) & 0xFF);
    bc_emit(func, (value >> 16) & 0xFF);
    bc_emit(func, (value >> 8) & 0xFF);
    bc_emit(func, value & 0xFF);
}

void bc_emit_long(struct bc_function* func, int64_t value) {
    for (int i = 7; i >= 0; i--) {
        bc_emit(func, (value >> (i * 8)) & 0xFF);
    }
}

/* ========== Constant Pool ========== */

uint16_t bc_add_constant(struct bc_function* func, lisp_value value) {
    /* Check if constant already exists */
    for (size_t i = 0; i < func->constant_count; i++) {
        if (func->constants[i] == value) {
            return (uint16_t)i;
        }
    }
    
    /* Grow constant pool if needed */
    size_t capacity = INITIAL_CONST_CAPACITY;
    while (capacity <= func->constant_count) {
        capacity *= 2;
    }
    
    if (capacity > INITIAL_CONST_CAPACITY && func->constant_count >= INITIAL_CONST_CAPACITY) {
        lisp_value* new_consts = (lisp_value*)kmalloc(capacity * sizeof(lisp_value));
        if (new_consts) {
            memcpy(new_consts, func->constants, func->constant_count * sizeof(lisp_value));
            kfree(func->constants);
            func->constants = new_consts;
        }
    }
    
    uint16_t idx = (uint16_t)func->constant_count;
    func->constants[func->constant_count++] = value;
    return idx;
}

/* ========== Debug Info ========== */

void bc_add_line(struct bc_function* func, uint32_t line) {
    /* Check if last entry has same line */
    if (func->line_count > 0 && 
        func->lines[func->line_count - 1].line == line) {
        return; /* Same line, no new entry needed */
    }
    
    /* Grow line table */
    size_t new_count = func->line_count + 1;
    struct bc_line_info* new_lines = (struct bc_line_info*)kmalloc(
        new_count * sizeof(struct bc_line_info));
    
    if (new_lines) {
        if (func->lines) {
            memcpy(new_lines, func->lines, func->line_count * sizeof(struct bc_line_info));
            kfree(func->lines);
        }
        func->lines = new_lines;
        func->lines[func->line_count].offset = (uint32_t)func->code_length;
        func->lines[func->line_count].line = line;
        func->line_count = new_count;
    }
}

/* ========== Jump Patching ========== */

void bc_patch_jump(struct bc_function* func, size_t offset) {
    /* Calculate jump distance from offset+2 to current position */
    int16_t jump = (int16_t)(func->code_length - offset - 2);
    func->code[offset] = (jump >> 8) & 0xFF;
    func->code[offset + 1] = jump & 0xFF;
}

size_t bc_current_offset(struct bc_function* func) {
    return func->code_length;
}

/* ========== Disassembly ========== */

static uint32_t get_line(struct bc_function* func, size_t offset) {
    if (!func->lines || func->line_count == 0) return 0;
    
    for (size_t i = func->line_count; i > 0; i--) {
        if (func->lines[i-1].offset <= offset) {
            return func->lines[i-1].line;
        }
    }
    return 0;
}

void bc_disassemble(struct bc_function* func) {
    printf("== %s ==\n", func->name ? func->name : "<anonymous>");
    printf("arity: %d, locals: %d, upvalues: %zu\n", 
           func->arity, func->local_count, func->upvalue_count);
    printf("constants: %zu, code: %zu bytes\n", 
           func->constant_count, func->code_length);
    printf("\n");
    
    size_t offset = 0;
    while (offset < func->code_length) {
        bc_disassemble_instruction(func, offset);
        
        /* Advance based on opcode */
        uint8_t op = func->code[offset];
        offset++;
        
        switch ((bc_opcode_t)op) {
            case BC_PUSH_FIXNUM:
                offset += 8; break;
            case BC_PUSH_CONST:
            case BC_LOAD_LOCAL_W:
            case BC_STORE_LOCAL_W:
            case BC_LOAD_GLOBAL:
            case BC_STORE_GLOBAL:
            case BC_DEFINE_GLOBAL:
            case BC_JUMP:
            case BC_JUMP_IF_NIL:
            case BC_JUMP_IF_NOT_NIL:
            case BC_JUMP_IF_FALSE:
            case BC_MAKE_CLOSURE:
            case BC_CALL_W:
            case BC_PUSH_HANDLER:
                offset += 2; break;
            case BC_PUSH_CHAR:
            case BC_JUMP_W:
                offset += 4; break;
            case BC_LOAD_LOCAL:
            case BC_STORE_LOCAL:
            case BC_LOAD_UPVALUE:
            case BC_STORE_UPVALUE:
            case BC_CALL:
            case BC_TAIL_CALL:
            case BC_CAPTURE_LOCAL:
            case BC_CAPTURE_UPVALUE:
            case BC_LIST:
                offset += 1; break;
            default:
                break; /* No operands */
        }
    }
}

void bc_disassemble_instruction(struct bc_function* func, size_t offset) {
    uint32_t line = get_line(func, offset);
    printf("%04zx  ", offset);
    
    if (line > 0) {
        printf("%4d | ", line);
    } else {
        printf("     | ");
    }
    
    uint8_t op = func->code[offset];
    printf("%-18s", bc_opcode_name((bc_opcode_t)op));
    
    switch ((bc_opcode_t)op) {
        case BC_PUSH_FIXNUM: {
            int64_t val = 0;
            for (int i = 0; i < 8; i++) {
                val = (val << 8) | func->code[offset + 1 + i];
            }
            printf(" %lld", (long long)val);
            break;
        }
        case BC_PUSH_CONST: {
            uint16_t idx = (func->code[offset + 1] << 8) | func->code[offset + 2];
            printf(" @%d", idx);
            break;
        }
        case BC_LOAD_LOCAL:
        case BC_STORE_LOCAL:
        case BC_LOAD_UPVALUE:
        case BC_STORE_UPVALUE:
        case BC_CAPTURE_LOCAL:
        case BC_CAPTURE_UPVALUE:
        case BC_CALL:
        case BC_TAIL_CALL:
        case BC_LIST: {
            uint8_t val = func->code[offset + 1];
            printf(" %d", val);
            break;
        }
        case BC_LOAD_LOCAL_W:
        case BC_STORE_LOCAL_W: {
            uint16_t idx = (func->code[offset + 1] << 8) | func->code[offset + 2];
            printf(" %d", idx);
            break;
        }
        case BC_LOAD_GLOBAL:
        case BC_STORE_GLOBAL:
        case BC_DEFINE_GLOBAL: {
            uint16_t idx = (func->code[offset + 1] << 8) | func->code[offset + 2];
            printf(" @%d", idx);
            break;
        }
        case BC_JUMP:
        case BC_JUMP_IF_NIL:
        case BC_JUMP_IF_NOT_NIL:
        case BC_JUMP_IF_FALSE:
        case BC_PUSH_HANDLER: {
            int16_t off = (int16_t)((func->code[offset + 1] << 8) | func->code[offset + 2]);
            printf(" -> %04zx", offset + 3 + off);
            break;
        }
        case BC_JUMP_W: {
            int32_t off = (func->code[offset + 1] << 24) | 
                          (func->code[offset + 2] << 16) |
                          (func->code[offset + 3] << 8) | 
                          func->code[offset + 4];
            printf(" -> %04zx", offset + 5 + off);
            break;
        }
        case BC_MAKE_CLOSURE: {
            uint16_t idx = (func->code[offset + 1] << 8) | func->code[offset + 2];
            printf(" func@%d", idx);
            break;
        }
        default:
            break;
    }
    
    printf("\n");
}
