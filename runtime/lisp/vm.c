/* AstraLisp OS Virtual Machine Implementation
 * 
 * Stack-based bytecode execution with threaded dispatch.
 */

#include "vm.h"
#include "bytecode.h"
#include "reader.h"
#include "symbols.h"
#include "types.h"
#include "evaluator.h"
#include "../gc/gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* Kernel compatibility */
#ifdef KERNEL
extern void* kmalloc(size_t size);
extern void kfree(void* ptr);
#else
#define kmalloc malloc
#define kfree free
#endif

/* ========== Helper Macros ========== */

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() (frame->ip += 2, ((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_INT() (frame->ip += 4, ((frame->ip[-4] << 24) | (frame->ip[-3] << 16) | (frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_LONG() read_long(&frame->ip)

#define PUSH(val) do { *vm->stack_top++ = (val); } while(0)
#define POP() (*(--vm->stack_top))
#define PEEK(n) (vm->stack_top[-1 - (n)])
#define DROP(n) (vm->stack_top -= (n))

#define CURRENT_FRAME() (&vm->frames[vm->frame_count - 1])

/* Read 64-bit value from bytecode */
static int64_t read_long(uint8_t** ip) {
    int64_t val = 0;
    for (int i = 0; i < 8; i++) {
        val = (val << 8) | (*(*ip)++);
    }
    return val;
}

/* ========== VM Initialization ========== */

int vm_init(struct vm_context* vm) {
    if (!vm) return -1;
    
    memset(vm, 0, sizeof(struct vm_context));
    vm->stack_top = vm->stack;
    vm->frame_count = 0;
    vm->open_upvalues = NULL;
    vm->handler_stack = NULL;
    vm->next_gc = 1024 * 1024; /* 1MB initial threshold */
    
    /* Create global environment */
    vm->globals = lisp_env_create(LISP_NIL);
    gc_add_root(&vm->globals);
    
    return 0;
}

void vm_free(struct vm_context* vm) {
    if (!vm) return;
    
    /* Free open upvalues */
    struct vm_upvalue* upvalue = vm->open_upvalues;
    while (upvalue) {
        struct vm_upvalue* next = upvalue->next;
        kfree(upvalue);
        upvalue = next;
    }
    
    /* Free error handlers */
    struct vm_handler* handler = vm->handler_stack;
    while (handler) {
        struct vm_handler* prev = handler->prev;
        kfree(handler);
        handler = prev;
    }
    
    if (vm->last_error) kfree(vm->last_error);
    
    gc_remove_root(&vm->globals);
}

/* ========== Stack Operations ========== */

void vm_push(struct vm_context* vm, lisp_value value) {
    if (vm->stack_top >= vm->stack + VM_STACK_SIZE) {
        vm_error(vm, "Stack overflow");
        return;
    }
    *vm->stack_top++ = value;
}

lisp_value vm_pop(struct vm_context* vm) {
    if (vm->stack_top <= vm->stack) {
        vm_error(vm, "Stack underflow");
        return LISP_NIL;
    }
    return *(--vm->stack_top);
}

lisp_value vm_peek(struct vm_context* vm, int distance) {
    return vm->stack_top[-1 - distance];
}

lisp_value vm_get_result(struct vm_context* vm) {
    if (vm->stack_top > vm->stack) {
        return vm->stack_top[-1];
    }
    return LISP_NIL;
}

/* ========== Error Handling ========== */

void vm_error(struct vm_context* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    
    char buffer[256];
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    
    if (vm->last_error) kfree(vm->last_error);
    vm->last_error = (char*)kmalloc(strlen(buffer) + 1);
    if (vm->last_error) {
        strcpy(vm->last_error, buffer);
    }
    
    fprintf(stderr, "VM Error: %s\n", buffer);
}

/* ========== Upvalue Management ========== */

struct vm_upvalue* vm_capture_upvalue(struct vm_context* vm, lisp_value* local) {
    /* Check if already captured */
    struct vm_upvalue* prev = NULL;
    struct vm_upvalue* curr = vm->open_upvalues;
    
    while (curr && curr->location > local) {
        prev = curr;
        curr = curr->next;
    }
    
    if (curr && curr->location == local) {
        return curr; /* Already exists */
    }
    
    /* Create new upvalue */
    struct vm_upvalue* upvalue = (struct vm_upvalue*)kmalloc(sizeof(struct vm_upvalue));
    if (!upvalue) return NULL;
    
    upvalue->location = local;
    upvalue->closed = LISP_NIL;
    upvalue->next = curr;
    
    if (prev) {
        prev->next = upvalue;
    } else {
        vm->open_upvalues = upvalue;
    }
    
    return upvalue;
}

void vm_close_upvalues(struct vm_context* vm, lisp_value* last) {
    while (vm->open_upvalues && vm->open_upvalues->location >= last) {
        struct vm_upvalue* upvalue = vm->open_upvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm->open_upvalues = upvalue->next;
    }
}

/* ========== Closure Creation ========== */

struct vm_closure* vm_create_closure(struct vm_context* vm, struct bc_function* func) {
    struct vm_closure* closure = (struct vm_closure*)gc_alloc(
        sizeof(struct vm_closure) + func->upvalue_count * sizeof(struct vm_upvalue*));
    
    if (!closure) return NULL;
    
    SET_TYPE(&closure->header, TYPE_FUNCTION);
    closure->func = func;
    closure->upvalue_count = func->upvalue_count;
    
    if (func->upvalue_count > 0) {
        closure->upvalues = (struct vm_upvalue**)(closure + 1);
        memset(closure->upvalues, 0, func->upvalue_count * sizeof(struct vm_upvalue*));
    } else {
        closure->upvalues = NULL;
    }
    
    return closure;
}

/* ========== Global Environment ========== */

void vm_define_global(struct vm_context* vm, lisp_value name, lisp_value value) {
    lisp_env_bind(vm->globals, name, value);
}

lisp_value vm_get_global(struct vm_context* vm, lisp_value name) {
    return lisp_env_lookup(vm->globals, name);
}

bool vm_set_global(struct vm_context* vm, lisp_value name, lisp_value value) {
    lisp_value existing = lisp_env_lookup(vm->globals, name);
    if (existing == LISP_UNBOUND) {
        return false;
    }
    lisp_env_bind(vm->globals, name, value);
    return true;
}

/* ========== Function Calls ========== */

static bool call_value(struct vm_context* vm, lisp_value callee, int argc) {
    if (!IS_POINTER(callee)) {
        vm_error(vm, "Cannot call non-function value");
        return false;
    }
    
    heap_type_t type = GET_TYPE(PTR_VAL(callee));
    
    if (type == TYPE_FUNCTION) {
        struct vm_closure* closure = (struct vm_closure*)PTR_VAL(callee);
        
        if (argc != closure->func->arity && !closure->func->is_variadic) {
            vm_error(vm, "Expected %d arguments but got %d", closure->func->arity, argc);
            return false;
        }
        
        if (vm->frame_count >= VM_FRAMES_MAX) {
            vm_error(vm, "Call stack overflow");
            return false;
        }
        
        struct vm_frame* frame = &vm->frames[vm->frame_count++];
        frame->closure = closure;
        frame->ip = closure->func->code;
        frame->slots = vm->stack_top - argc; /* Point to first arg, not closure */
        
        return true;
    } else if (type == TYPE_BUILTIN) {
        struct lisp_builtin* builtin = (struct lisp_builtin*)PTR_VAL(callee);
        
        /* Build args list for builtin */
        lisp_value args = LISP_NIL;
        for (int i = argc - 1; i >= 0; i--) {
            args = lisp_create_cons(vm->stack_top[-1 - i], args);
        }
        
        /* Pop arguments and callee */
        vm->stack_top -= argc + 1;
        
        /* Call builtin */
        lisp_value result = builtin->fn(vm->globals, args);
        PUSH(result);
        
        return true;
    }
    
    vm_error(vm, "Value is not callable");
    return false;
}

vm_result_t vm_call(struct vm_context* vm, struct vm_closure* closure, int argc) {
    vm_push(vm, PTR_TO_VAL(closure));
    
    if (!call_value(vm, PTR_TO_VAL(closure), argc)) {
        return VM_RUNTIME_ERROR;
    }
    
    return vm_run(vm);
}

/* ========== Main Dispatch Loop ========== */

vm_result_t vm_execute(struct vm_context* vm, struct bc_chunk* chunk) {
    if (!chunk || !chunk->main_function) {
        return VM_COMPILE_ERROR;
    }
    
    /* Create closure for main function */
    struct vm_closure* main_closure = vm_create_closure(vm, chunk->main_function);
    if (!main_closure) {
        return VM_OUT_OF_MEMORY;
    }
    
    /* Push main closure and call it */
    PUSH(PTR_TO_VAL(main_closure));
    
    struct vm_frame* frame = &vm->frames[vm->frame_count++];
    frame->closure = main_closure;
    frame->ip = main_closure->func->code;
    frame->slots = vm->stack_top - 1;
    
    return vm_run(vm);
}

vm_result_t vm_run(struct vm_context* vm) {
    struct vm_frame* frame = CURRENT_FRAME();
    
    /* GC roots for active values */
    GC_PUSH_1(vm->globals);
    
    for (;;) {
        vm->instructions_executed++;
        
        uint8_t instruction = READ_BYTE();
        
        switch ((bc_opcode_t)instruction) {
            
            /* ===== Stack Manipulation ===== */
            
            case BC_NOP:
                break;
                
            case BC_POP:
                DROP(1);
                break;
                
            case BC_DUP:
                PUSH(PEEK(0));
                break;
                
            case BC_SWAP: {
                lisp_value a = POP();
                lisp_value b = POP();
                PUSH(a);
                PUSH(b);
                break;
            }
            
            case BC_ROT: {
                lisp_value c = POP();
                lisp_value b = POP();
                lisp_value a = POP();
                PUSH(b);
                PUSH(c);
                PUSH(a);
                break;
            }
            
            /* ===== Constants ===== */
            
            case BC_PUSH_NIL:
                PUSH(LISP_NIL);
                break;
                
            case BC_PUSH_T:
                PUSH(LISP_T);
                break;
                
            case BC_PUSH_FIXNUM: {
                int64_t value = READ_LONG();
                PUSH(MAKE_FIXNUM(value));
                break;
            }
            
            case BC_PUSH_CONST: {
                uint16_t idx = READ_SHORT();
                PUSH(frame->closure->func->constants[idx]);
                break;
            }
            
            case BC_PUSH_CHAR: {
                uint32_t c = READ_INT();
                PUSH(MAKE_CHAR(c));
                break;
            }
            
            /* ===== Local Variables ===== */
            
            case BC_LOAD_LOCAL: {
                uint8_t idx = READ_BYTE();
                PUSH(frame->slots[idx]);
                break;
            }
            
            case BC_STORE_LOCAL: {
                uint8_t idx = READ_BYTE();
                frame->slots[idx] = PEEK(0);
                break;
            }
            
            case BC_LOAD_LOCAL_W: {
                uint16_t idx = READ_SHORT();
                PUSH(frame->slots[idx]);
                break;
            }
            
            case BC_STORE_LOCAL_W: {
                uint16_t idx = READ_SHORT();
                frame->slots[idx] = PEEK(0);
                break;
            }
            
            /* ===== Upvalues ===== */
            
            case BC_LOAD_UPVALUE: {
                uint8_t idx = READ_BYTE();
                PUSH(*frame->closure->upvalues[idx]->location);
                break;
            }
            
            case BC_STORE_UPVALUE: {
                uint8_t idx = READ_BYTE();
                *frame->closure->upvalues[idx]->location = PEEK(0);
                break;
            }
            
            case BC_CLOSE_UPVALUE:
                vm_close_upvalues(vm, vm->stack_top - 1);
                DROP(1);
                break;
            
            /* ===== Globals ===== */
            
            case BC_LOAD_GLOBAL: {
                uint16_t idx = READ_SHORT();
                lisp_value name = frame->closure->func->constants[idx];
                lisp_value value = vm_get_global(vm, name);
                if (value == LISP_UNBOUND) {
                    vm_error(vm, "Undefined variable");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                PUSH(value);
                break;
            }
            
            case BC_STORE_GLOBAL: {
                uint16_t idx = READ_SHORT();
                lisp_value name = frame->closure->func->constants[idx];
                if (!vm_set_global(vm, name, PEEK(0))) {
                    vm_error(vm, "Cannot set undefined variable");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_DEFINE_GLOBAL: {
                uint16_t idx = READ_SHORT();
                lisp_value name = frame->closure->func->constants[idx];
                vm_define_global(vm, name, PEEK(0));
                DROP(1);
                break;
            }
            
            /* ===== Arithmetic ===== */
            
            case BC_ADD: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) + FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_SUB: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) - FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_MUL: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) * FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_DIV: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    if (FIXNUM_VAL(b) == 0) {
                        vm_error(vm, "Division by zero");
                        GC_POP();
                        return VM_RUNTIME_ERROR;
                    }
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) / FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_MOD: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    if (FIXNUM_VAL(b) == 0) {
                        vm_error(vm, "Division by zero");
                        GC_POP();
                        return VM_RUNTIME_ERROR;
                    }
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) % FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_NEG: {
                lisp_value a = POP();
                if (IS_FIXNUM(a)) {
                    PUSH(MAKE_FIXNUM(-FIXNUM_VAL(a)));
                } else {
                    vm_error(vm, "Operand must be a number");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            /* ===== Bitwise ===== */
            
            case BC_AND: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) & FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be integers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_OR: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) | FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be integers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_XOR: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) ^ FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be integers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_NOT: {
                lisp_value a = POP();
                if (IS_FIXNUM(a)) {
                    PUSH(MAKE_FIXNUM(~FIXNUM_VAL(a)));
                } else {
                    vm_error(vm, "Operand must be an integer");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_SHL: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) << FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be integers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_SHR: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(MAKE_FIXNUM(FIXNUM_VAL(a) >> FIXNUM_VAL(b)));
                } else {
                    vm_error(vm, "Operands must be integers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            /* ===== Comparison ===== */
            
            case BC_EQ: {
                lisp_value b = POP();
                lisp_value a = POP();
                PUSH(a == b ? LISP_T : LISP_NIL);
                break;
            }
            
            case BC_EQUAL: {
                /* Structural equality - simplified for now */
                lisp_value b = POP();
                lisp_value a = POP();
                PUSH(a == b ? LISP_T : LISP_NIL);
                break;
            }
            
            case BC_LT: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(FIXNUM_VAL(a) < FIXNUM_VAL(b) ? LISP_T : LISP_NIL);
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_LE: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(FIXNUM_VAL(a) <= FIXNUM_VAL(b) ? LISP_T : LISP_NIL);
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_GT: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(FIXNUM_VAL(a) > FIXNUM_VAL(b) ? LISP_T : LISP_NIL);
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_GE: {
                lisp_value b = POP();
                lisp_value a = POP();
                if (IS_FIXNUM(a) && IS_FIXNUM(b)) {
                    PUSH(FIXNUM_VAL(a) >= FIXNUM_VAL(b) ? LISP_T : LISP_NIL);
                } else {
                    vm_error(vm, "Operands must be numbers");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_ZEROP: {
                lisp_value a = POP();
                PUSH(IS_FIXNUM(a) && FIXNUM_VAL(a) == 0 ? LISP_T : LISP_NIL);
                break;
            }
            
            case BC_NULLP: {
                lisp_value a = POP();
                PUSH(IS_NIL(a) ? LISP_T : LISP_NIL);
                break;
            }
            
            /* ===== Control Flow ===== */
            
            case BC_JUMP: {
                int16_t offset = (int16_t)READ_SHORT();
                frame->ip += offset;
                break;
            }
            
            case BC_JUMP_IF_NIL: {
                int16_t offset = (int16_t)READ_SHORT();
                if (IS_NIL(POP())) {
                    frame->ip += offset;
                }
                break;
            }
            
            case BC_JUMP_IF_NOT_NIL: {
                int16_t offset = (int16_t)READ_SHORT();
                if (!IS_NIL(POP())) {
                    frame->ip += offset;
                }
                break;
            }
            
            case BC_JUMP_IF_FALSE: {
                int16_t offset = (int16_t)READ_SHORT();
                if (IS_NIL(PEEK(0))) {
                    frame->ip += offset;
                }
                break;
            }
            
            case BC_JUMP_W: {
                int32_t offset = (int32_t)READ_INT();
                frame->ip += offset;
                break;
            }
            
            /* ===== Function Calls ===== */
            
            case BC_CALL: {
                uint8_t argc = READ_BYTE();
                vm->calls_made++;
                if (!call_value(vm, PEEK(argc), argc)) {
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                frame = CURRENT_FRAME();
                break;
            }
            
            case BC_CALL_W: {
                uint16_t argc = READ_SHORT();
                vm->calls_made++;
                if (!call_value(vm, PEEK(argc), argc)) {
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                frame = CURRENT_FRAME();
                break;
            }
            
            case BC_TAIL_CALL: {
                uint8_t argc = READ_BYTE();
                /* Reuse current frame for tail call */
                lisp_value callee = PEEK(argc);
                
                if (!IS_POINTER(callee) || GET_TYPE(PTR_VAL(callee)) != TYPE_FUNCTION) {
                    vm_error(vm, "Tail call to non-function");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                
                struct vm_closure* closure = (struct vm_closure*)PTR_VAL(callee);
                
                /* Close upvalues */
                vm_close_upvalues(vm, frame->slots);
                
                /* Shift arguments down */
                for (int i = 0; i <= argc; i++) {
                    frame->slots[i] = vm->stack_top[-argc - 1 + i];
                }
                vm->stack_top = frame->slots + argc + 1;
                
                /* Update frame */
                frame->closure = closure;
                frame->ip = closure->func->code;
                break;
            }
            
            case BC_RETURN: {
                lisp_value result = POP();
                
                vm_close_upvalues(vm, frame->slots);
                
                vm->frame_count--;
                if (vm->frame_count == 0) {
                    GC_POP();
                    PUSH(result);
                    return VM_OK;
                }
                
                vm->stack_top = frame->slots;
                PUSH(result);
                frame = CURRENT_FRAME();
                break;
            }
            
            case BC_APPLY: {
                /* (apply fn args-list) */
                lisp_value args_list = POP();
                lisp_value fn = POP();
                
                /* Push function and arguments */
                PUSH(fn);
                int argc = 0;
                while (IS_CONS(args_list)) {
                    PUSH(CAR(args_list));
                    args_list = CDR(args_list);
                    argc++;
                }
                
                if (!call_value(vm, PEEK(argc), argc)) {
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                frame = CURRENT_FRAME();
                break;
            }
            
            /* ===== Closures ===== */
            
            case BC_MAKE_CLOSURE: {
                uint16_t func_idx = READ_SHORT();
                struct bc_function* func = frame->closure->func->constants[func_idx] ? 
                    (struct bc_function*)(uintptr_t)frame->closure->func->constants[func_idx] : NULL;
                    
                if (!func) {
                    vm_error(vm, "Invalid function index");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                
                struct vm_closure* closure = vm_create_closure(vm, func);
                if (!closure) {
                    vm_error(vm, "Out of memory creating closure");
                    GC_POP();
                    return VM_OUT_OF_MEMORY;
                }
                
                /* Capture upvalues as specified */
                for (size_t i = 0; i < func->upvalue_count; i++) {
                    struct bc_upvalue_desc* desc = &func->upvalues[i];
                    if (desc->is_local) {
                        closure->upvalues[i] = vm_capture_upvalue(vm, &frame->slots[desc->index]);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[desc->index];
                    }
                }
                
                PUSH(PTR_TO_VAL(closure));
                break;
            }
            
            /* ===== List Operations ===== */
            
            case BC_CONS: {
                lisp_value cdr = POP();
                lisp_value car = POP();
                PUSH(lisp_create_cons(car, cdr));
                break;
            }
            
            case BC_CAR: {
                lisp_value pair = POP();
                if (IS_CONS(pair)) {
                    PUSH(CAR(pair));
                } else if (IS_NIL(pair)) {
                    PUSH(LISP_NIL);
                } else {
                    vm_error(vm, "CAR requires a list");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_CDR: {
                lisp_value pair = POP();
                if (IS_CONS(pair)) {
                    PUSH(CDR(pair));
                } else if (IS_NIL(pair)) {
                    PUSH(LISP_NIL);
                } else {
                    vm_error(vm, "CDR requires a list");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                break;
            }
            
            case BC_SET_CAR: {
                lisp_value val = POP();
                lisp_value pair = POP();
                if (IS_CONS(pair)) {
                    struct lisp_cons* c = (struct lisp_cons*)PTR_VAL(pair);
                    c->car = val;
                    gc_write_barrier(pair, &c->car, val);
                }
                break;
            }
            
            case BC_SET_CDR: {
                lisp_value val = POP();
                lisp_value pair = POP();
                if (IS_CONS(pair)) {
                    struct lisp_cons* c = (struct lisp_cons*)PTR_VAL(pair);
                    c->cdr = val;
                    gc_write_barrier(pair, &c->cdr, val);
                }
                break;
            }
            
            case BC_LIST: {
                uint8_t count = READ_BYTE();
                lisp_value list = LISP_NIL;
                for (int i = 0; i < count; i++) {
                    list = lisp_create_cons(vm->stack_top[-1 - i], list);
                }
                DROP(count);
                PUSH(list);
                break;
            }
            
            /* ===== Type Checks ===== */
            
            case BC_CONSP:
                PUSH(IS_CONS(POP()) ? LISP_T : LISP_NIL);
                break;
                
            case BC_SYMBOLP:
                PUSH(IS_SYMBOL(POP()) ? LISP_T : LISP_NIL);
                break;
                
            case BC_NUMBERP:
                PUSH(IS_FIXNUM(POP()) ? LISP_T : LISP_NIL);
                break;
                
            case BC_STRINGP:
                PUSH(IS_STRING(POP()) ? LISP_T : LISP_NIL);
                break;
                
            case BC_FUNCTIONP: {
                lisp_value v = POP();
                bool is_fn = IS_POINTER(v) && 
                    (GET_TYPE(PTR_VAL(v)) == TYPE_FUNCTION || 
                     GET_TYPE(PTR_VAL(v)) == TYPE_BUILTIN);
                PUSH(is_fn ? LISP_T : LISP_NIL);
                break;
            }
            
            case BC_ATOMP:
                PUSH(!IS_CONS(POP()) ? LISP_T : LISP_NIL);
                break;
            
            /* ===== Error Handling ===== */
            
            case BC_PUSH_HANDLER: {
                int16_t offset = (int16_t)READ_SHORT();
                
                struct vm_handler* handler = (struct vm_handler*)kmalloc(sizeof(struct vm_handler));
                if (!handler) {
                    vm_error(vm, "Out of memory for error handler");
                    GC_POP();
                    return VM_OUT_OF_MEMORY;
                }
                
                handler->frame = frame;
                handler->handler_ip = frame->ip + offset;
                handler->stack_top = vm->stack_top;
                handler->prev = vm->handler_stack;
                vm->handler_stack = handler;
                break;
            }
            
            case BC_POP_HANDLER: {
                if (vm->handler_stack) {
                    struct vm_handler* handler = vm->handler_stack;
                    vm->handler_stack = handler->prev;
                    kfree(handler);
                }
                break;
            }
            
            case BC_THROW: {
                lisp_value error_val = POP();
                
                if (!vm->handler_stack) {
                    vm_error(vm, "Unhandled exception");
                    GC_POP();
                    return VM_RUNTIME_ERROR;
                }
                
                struct vm_handler* handler = vm->handler_stack;
                vm->handler_stack = handler->prev;
                
                /* Unwind to handler frame */
                while (vm->frame_count > 0 && CURRENT_FRAME() != handler->frame) {
                    vm->frame_count--;
                }
                
                frame = handler->frame;
                frame->ip = handler->handler_ip;
                vm->stack_top = handler->stack_top;
                PUSH(error_val);
                
                kfree(handler);
                break;
            }
            
            /* ===== Debug ===== */
            
            case BC_DEBUG_PRINT: {
                lisp_value val = PEEK(0);
                printf("DEBUG: ");
                lisp_print(val);
                printf("\n");
                break;
            }
            
            case BC_BREAKPOINT:
                /* In a real debugger, would pause here */
                printf("BREAKPOINT at offset %zu\n", 
                       (size_t)(frame->ip - frame->closure->func->code - 1));
                break;
            
            case BC_HALT:
                GC_POP();
                return VM_OK;
            
            default:
                vm_error(vm, "Unknown opcode: 0x%02x", instruction);
                GC_POP();
                return VM_RUNTIME_ERROR;
        }
    }
}

/* ========== Debug Helpers ========== */

void vm_stack_trace(struct vm_context* vm) {
    printf("=== Stack Trace ===\n");
    for (int i = vm->frame_count - 1; i >= 0; i--) {
        struct vm_frame* frame = &vm->frames[i];
        printf("[%d] %s\n", i, frame->closure->func->name ? frame->closure->func->name : "<anonymous>");
    }
}

void vm_print_state(struct vm_context* vm) {
    printf("=== VM State ===\n");
    printf("Frames: %d\n", vm->frame_count);
    printf("Stack depth: %td\n", vm->stack_top - vm->stack);
    printf("Instructions: %llu\n", (unsigned long long)vm->instructions_executed);
    printf("Calls: %llu\n", (unsigned long long)vm->calls_made);
    
    printf("\nStack:\n");
    for (lisp_value* slot = vm->stack; slot < vm->stack_top; slot++) {
        printf("  [%td] ", slot - vm->stack);
        lisp_print(*slot);
        printf("\n");
    }
}
