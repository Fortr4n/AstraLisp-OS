/* AstraLisp OS Virtual Machine
 * 
 * Stack-based bytecode execution engine.
 */

#ifndef VM_H
#define VM_H

#include "bytecode.h"
#include "tagged.h"
#include "objects.h"
#include <stdint.h>
#include <stdbool.h>

/* VM configuration */
#define VM_STACK_SIZE     65536   /* Value stack size */
#define VM_FRAMES_MAX     1024    /* Maximum call depth */
#define VM_OPEN_UPVALUES_MAX 256  /* Maximum open upvalues */

/* ========== Upvalue (for closures) ========== */

/* Runtime upvalue - captures variables across closures */
struct vm_upvalue {
    lisp_value* location;       /* Pointer to captured value */
    lisp_value closed;          /* Storage when closed over */
    struct vm_upvalue* next;    /* Linked list for open upvalues */
};

/* ========== Closure ========== */

/* Runtime closure - function + captured environment */
struct vm_closure {
    struct lisp_header header;  /* GC header (TYPE_FUNCTION) */
    struct bc_function* func;   /* Bytecode function prototype */
    struct vm_upvalue** upvalues; /* Captured upvalues */
    size_t upvalue_count;
};

/* ========== Call Frame ========== */

/* Call frame for function invocation */
struct vm_frame {
    struct vm_closure* closure; /* Current closure */
    uint8_t* ip;                /* Instruction pointer */
    lisp_value* slots;          /* Pointer into value stack for locals */
};

/* ========== Error Handler ========== */

/* Bytecode error handler (for try/catch) */
struct vm_handler {
    struct vm_frame* frame;     /* Frame where handler was pushed */
    uint8_t* handler_ip;        /* IP to jump to on error */
    lisp_value* stack_top;      /* Stack top when handler was pushed */
    struct vm_handler* prev;    /* Previous handler */
};

/* ========== VM Context ========== */

/* VM execution result */
typedef enum {
    VM_OK,
    VM_RUNTIME_ERROR,
    VM_COMPILE_ERROR,
    VM_STACK_OVERFLOW,
    VM_OUT_OF_MEMORY
} vm_result_t;

/* Main VM context */
struct vm_context {
    /* Value stack */
    lisp_value stack[VM_STACK_SIZE];
    lisp_value* stack_top;
    
    /* Call frames */
    struct vm_frame frames[VM_FRAMES_MAX];
    int frame_count;
    
    /* Open upvalues (linked list, most recent first) */
    struct vm_upvalue* open_upvalues;
    
    /* Global environment */
    lisp_value globals;
    
    /* Error handling */
    struct vm_handler* handler_stack;
    char* last_error;
    
    /* GC integration */
    size_t bytes_allocated;
    size_t next_gc;
    
    /* Statistics */
    uint64_t instructions_executed;
    uint64_t calls_made;
};

/* ========== VM API ========== */

/* Initialize VM */
int vm_init(struct vm_context* vm);

/* Free VM resources */
void vm_free(struct vm_context* vm);

/* Execute bytecode chunk */
vm_result_t vm_execute(struct vm_context* vm, struct bc_chunk* chunk);

/* Execute from a closure */
vm_result_t vm_call(struct vm_context* vm, struct vm_closure* closure, int argc);

/* Run VM dispatch loop */
vm_result_t vm_run(struct vm_context* vm);

/* Get result from top of stack */
lisp_value vm_get_result(struct vm_context* vm);

/* Push value onto stack */
void vm_push(struct vm_context* vm, lisp_value value);

/* Pop value from stack */
lisp_value vm_pop(struct vm_context* vm);

/* Peek at stack without popping */
lisp_value vm_peek(struct vm_context* vm, int distance);

/* Set runtime error */
void vm_error(struct vm_context* vm, const char* format, ...);

/* ========== Closure API ========== */

/* Create closure from function prototype */
struct vm_closure* vm_create_closure(struct vm_context* vm, struct bc_function* func);

/* Create upvalue for local variable */
struct vm_upvalue* vm_capture_upvalue(struct vm_context* vm, lisp_value* local);

/* Close all upvalues up to a given stack slot */
void vm_close_upvalues(struct vm_context* vm, lisp_value* last);

/* ========== Global Environment ========== */

/* Define global variable */
void vm_define_global(struct vm_context* vm, lisp_value name, lisp_value value);

/* Get global variable */
lisp_value vm_get_global(struct vm_context* vm, lisp_value name);

/* Set global variable */
bool vm_set_global(struct vm_context* vm, lisp_value name, lisp_value value);

/* ========== Built-in Registration ========== */

/* Native function signature for VM */
typedef lisp_value (*vm_native_fn)(struct vm_context* vm, int argc, lisp_value* args);

/* Register native function */
void vm_register_native(struct vm_context* vm, const char* name, vm_native_fn fn);

/* ========== Debugging ========== */

/* Print stack trace */
void vm_stack_trace(struct vm_context* vm);

/* Print current state */
void vm_print_state(struct vm_context* vm);

#endif /* VM_H */
