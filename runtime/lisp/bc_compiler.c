/* AstraLisp OS Bytecode Compiler
 * 
 * Compiles Lisp AST to bytecode for VM execution.
 */

#include "bytecode.h"
#include "vm.h"
#include "reader.h"
#include "symbols.h"
#include "objects.h"
#include "types.h"
#include "../gc/gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Kernel compatibility */
#ifdef KERNEL
extern void* kmalloc(size_t size);
extern void kfree(void* ptr);
#else
#define kmalloc malloc
#define kfree free
#endif

/* ========== Compiler State ========== */

/* Local variable in current scope */
struct local {
    lisp_value name;            /* Symbol for variable name */
    int depth;                  /* Scope depth where declared */
    bool is_captured;           /* Is captured by a closure? */
};

/* Upvalue reference */
struct upvalue_ref {
    uint8_t index;              /* Index in enclosing scope */
    bool is_local;              /* Is local in enclosing scope? */
};

/* Compiler context for a single function */
struct compiler {
    struct compiler* enclosing;         /* Enclosing compiler (for nested functions) */
    struct bc_function* function;       /* Function being compiled */
    
    /* Local variables */
    struct local locals[256];
    int local_count;
    int scope_depth;
    
    /* Upvalues */
    struct upvalue_ref upvalues[256];
    int upvalue_count;
    
    /* Loop context for break/continue */
    int loop_start;
    int loop_depth;
    int* break_jumps;
    int break_count;
    int break_capacity;
    
    /* Current source line */
    int current_line;
};

/* Global compiler state */
static struct compiler* current_compiler = NULL;

/* ========== Error Handling ========== */

static bool had_error = false;

static void compile_error(const char* message) {
    fprintf(stderr, "Compile error: %s\n", message);
    had_error = true;
}

/* ========== Helper Functions ========== */

static struct bc_function* current_function(void) {
    return current_compiler->function;
}

static void emit_byte(uint8_t byte) {
    bc_emit(current_function(), byte);
}

static void emit_op(bc_opcode_t op) {
    bc_emit_op(current_function(), op);
}

static void emit_bytes(bc_opcode_t op1, uint8_t operand) {
    emit_op(op1);
    emit_byte(operand);
}

static void emit_short(uint16_t value) {
    bc_emit_short(current_function(), value);
}

static size_t emit_jump(bc_opcode_t op) {
    emit_op(op);
    emit_byte(0xFF);
    emit_byte(0xFF);
    return current_function()->code_length - 2;
}

static void patch_jump(size_t offset) {
    bc_patch_jump(current_function(), offset);
}

static void emit_loop(size_t loop_start) {
    emit_op(BC_JUMP);
    
    int offset = (int)(current_function()->code_length - loop_start + 2);
    if (offset > 0x7FFF) {
        compile_error("Loop body too large");
        return;
    }
    
    /* Negative offset for backward jump */
    int16_t jump = -offset;
    emit_byte((jump >> 8) & 0xFF);
    emit_byte(jump & 0xFF);
}

static uint16_t make_constant(lisp_value value) {
    return bc_add_constant(current_function(), value);
}

static void emit_constant(lisp_value value) {
    uint16_t idx = make_constant(value);
    emit_op(BC_PUSH_CONST);
    emit_short(idx);
}

/* ========== Scope Management ========== */

static void begin_scope(void) {
    current_compiler->scope_depth++;
}

static void end_scope(void) {
    current_compiler->scope_depth--;
    
    /* Pop locals that are going out of scope */
    while (current_compiler->local_count > 0 &&
           current_compiler->locals[current_compiler->local_count - 1].depth > current_compiler->scope_depth) {
        
        if (current_compiler->locals[current_compiler->local_count - 1].is_captured) {
            emit_op(BC_CLOSE_UPVALUE);
        } else {
            emit_op(BC_POP);
        }
        current_compiler->local_count--;
    }
}

/* ========== Local Variables ========== */

static void add_local(lisp_value name) {
    if (current_compiler->local_count >= 256) {
        compile_error("Too many local variables in function");
        return;
    }
    
    struct local* local = &current_compiler->locals[current_compiler->local_count++];
    local->name = name;
    local->depth = current_compiler->scope_depth;
    local->is_captured = false;
}

static int resolve_local(struct compiler* compiler, lisp_value name) {
    for (int i = compiler->local_count - 1; i >= 0; i--) {
        if (compiler->locals[i].name == name) {
            return i;
        }
    }
    return -1;
}

static int add_upvalue(struct compiler* compiler, uint8_t index, bool is_local) {
    int upvalue_count = compiler->upvalue_count;
    
    /* Check if already captured */
    for (int i = 0; i < upvalue_count; i++) {
        if (compiler->upvalues[i].index == index && 
            compiler->upvalues[i].is_local == is_local) {
            return i;
        }
    }
    
    if (upvalue_count >= 256) {
        compile_error("Too many closure variables");
        return 0;
    }
    
    compiler->upvalues[upvalue_count].is_local = is_local;
    compiler->upvalues[upvalue_count].index = index;
    return compiler->upvalue_count++;
}

static int resolve_upvalue(struct compiler* compiler, lisp_value name) {
    if (!compiler->enclosing) return -1;
    
    int local = resolve_local(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].is_captured = true;
        return add_upvalue(compiler, (uint8_t)local, true);
    }
    
    int upvalue = resolve_upvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return add_upvalue(compiler, (uint8_t)upvalue, false);
    }
    
    return -1;
}

/* ========== Expression Compilation ========== */

/* Forward declarations */
static void compile_expression(lisp_value expr);
static void compile_body(lisp_value body);

static void compile_atom(lisp_value expr) {
    if (IS_FIXNUM(expr)) {
        int64_t val = FIXNUM_VAL(expr);
        emit_op(BC_PUSH_FIXNUM);
        bc_emit_long(current_function(), val);
    } else if (IS_NIL(expr)) {
        emit_op(BC_PUSH_NIL);
    } else if (IS_T(expr)) {
        emit_op(BC_PUSH_T);
    } else if (IS_CHAR(expr)) {
        emit_op(BC_PUSH_CHAR);
        bc_emit_int(current_function(), CHAR_VAL(expr));
    } else if (IS_SYMBOL(expr)) {
        /* Check for special symbols t and nil */
        lisp_value name = SYMBOL_NAME(expr);
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(name);
        
        if (strcmp(s->data, "t") == 0) {
            emit_op(BC_PUSH_T);
            return;
        } else if (strcmp(s->data, "nil") == 0) {
            emit_op(BC_PUSH_NIL);
            return;
        }
        
        /* Variable reference */
        int local = resolve_local(current_compiler, expr);
        if (local != -1) {
            if (local < 256) {
                emit_bytes(BC_LOAD_LOCAL, (uint8_t)local);
            } else {
                emit_op(BC_LOAD_LOCAL_W);
                emit_short((uint16_t)local);
            }
        } else {
            int upvalue = resolve_upvalue(current_compiler, expr);
            if (upvalue != -1) {
                emit_bytes(BC_LOAD_UPVALUE, (uint8_t)upvalue);
            } else {
                /* Global variable */
                uint16_t idx = make_constant(expr);
                emit_op(BC_LOAD_GLOBAL);
                emit_short(idx);
            }
        }
    } else if (IS_STRING(expr)) {
        emit_constant(expr);
    } else {
        emit_constant(expr);
    }
}


static void compile_quote(lisp_value args) {
    if (!IS_CONS(args)) {
        compile_error("quote requires an argument");
        return;
    }
    emit_constant(CAR(args));
}

static void compile_if(lisp_value args) {
    if (!IS_CONS(args)) {
        compile_error("if requires a condition");
        return;
    }
    
    lisp_value cond = CAR(args);
    lisp_value then_branch = LISP_NIL;
    lisp_value else_branch = LISP_NIL;
    
    if (IS_CONS(CDR(args))) {
        then_branch = CAR(CDR(args));
        if (IS_CONS(CDR(CDR(args)))) {
            else_branch = CAR(CDR(CDR(args)));
        }
    }
    
    /* Compile condition */
    compile_expression(cond);
    
    /* Jump to else if nil */
    size_t else_jump = emit_jump(BC_JUMP_IF_NIL);
    
    /* Compile then branch */
    compile_expression(then_branch);
    
    /* Jump over else branch */
    size_t end_jump = emit_jump(BC_JUMP);
    
    /* Patch else jump */
    patch_jump(else_jump);
    
    /* Compile else branch */
    compile_expression(else_branch);
    
    /* Patch end jump */
    patch_jump(end_jump);
}

static void compile_lambda(lisp_value args) {
    if (!IS_CONS(args)) {
        compile_error("lambda requires parameter list");
        return;
    }
    
    lisp_value params = CAR(args);
    lisp_value body = CDR(args);
    
    /* Create new compiler for function */
    struct compiler func_compiler;
    memset(&func_compiler, 0, sizeof(struct compiler));
    func_compiler.enclosing = current_compiler;
    func_compiler.function = bc_function_create(NULL, 0);
    func_compiler.scope_depth = 0;
    
    current_compiler = &func_compiler;
    begin_scope();
    
    /* Add parameters as locals */
    int arity = 0;
    lisp_value p = params;
    while (IS_CONS(p)) {
        add_local(CAR(p));
        arity++;
        p = CDR(p);
    }
    func_compiler.function->arity = arity;
    func_compiler.function->local_count = arity;
    
    /* Compile body */
    compile_body(body);
    
    /* Emit return */
    emit_op(BC_RETURN);
    
    /* Copy upvalue info to function */
    struct bc_function* func = func_compiler.function;
    if (func_compiler.upvalue_count > 0) {
        func->upvalue_count = func_compiler.upvalue_count;
        func->upvalues = (struct bc_upvalue_desc*)kmalloc(
            func_compiler.upvalue_count * sizeof(struct bc_upvalue_desc));
        for (int i = 0; i < func_compiler.upvalue_count; i++) {
            func->upvalues[i].is_local = func_compiler.upvalues[i].is_local;
            func->upvalues[i].index = func_compiler.upvalues[i].index;
        }
    }
    
    /* Restore enclosing compiler */
    current_compiler = func_compiler.enclosing;
    
    /* Emit closure creation in parent */
    uint16_t func_idx = make_constant((lisp_value)(uintptr_t)func);
    emit_op(BC_MAKE_CLOSURE);
    emit_short(func_idx);
    
    /* Emit upvalue capture instructions */
    for (int i = 0; i < func_compiler.upvalue_count; i++) {
        if (func_compiler.upvalues[i].is_local) {
            emit_bytes(BC_CAPTURE_LOCAL, func_compiler.upvalues[i].index);
        } else {
            emit_bytes(BC_CAPTURE_UPVALUE, func_compiler.upvalues[i].index);
        }
    }
}

static void compile_let(lisp_value args) {
    if (!IS_CONS(args)) {
        compile_error("let requires bindings list");
        return;
    }
    
    lisp_value bindings = CAR(args);
    lisp_value body = CDR(args);
    
    begin_scope();
    
    /* Process bindings */
    while (IS_CONS(bindings)) {
        lisp_value binding = CAR(bindings);
        
        if (IS_CONS(binding)) {
            lisp_value name = CAR(binding);
            lisp_value init = IS_CONS(CDR(binding)) ? CAR(CDR(binding)) : LISP_NIL;
            
            /* Compile initializer */
            compile_expression(init);
            
            /* Add local */
            add_local(name);
        }
        
        bindings = CDR(bindings);
    }
    
    /* Compile body */
    compile_body(body);
    
    end_scope();
}

static void compile_setq(lisp_value args) {
    if (!IS_CONS(args) || !IS_CONS(CDR(args))) {
        compile_error("setq requires variable and value");
        return;
    }
    
    lisp_value name = CAR(args);
    lisp_value value = CAR(CDR(args));
    
    compile_expression(value);
    
    int local = resolve_local(current_compiler, name);
    if (local != -1) {
        if (local < 256) {
            emit_bytes(BC_STORE_LOCAL, (uint8_t)local);
        } else {
            emit_op(BC_STORE_LOCAL_W);
            emit_short((uint16_t)local);
        }
    } else {
        int upvalue = resolve_upvalue(current_compiler, name);
        if (upvalue != -1) {
            emit_bytes(BC_STORE_UPVALUE, (uint8_t)upvalue);
        } else {
            uint16_t idx = make_constant(name);
            emit_op(BC_STORE_GLOBAL);
            emit_short(idx);
        }
    }
}

static void compile_defun(lisp_value args) {
    if (!IS_CONS(args) || !IS_CONS(CDR(args))) {
        compile_error("defun requires name and parameters");
        return;
    }
    
    lisp_value name = CAR(args);
    lisp_value params = CAR(CDR(args));
    lisp_value body = CDR(CDR(args));
    
    /* Build lambda expression */
    lisp_value lambda_args = lisp_create_cons(params, body);
    GC_PUSH_1(lambda_args);
    
    /* Compile as lambda */
    compile_lambda(lambda_args);
    
    GC_POP();
    
    /* Define in global scope */
    uint16_t idx = make_constant(name);
    emit_op(BC_DEFINE_GLOBAL);
    emit_short(idx);
    
    /* Push name as result */
    emit_constant(name);
}

static void compile_progn(lisp_value body) {
    if (IS_NIL(body)) {
        emit_op(BC_PUSH_NIL);
        return;
    }
    
    while (IS_CONS(body)) {
        compile_expression(CAR(body));
        
        if (IS_CONS(CDR(body))) {
            emit_op(BC_POP); /* Discard intermediate results */
        }
        body = CDR(body);
    }
}

static void compile_try(lisp_value args) {
    if (!IS_CONS(args) || !IS_CONS(CDR(args))) {
        compile_error("try requires expression and handler");
        return;
    }
    
    lisp_value expr = CAR(args);
    lisp_value handler = CAR(CDR(args));
    
    /* Emit handler setup */
    size_t handler_jump = emit_jump(BC_PUSH_HANDLER);
    
    /* Compile protected expression */
    compile_expression(expr);
    
    /* Pop handler if successful */
    emit_op(BC_POP_HANDLER);
    
    /* Jump past handler code */
    size_t end_jump = emit_jump(BC_JUMP);
    
    /* Handler code starts here */
    patch_jump(handler_jump);
    
    /* Error value is on stack, call handler with it */
    compile_expression(handler);
    emit_bytes(BC_CALL, 1);
    
    patch_jump(end_jump);
}

static void compile_call(lisp_value expr) {
    lisp_value func = CAR(expr);
    lisp_value args = CDR(expr);
    
    /* Compile function */
    compile_expression(func);
    
    /* Compile arguments */
    int argc = 0;
    while (IS_CONS(args)) {
        compile_expression(CAR(args));
        argc++;
        args = CDR(args);
    }
    
    /* Emit call */
    if (argc < 256) {
        emit_bytes(BC_CALL, (uint8_t)argc);
    } else {
        emit_op(BC_CALL_W);
        emit_short((uint16_t)argc);
    }
}

static void compile_builtin_op(const char* op, lisp_value args) {
    bc_opcode_t opcode;
    int arity = 2;
    
    if (strcmp(op, "+") == 0) {
        opcode = BC_ADD;
    } else if (strcmp(op, "-") == 0) {
        opcode = BC_SUB;
    } else if (strcmp(op, "*") == 0) {
        opcode = BC_MUL;
    } else if (strcmp(op, "/") == 0) {
        opcode = BC_DIV;
    } else if (strcmp(op, "mod") == 0 || strcmp(op, "%") == 0) {
        opcode = BC_MOD;
    } else if (strcmp(op, "eq") == 0) {
        opcode = BC_EQ;
    } else if (strcmp(op, "<") == 0) {
        opcode = BC_LT;
    } else if (strcmp(op, "<=") == 0) {
        opcode = BC_LE;
    } else if (strcmp(op, ">") == 0) {
        opcode = BC_GT;
    } else if (strcmp(op, ">=") == 0) {
        opcode = BC_GE;
    } else if (strcmp(op, "car") == 0) {
        opcode = BC_CAR;
        arity = 1;
    } else if (strcmp(op, "cdr") == 0) {
        opcode = BC_CDR;
        arity = 1;
    } else if (strcmp(op, "cons") == 0) {
        opcode = BC_CONS;
    } else if (strcmp(op, "null?") == 0 || strcmp(op, "null") == 0) {
        opcode = BC_NULLP;
        arity = 1;
    } else if (strcmp(op, "atom") == 0 || strcmp(op, "atom?") == 0) {
        opcode = BC_ATOMP;
        arity = 1;
    } else {
        return; /* Not a builtin op */
    }
    
    /* Compile arguments */
    int argc = 0;
    while (IS_CONS(args) && argc < arity) {
        compile_expression(CAR(args));
        argc++;
        args = CDR(args);
    }
    
    /* Handle variadic + and * */
    if ((opcode == BC_ADD || opcode == BC_MUL) && argc > 2) {
        /* Fold additional arguments */
        while (argc > 2) {
            emit_op(opcode);
            argc--;
        }
    }
    
    emit_op(opcode);
}

static void compile_list_form(lisp_value expr) {
    lisp_value head = CAR(expr);
    lisp_value args = CDR(expr);
    
    /* Check for special forms */
    if (IS_SYMBOL(head)) {
        lisp_value name = SYMBOL_NAME(head);
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(name);
        const char* name_str = s->data;
        
        if (strcmp(name_str, "quote") == 0) {
            compile_quote(args);
            return;
        } else if (strcmp(name_str, "if") == 0) {
            compile_if(args);
            return;
        } else if (strcmp(name_str, "lambda") == 0) {
            compile_lambda(args);
            return;
        } else if (strcmp(name_str, "let") == 0) {
            compile_let(args);
            return;
        } else if (strcmp(name_str, "setq") == 0) {
            compile_setq(args);
            return;
        } else if (strcmp(name_str, "defun") == 0) {
            compile_defun(args);
            return;
        } else if (strcmp(name_str, "progn") == 0) {
            compile_progn(args);
            return;
        } else if (strcmp(name_str, "try") == 0) {
            compile_try(args);
            return;
        }
        
        /* Check for inline builtins */
        if (strcmp(name_str, "+") == 0 || strcmp(name_str, "-") == 0 ||
            strcmp(name_str, "*") == 0 || strcmp(name_str, "/") == 0 ||
            strcmp(name_str, "car") == 0 || strcmp(name_str, "cdr") == 0 ||
            strcmp(name_str, "cons") == 0 || strcmp(name_str, "eq") == 0 ||
            strcmp(name_str, "<") == 0 || strcmp(name_str, ">") == 0 ||
            strcmp(name_str, "<=") == 0 || strcmp(name_str, ">=") == 0) {
            compile_builtin_op(name_str, args);
            return;
        }
    }
    
    /* Regular function call */
    compile_call(expr);
}

static void compile_expression(lisp_value expr) {
    if (IS_CONS(expr)) {
        compile_list_form(expr);
    } else {
        compile_atom(expr);
    }
}

static void compile_body(lisp_value body) {
    if (IS_NIL(body)) {
        emit_op(BC_PUSH_NIL);
        return;
    }
    
    while (IS_CONS(body)) {
        compile_expression(CAR(body));
        
        if (IS_CONS(CDR(body))) {
            emit_op(BC_POP);
        }
        body = CDR(body);
    }
}

/* ========== Public API ========== */

struct bc_chunk* bc_compile(lisp_value expr) {
    had_error = false;
    
    /* Create chunk */
    struct bc_chunk* chunk = bc_chunk_create();
    if (!chunk) return NULL;
    
    /* Create compiler for top-level */
    struct compiler top_compiler;
    memset(&top_compiler, 0, sizeof(struct compiler));
    top_compiler.function = bc_function_create("<main>", 0);
    top_compiler.scope_depth = 0;
    
    current_compiler = &top_compiler;
    
    /* Compile expression */
    compile_expression(expr);
    
    /* Emit return */
    emit_op(BC_RETURN);
    
    chunk->main_function = top_compiler.function;
    
    if (had_error) {
        bc_chunk_free(chunk);
        return NULL;
    }
    
    return chunk;
}

struct bc_chunk* bc_compile_string(const char* source) {
    struct reader_context ctx;
    reader_init(&ctx, source);
    
    lisp_value expr = reader_read(&ctx);
    GC_PUSH_1(expr);
    
    struct bc_chunk* chunk = bc_compile(expr);
    
    GC_POP();
    return chunk;
}
