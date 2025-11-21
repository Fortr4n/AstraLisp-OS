/* AstraLisp OS Lisp Reader */

#include "reader.h"
#include "tagged.h"
#include "objects.h"
#include "types.h"
#include "../gc/gc.h"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Helper: Peek next character */
static char peek(struct reader_context* ctx) {
    if (ctx->pos >= ctx->len) return 0;
    return ctx->input[ctx->pos];
}

/* Helper: Advance position */
static char advance(struct reader_context* ctx) {
    if (ctx->pos >= ctx->len) return 0;
    return ctx->input[ctx->pos++];
}

/* Helper: Skip whitespace */
static void skip_whitespace(struct reader_context* ctx) {
    while (ctx->pos < ctx->len) {
        char c = peek(ctx);
        if (isspace(c)) {
            advance(ctx);
        } else if (c == ';') {
            /* Comment: skip to end of line */
            while (ctx->pos < ctx->len && peek(ctx) != '\n') {
                advance(ctx);
            }
        } else {
            break;
        }
    }
}

/* Initialize reader context */
void reader_init(struct reader_context* ctx, const char* input) {
    ctx->input = input;
    ctx->pos = 0;
    ctx->len = strlen(input);
    ctx->line = 1;
    ctx->column = 1;
    ctx->symbols = LISP_NIL;
}

/* Read integer */
static lisp_value read_integer(struct reader_context* ctx) {
    char buf[64];
    int i = 0;
    
    if (peek(ctx) == '-') {
        buf[i++] = advance(ctx);
    }
    
    while (isdigit(peek(ctx))) {
        if (i < 63) buf[i++] = advance(ctx);
        else advance(ctx);
    }
    buf[i] = 0;
    
    int64_t val = atoll(buf);
    return MAKE_FIXNUM(val);
}

/* Read symbol */
static lisp_value read_symbol(struct reader_context* ctx) {
    char buf[256];
    int i = 0;
    
    while (ctx->pos < ctx->len) {
        char c = peek(ctx);
        if (isspace(c) || c == '(' || c == ')' || c == ';') {
            break;
        }
        if (i < 255) buf[i++] = advance(ctx);
        else advance(ctx);
    }
    buf[i] = 0;
    
    /* Check for NIL and T */
    if (strcasecmp(buf, "nil") == 0) return LISP_NIL;
    if (strcasecmp(buf, "t") == 0) return LISP_T;
    
    return lisp_create_symbol(buf);
}

/* Read list */
static lisp_value read_list(struct reader_context* ctx) {
    advance(ctx); /* Skip '(' */
    skip_whitespace(ctx);
    
    if (peek(ctx) == ')') {
        advance(ctx);
        return LISP_NIL;
    }
    
    lisp_value head = LISP_NIL;
    lisp_value tail = LISP_NIL;
    
    /* Protect head and tail during reading */
    GC_PUSH_2(head, tail);
    
    while (ctx->pos < ctx->len && peek(ctx) != ')') {
        lisp_value val = reader_read(ctx);
        
        /* Check for dot notation */
        /* Note: This simple reader doesn't handle dot notation perfectly yet */
        /* If val is the dot symbol, read next as cdr and finish */
        /* For now, standard list reading */
        
        lisp_value new_cons = lisp_create_cons(val, LISP_NIL);
        
        if (IS_NIL(head)) {
            head = new_cons;
            tail = new_cons;
        } else {
            struct lisp_cons* c = (struct lisp_cons*)PTR_VAL(tail);
            c->cdr = new_cons;
            gc_write_barrier(tail, &c->cdr, new_cons);
            tail = new_cons;
        }
        
        skip_whitespace(ctx);
    }
    
    if (peek(ctx) == ')') {
        advance(ctx);
    }
    
    GC_POP(); /* head, tail */
    return head;
}

/* Read quote */
static lisp_value read_quote(struct reader_context* ctx) {
    advance(ctx); /* Skip '\'' */
    lisp_value expr = reader_read(ctx);
    
    /* Return (quote expr) */
    GC_PUSH_1(expr);
    lisp_value q = lisp_create_symbol("quote");
    lisp_value list = lisp_create_cons(expr, LISP_NIL);
    lisp_value result = lisp_create_cons(q, list);
    GC_POP();
    return result;
}

/* Main read function */
lisp_value reader_read(struct reader_context* ctx) {
    skip_whitespace(ctx);
    
    char c = peek(ctx);
    if (c == 0) return LISP_NIL; /* EOF */
    
    if (c == '(') {
        return read_list(ctx);
    } else if (c == '\'') {
        return read_quote(ctx);
    } else if (isdigit(c) || (c == '-' && isdigit(ctx->input[ctx->pos+1]))) {
        return read_integer(ctx);
    } else {
        return read_symbol(ctx);
    }
}

/* Factory functions */

lisp_value lisp_create_cons(lisp_value car, lisp_value cdr) {
    struct lisp_cons* cons = (struct lisp_cons*)gc_alloc(sizeof(struct lisp_cons));
    if (!cons) return LISP_NIL;
    
    /* Set header type */
    SET_TYPE(&cons->header, TYPE_CONS);
    
    cons->car = car;
    cons->cdr = cdr;
    
    lisp_value val = PTR_TO_VAL(cons);
    
    /* Write barriers */
    gc_write_barrier(val, &cons->car, car);
    gc_write_barrier(val, &cons->cdr, cdr);
    
    return val;
}

lisp_value lisp_create_symbol(const char* name) {
    /* Interning logic should go here. For now, just create new symbol. */
    /* In a real system, we'd check a symbol table. */
    
    /* Create string for name */
    size_t len = strlen(name);
    struct lisp_string* str = (struct lisp_string*)gc_alloc(sizeof(struct lisp_string) + len + 1);
    if (!str) return LISP_NIL;
    
    SET_TYPE(&str->header, TYPE_STRING);
    
    str->length = len;
    strcpy(str->data, name);
    
    lisp_value name_val = PTR_TO_VAL(str);
    GC_PUSH_1(name_val);
    
    /* Create symbol */
    struct lisp_symbol* sym = (struct lisp_symbol*)gc_alloc(sizeof(struct lisp_symbol));
    if (!sym) {
        GC_POP();
        return LISP_NIL;
    }
    
    SET_TYPE(&sym->header, TYPE_SYMBOL);
    
    sym->name = name_val;
    sym->value = LISP_NIL; /* Unbound */
    sym->function = LISP_NIL;
    sym->hash = 0; /* Todo: hash function */
    
    lisp_value sym_val = PTR_TO_VAL(sym);
    gc_write_barrier(sym_val, &sym->name, name_val);
    
    GC_POP();
    return sym_val;
}

lisp_value lisp_create_function(lisp_value code, lisp_value env, lisp_value args) {
    struct lisp_function* func = (struct lisp_function*)gc_alloc(sizeof(struct lisp_function));
    if (!func) return LISP_NIL;
    
    SET_TYPE(&func->header, TYPE_FUNCTION);
    
    func->code = code;
    func->env = env;
    func->args = args;
    func->name = LISP_NIL;
    
    lisp_value val = PTR_TO_VAL(func);
    gc_write_barrier(val, &func->code, code);
    gc_write_barrier(val, &func->env, env);
    gc_write_barrier(val, &func->args, args);
    
    return val;
}

/* Placeholder for other factories */
lisp_value lisp_create_string(const char* data, size_t length) {
    return LISP_NIL; // Todo
}

lisp_value lisp_create_vector(size_t length) {
    return LISP_NIL; // Todo
}

void lisp_print(lisp_value val) {
    if (IS_NIL(val)) {
        printf("NIL");
    } else if (IS_T(val)) {
        printf("T");
    } else if (IS_FIXNUM(val)) {
        printf("%lld", FIXNUM_VAL(val));
    } else if (IS_CONS(val)) {
        printf("(");
        lisp_print(CAR(val));
        lisp_value curr = CDR(val);
        while (IS_CONS(curr)) {
            printf(" ");
            lisp_print(CAR(curr));
            curr = CDR(curr);
        }
        if (!IS_NIL(curr)) {
            printf(" . ");
            lisp_print(curr);
        }
        printf(")");
    } else if (IS_SYMBOL(val)) {
        lisp_value name = SYMBOL_NAME(val);
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(name);
        printf("%s", s->data);
    } else if (IS_STRING(val)) {
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(val);
        printf("\"%s\"", s->data);
    } else if (IS_FUNCTION(val)) {
        printf("#<FUNCTION>");
    } else {
        printf("#<UNKNOWN>");
    }
}
