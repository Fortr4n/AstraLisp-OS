/* AstraLisp OS Reader Implementation */

#include "reader.h"
#include "objects.h"
#include "types.h"
#include "symbols.h"
#include "../gc/gc.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "lisp_io.h"

#ifdef KERNEL
    #include "../../kernel/hal/serial.h"
#endif

/* Helper: Print integer */
static void print_int(int64_t n) {
    char buffer[32];
    int i = 0;
    int sign = 0;
    
    if (n == 0) {
        LISP_PUTS("0");
        return;
    }

    
    if (n < 0) {
        sign = 1;
        n = -n;
    }
    
    while (n > 0) {
        buffer[i++] = (n % 10) + '0';
        n /= 10;
    }
    
    if (sign) buffer[i++] = '-';
    
    while (i > 0) {
        LISP_PUTCHAR(buffer[--i]);
    }
}



/* Helper: Skip whitespace */
static void skip_whitespace(struct reader_context* ctx) {
    while (ctx->pos < ctx->len) {
        char c = ctx->input[ctx->pos];
        if (isspace(c)) {
            if (c == '\n') {
                ctx->line++;
                ctx->column = 0;
            } else {
                ctx->column++;
            }
            ctx->pos++;
        } else if (c == ';') {
            /* Comment: skip to end of line */
            while (ctx->pos < ctx->len && ctx->input[ctx->pos] != '\n') {
                ctx->pos++;
            }
        } else {
            break;
        }
    }
}

/* Helper: Peek character */
static char peek(struct reader_context* ctx) {
    if (ctx->pos >= ctx->len) return 0;
    return ctx->input[ctx->pos];
}

/* Helper: Read character */
static char next(struct reader_context* ctx) {
    if (ctx->pos >= ctx->len) return 0;
    char c = ctx->input[ctx->pos++];
    if (c == '\n') {
        ctx->line++;
        ctx->column = 0;
    } else {
        ctx->column++;
    }
    return c;
}

/* Initialize reader */
void reader_init(struct reader_context* ctx, const char* input) {
    ctx->input = input;
    ctx->pos = 0;
    ctx->len = strlen(input);
    ctx->line = 1;
    ctx->column = 0;
    ctx->symbols = LISP_NIL;
}

/* Factory: Create Cons */
lisp_value lisp_create_cons(lisp_value car, lisp_value cdr) {
    struct lisp_cons* cons = (struct lisp_cons*)gc_alloc(sizeof(struct lisp_cons));
    if (!cons) return LISP_NIL;
    
    SET_TYPE(&cons->header, TYPE_CONS);
    
    cons->car = car;
    cons->cdr = cdr;
    
    lisp_value val = PTR_TO_VAL(cons);
    
    /* Write barriers */
    gc_write_barrier(val, &cons->car, car);
    gc_write_barrier(val, &cons->cdr, cdr);
    
    return val;
}

/* Factory: Create Symbol (Uses Interning) */
lisp_value lisp_create_symbol(const char* name) {
    return lisp_intern(name);
}

/* Factory: Create String */
lisp_value lisp_create_string(const char* data, size_t length) {
    struct lisp_string* str = (struct lisp_string*)gc_alloc(sizeof(struct lisp_string) + length + 1);
    if (!str) return LISP_NIL;
    
    SET_TYPE(&str->header, TYPE_STRING);
    printf("Debug: Alloc String %p Type=%d\n", PTR_TO_VAL(str), GET_TYPE(PTR_TO_VAL(str)));

    str->length = length;
    memcpy(str->data, data, length);
    str->data[length] = '\0';

    
    return PTR_TO_VAL(str);
}

/* Factory: Create Vector */
lisp_value lisp_create_vector(size_t length) {
    struct lisp_vector* vec = (struct lisp_vector*)gc_alloc(sizeof(struct lisp_vector) + length * sizeof(lisp_value));
    if (!vec) return LISP_NIL;
    
    SET_TYPE(&vec->header, TYPE_VECTOR);
    vec->length = length;
    
    /* Initialize with NIL */
    for (size_t i = 0; i < length; i++) {
        vec->data[i] = LISP_NIL;
    }
    
    return PTR_TO_VAL(vec);
}

/* Factory: Create Function */
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

/* Parse List */
static lisp_value read_list(struct reader_context* ctx) {
    next(ctx); /* Skip '(' */
    skip_whitespace(ctx);
    
    if (peek(ctx) == ')') {
        next(ctx);
        return LISP_NIL;
    }
    
    lisp_value head = LISP_NIL;
    lisp_value tail = LISP_NIL;
    
    GC_PUSH_2(head, tail);
    
    while (ctx->pos < ctx->len && peek(ctx) != ')') {



        if (peek(ctx) == '.') {
            /* Dotted list */
            next(ctx); /* Skip '.' */
            skip_whitespace(ctx);
            lisp_value last = reader_read(ctx);
            
            if (!IS_NIL(tail)) {
                struct lisp_cons* c = (struct lisp_cons*)PTR_VAL(tail);
                c->cdr = last;
                gc_write_barrier(tail, &c->cdr, last);
            } else {
                head = last; /* Should not happen for proper dotted list (a . b) */
            }
            
            skip_whitespace(ctx);
            if (peek(ctx) == ')') next(ctx);
            goto done;
        }
        
        lisp_value val = reader_read(ctx);
        GC_PUSH_1(val);
        
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
        
        GC_POP(); /* val */
        skip_whitespace(ctx);
    }
    
    if (peek(ctx) == ')') next(ctx);
    
done:
    GC_POP(); /* head, tail */
    return head;
}

/* Parse Symbol or Number */
static lisp_value read_atom(struct reader_context* ctx) {
    char buffer[256];
    int idx = 0;
    
    while (ctx->pos < ctx->len) {
        char c = peek(ctx);
        if (isspace(c) || c == ')' || c == '(') break;
        
        if (idx < 255) buffer[idx++] = next(ctx);
        else next(ctx);
    }
    buffer[idx] = '\0';
    
    /* Check if number */
    char* end;
    long long num = strtoll(buffer, &end, 10);
    if (*end == '\0') {
        return MAKE_FIXNUM(num);
    }
    
    return lisp_create_symbol(buffer);
}

/* Read string literal with escape sequence support */
static lisp_value read_string(struct reader_context* ctx) {
    next(ctx); /* Skip opening double quote */
    
    char buffer[1024];
    int idx = 0;
    
    while (ctx->pos < ctx->len) {
        char c = next(ctx);
        
        if (c == '"') {
            /* End of string */
            buffer[idx] = '\0';
            return lisp_create_string(buffer, idx);
        } else if (c == '\\') {
            /* Escape sequence */
            if (ctx->pos >= ctx->len) break;
            char escaped = next(ctx);
            switch (escaped) {
                case 'n':  buffer[idx++] = '\n'; break;
                case 't':  buffer[idx++] = '\t'; break;
                case 'r':  buffer[idx++] = '\r'; break;
                case '\\': buffer[idx++] = '\\'; break;
                case '"':  buffer[idx++] = '"';  break;
                case '0':  buffer[idx++] = '\0'; break;
                default:   buffer[idx++] = escaped; break;
            }
        } else if (c == '\0') {
            /* Unexpected end of input */
            break;
        } else {
            if (idx < 1023) buffer[idx++] = c;
        }
    }
    
    /* Unterminated string - return what we have */
    buffer[idx] = '\0';
    return lisp_create_string(buffer, idx);
}

/* Read object */
lisp_value reader_read(struct reader_context* ctx) {
    skip_whitespace(ctx);


    
    char c = peek(ctx);
    if (c == '(') {
        return read_list(ctx);
    } else if (c == '"') {
        /* String literal */
        return read_string(ctx);
    } else if (c == '\'') {
        next(ctx);
        lisp_value expr = reader_read(ctx);
        GC_PUSH_1(expr);
        lisp_value quote_sym = lisp_create_symbol("quote");
        GC_PUSH_1(quote_sym);
        
        lisp_value list = lisp_create_cons(expr, LISP_NIL);
        GC_PUSH_1(list);
        lisp_value result = lisp_create_cons(quote_sym, list);
        
        GC_POP(); /* list */
        GC_POP(); /* quote_sym */
        GC_POP(); /* expr */
        return result;
    } else if (c == 0) {
        return LISP_NIL; /* EOF */
    } else {
        return read_atom(ctx);
    }
}


/* Print object */
/* Print object */
void lisp_print(lisp_value obj) {
    if (IS_FIXNUM(obj)) {


        print_int(FIXNUM_VAL(obj));
    } else if (IS_NIL(obj)) {
        LISP_PUTS("nil");
    } else if (IS_T(obj)) {
        LISP_PUTS("t");
    } else if (IS_CONS(obj)) {
        LISP_PUTS("(");
        lisp_print(CAR(obj));
        lisp_value curr = CDR(obj);
        while (IS_CONS(curr)) {
            LISP_PUTS(" ");
            lisp_print(CAR(curr));
            curr = CDR(curr);
        }
        if (!IS_NIL(curr)) {
            LISP_PUTS(" . ");
            lisp_print(curr);
        }
        LISP_PUTS(")");
    } else if (IS_SYMBOL(obj)) {
        lisp_value name = SYMBOL_NAME(obj);
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(name);
        LISP_PUTS(s->data);
    } else if (IS_STRING(obj)) {
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(obj);
        LISP_PUTS("\"");
        LISP_PUTS(s->data);
        LISP_PUTS("\"");
    } else if (IS_FUNCTION(obj)) {
        LISP_PUTS("<function>");
    } else if (GET_TYPE(PTR_VAL(obj)) == TYPE_BUILTIN) {
        LISP_PUTS("<builtin>");
    } else if (GET_TYPE(PTR_VAL(obj)) == TYPE_ENV) {
        LISP_PUTS("<env>");
    } else {
        LISP_PUTS("?");
    }
}


