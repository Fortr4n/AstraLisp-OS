/* AstraLisp OS Lisp Reader Implementation */

#include "reader.h"
#include "../../kernel/mm/heap.h"
#include "../../runtime/gc/gc.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdint.h>

static struct lisp_object* nil_object = NULL;

/* Hash function for symbols */
static uint32_t hash_string(const char* str) {
    uint32_t hash = 5381;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash;
}

/* Create nil */
struct lisp_object* lisp_nil(void) {
    if (!nil_object) {
        nil_object = lisp_create_object(LISP_NIL);
        if (nil_object) {
            nil_object->ref_count = 999999;  /* Never free nil */
        }
    }
    return nil_object;
}

/* Create object */
struct lisp_object* lisp_create_object(lisp_type_t type) {
    struct lisp_object* obj = (struct lisp_object*)gc_alloc(sizeof(struct lisp_object));
    if (!obj) {
        return NULL;
    }
    
    memset(obj, 0, sizeof(struct lisp_object));
    obj->type = type;
    obj->ref_count = 1;
    
    return obj;
}

/* Create integer */
struct lisp_object* lisp_create_integer(int64_t value) {
    struct lisp_object* obj = lisp_create_object(LISP_INTEGER);
    if (!obj) {
        return NULL;
    }
    
    obj->value.integer = value;
    return obj;
}

/* Create symbol */
struct lisp_object* lisp_create_symbol(const char* name) {
    if (!name) {
        return lisp_nil();
    }
    
    struct lisp_object* obj = lisp_create_object(LISP_SYMBOL);
    if (!obj) {
        return NULL;
    }
    
    size_t name_len = strlen(name);
    obj->value.symbol.name = (char*)kmalloc(name_len + 1);
    if (!obj->value.symbol.name) {
        lisp_decref(obj);
        return NULL;
    }
    
    strcpy(obj->value.symbol.name, name);
    obj->value.symbol.hash = hash_string(name);
    
    return obj;
}

/* Create string */
struct lisp_object* lisp_create_string(const char* data, size_t length) {
    struct lisp_object* obj = lisp_create_object(LISP_STRING);
    if (!obj) {
        return NULL;
    }
    
    obj->value.string.data = (char*)kmalloc(length + 1);
    if (!obj->value.string.data) {
        lisp_decref(obj);
        return NULL;
    }
    
    memcpy(obj->value.string.data, data, length);
    obj->value.string.data[length] = '\0';
    obj->value.string.length = length;
    
    return obj;
}

/* Create cons */
struct lisp_object* lisp_create_cons(struct lisp_object* car, struct lisp_object* cdr) {
    struct lisp_object* obj = lisp_create_object(LISP_CONS);
    if (!obj) {
        return NULL;
    }
    
    obj->value.cons.car = car;
    if (car) {
        lisp_incref(car);
    }
    
    obj->value.cons.cdr = cdr;
    if (cdr) {
        lisp_incref(cdr);
    }
    
    return obj;
}

/* Increment reference count */
void lisp_incref(struct lisp_object* obj) {
    if (obj && obj != nil_object) {
        obj->ref_count++;
    }
}

/* Decrement reference count */
void lisp_decref(struct lisp_object* obj) {
    if (!obj || obj == nil_object) {
        return;
    }
    
    obj->ref_count--;
    if (obj->ref_count == 0) {
        /* Free object */
        switch (obj->type) {
            case LISP_SYMBOL:
                if (obj->value.symbol.name) {
                    kfree(obj->value.symbol.name);
                }
                break;
            case LISP_STRING:
                if (obj->value.string.data) {
                    kfree(obj->value.string.data);
                }
                break;
            case LISP_CONS:
                lisp_decref(obj->value.cons.car);
                lisp_decref(obj->value.cons.cdr);
                break;
            case LISP_VECTOR:
                if (obj->value.vector.elements) {
                    for (size_t i = 0; i < obj->value.vector.length; i++) {
                        lisp_decref(obj->value.vector.elements[i]);
                    }
                    kfree(obj->value.vector.elements);
                }
                break;
            case LISP_FUNCTION:
                lisp_decref(obj->value.function.params);
                lisp_decref(obj->value.function.body);
                lisp_decref(obj->value.function.env);
                break;
            case LISP_ERROR:
                if (obj->value.error.message) {
                    kfree(obj->value.error.message);
                }
                lisp_decref(obj->value.error.data);
                break;
            default:
                break;
        }
        kfree(obj);
    }
}

/* Initialize reader */
int reader_init(struct reader_context* ctx, const char* input) {
    if (!ctx || !input) {
        return -1;
    }
    
    ctx->input = input;
    ctx->position = 0;
    ctx->length = strlen(input);
    ctx->line = 1;
    ctx->column = 1;
    ctx->symbols = lisp_nil();
    
    return 0;
}

/* Peek character */
char reader_peek(struct reader_context* ctx) {
    if (!ctx || ctx->position >= ctx->length) {
        return '\0';
    }
    return ctx->input[ctx->position];
}

/* Consume character */
char reader_consume(struct reader_context* ctx) {
    if (!ctx || ctx->position >= ctx->length) {
        return '\0';
    }
    
    char c = ctx->input[ctx->position++];
    if (c == '\n') {
        ctx->line++;
        ctx->column = 1;
    } else {
        ctx->column++;
    }
    
    return c;
}

/* Check if end of input */
bool reader_eof(struct reader_context* ctx) {
    if (!ctx) {
        return true;
    }
    return ctx->position >= ctx->length;
}

/* Skip whitespace */
void reader_skip_whitespace(struct reader_context* ctx) {
    if (!ctx) {
        return;
    }
    
    while (!reader_eof(ctx)) {
        char c = reader_peek(ctx);
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            reader_consume(ctx);
        } else if (c == ';') {
            /* Skip comment */
            while (!reader_eof(ctx) && reader_peek(ctx) != '\n') {
                reader_consume(ctx);
            }
        } else {
            break;
        }
    }
}

/* Read number */
struct lisp_object* reader_read_number(struct reader_context* ctx) {
    if (!ctx) {
        return NULL;
    }
    
    bool negative = false;
    if (reader_peek(ctx) == '-') {
        negative = true;
        reader_consume(ctx);
    }
    
    int64_t value = 0;
    bool has_digits = false;
    
    while (!reader_eof(ctx)) {
        char c = reader_peek(ctx);
        if (isdigit(c)) {
            value = value * 10 + (c - '0');
            reader_consume(ctx);
            has_digits = true;
        } else {
            break;
        }
    }
    
    if (!has_digits) {
        return NULL;
    }
    
    if (negative) {
        value = -value;
    }
    
    return lisp_create_integer(value);
}

/* Read string */
struct lisp_object* reader_read_string(struct reader_context* ctx) {
    if (!ctx || reader_peek(ctx) != '"') {
        return NULL;
    }
    
    reader_consume(ctx);  /* Consume opening quote */
    
    char buffer[4096];
    size_t len = 0;
    bool escaped = false;
    
    while (!reader_eof(ctx)) {
        char c = reader_consume(ctx);
        
        if (escaped) {
            switch (c) {
                case 'n': buffer[len++] = '\n'; break;
                case 't': buffer[len++] = '\t'; break;
                case 'r': buffer[len++] = '\r'; break;
                case '\\': buffer[len++] = '\\'; break;
                case '"': buffer[len++] = '"'; break;
                default: buffer[len++] = c; break;
            }
            escaped = false;
        } else if (c == '\\') {
            escaped = true;
        } else if (c == '"') {
            break;
        } else {
            buffer[len++] = c;
        }
        
        if (len >= sizeof(buffer) - 1) {
            break;
        }
    }
    
    return lisp_create_string(buffer, len);
}

/* Read symbol */
struct lisp_object* reader_read_symbol(struct reader_context* ctx) {
    if (!ctx) {
        return NULL;
    }
    
    char buffer[256];
    size_t len = 0;
    
    while (!reader_eof(ctx) && len < sizeof(buffer) - 1) {
        char c = reader_peek(ctx);
        if (isalnum(c) || c == '-' || c == '_' || c == '+' || c == '*' || 
            c == '/' || c == '=' || c == '<' || c == '>' || c == '?') {
            buffer[len++] = reader_consume(ctx);
        } else {
            break;
        }
    }
    
    if (len == 0) {
        return NULL;
    }
    
    buffer[len] = '\0';
    
    /* Check for special symbols */
    if (strcmp(buffer, "nil") == 0 || strcmp(buffer, "()") == 0) {
        return lisp_nil();
    }
    if (strcmp(buffer, "t") == 0) {
        return lisp_create_integer(1);  /* True */
    }
    
    return lisp_create_symbol(buffer);
}

/* Read list */
struct lisp_object* reader_read_list(struct reader_context* ctx) {
    if (!ctx || reader_peek(ctx) != '(') {
        return NULL;
    }
    
    reader_consume(ctx);  /* Consume opening paren */
    reader_skip_whitespace(ctx);
    
    if (reader_peek(ctx) == ')') {
        reader_consume(ctx);
        return lisp_nil();
    }
    
    struct lisp_object* first = reader_read(ctx);
    if (!first) {
        return NULL;
    }
    
    reader_skip_whitespace(ctx);
    
    if (reader_peek(ctx) == '.') {
        /* Dotted pair */
        reader_consume(ctx);
        reader_skip_whitespace(ctx);
        struct lisp_object* cdr = reader_read(ctx);
        reader_skip_whitespace(ctx);
        if (reader_peek(ctx) != ')') {
            lisp_decref(first);
            lisp_decref(cdr);
            return NULL;
        }
        reader_consume(ctx);
        return lisp_create_cons(first, cdr);
    }
    
    struct lisp_object* rest = reader_read_list(ctx);
    return lisp_create_cons(first, rest);
}

/* Read object from input */
struct lisp_object* reader_read(struct reader_context* ctx) {
    if (!ctx) {
        return NULL;
    }
    
    reader_skip_whitespace(ctx);
    
    if (reader_eof(ctx)) {
        return NULL;
    }
    
    char c = reader_peek(ctx);
    
    if (c == '(') {
        return reader_read_list(ctx);
    } else if (c == '"') {
        return reader_read_string(ctx);
    } else if (c == '\'') {
        /* Quote */
        reader_consume(ctx);
        struct lisp_object* quoted = reader_read(ctx);
        if (!quoted) {
            return NULL;
        }
        struct lisp_object* quote_symbol = lisp_create_symbol("quote");
        return lisp_create_cons(quote_symbol, lisp_create_cons(quoted, lisp_nil()));
    } else if (isdigit(c) || c == '-') {
        return reader_read_number(ctx);
    } else {
        return reader_read_symbol(ctx);
    }
}

/* Print object */
void lisp_print(struct lisp_object* obj) {
    if (!obj) {
        return;
    }
    
    switch (obj->type) {
        case LISP_NIL:
            /* Print nothing or "nil" */
            break;
        case LISP_INTEGER:
            /* Print integer (would use printf) */
            break;
        case LISP_FLOAT:
            /* Print float */
            break;
        case LISP_SYMBOL:
            /* Print symbol name */
            break;
        case LISP_STRING:
            /* Print string */
            break;
        case LISP_CONS:
            /* Print list */
            break;
        case LISP_VECTOR:
            /* Print vector */
            break;
        case LISP_FUNCTION:
            /* Print function */
            break;
        case LISP_ERROR:
            /* Print error */
            break;
        default:
            break;
    }
}

/* Object equality */
bool lisp_equal(struct lisp_object* a, struct lisp_object* b) {
    if (a == b) {
        return true;
    }
    
    if (!a || !b) {
        return false;
    }
    
    if (a->type != b->type) {
        return false;
    }
    
    switch (a->type) {
        case LISP_NIL:
            return true;
        case LISP_INTEGER:
            return a->value.integer == b->value.integer;
        case LISP_SYMBOL:
            return strcmp(a->value.symbol.name, b->value.symbol.name) == 0;
        case LISP_STRING:
            return a->value.string.length == b->value.string.length &&
                   memcmp(a->value.string.data, b->value.string.data, a->value.string.length) == 0;
        case LISP_CONS:
            return lisp_equal(a->value.cons.car, b->value.cons.car) &&
                   lisp_equal(a->value.cons.cdr, b->value.cons.cdr);
        default:
            return false;
    }
}
