/* AstraLisp OS Lisp Reader */

#ifndef READER_H
#define READER_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Lisp object types */
typedef enum {
    LISP_NIL,
    LISP_INTEGER,
    LISP_FLOAT,
    LISP_SYMBOL,
    LISP_STRING,
    LISP_CONS,
    LISP_VECTOR,
    LISP_FUNCTION,
    LISP_MACRO,
    LISP_SPECIAL_FORM,
    LISP_ERROR
} lisp_type_t;

/* Lisp object */
struct lisp_object {
    lisp_type_t type;
    uint32_t ref_count;
    union {
        int64_t integer;
        double float_val;
        struct {
            char* name;
            uint32_t hash;
        } symbol;
        struct {
            char* data;
            size_t length;
        } string;
        struct {
            struct lisp_object* car;
            struct lisp_object* cdr;
        } cons;
        struct {
            struct lisp_object** elements;
            size_t length;
            size_t capacity;
        } vector;
        struct {
            struct lisp_object* params;
            struct lisp_object* body;
            struct lisp_object* env;
            bool is_macro;
        } function;
        struct {
            char* message;
            struct lisp_object* data;
        } error;
    } value;
};

/* Reader context */
struct reader_context {
    const char* input;
    size_t position;
    size_t length;
    int line;
    int column;
    struct lisp_object* symbols;  /* Symbol table */
};

/* Initialize reader */
int reader_init(struct reader_context* ctx, const char* input);

/* Read object from input */
struct lisp_object* reader_read(struct reader_context* ctx);

/* Read list */
struct lisp_object* reader_read_list(struct reader_context* ctx);

/* Read symbol */
struct lisp_object* reader_read_symbol(struct reader_context* ctx);

/* Read string */
struct lisp_object* reader_read_string(struct reader_context* ctx);

/* Read number */
struct lisp_object* reader_read_number(struct reader_context* ctx);

/* Skip whitespace */
void reader_skip_whitespace(struct reader_context* ctx);

/* Peek character */
char reader_peek(struct reader_context* ctx);

/* Consume character */
char reader_consume(struct reader_context* ctx);

/* Check if end of input */
bool reader_eof(struct reader_context* ctx);

/* Create object */
struct lisp_object* lisp_create_object(lisp_type_t type);

/* Create integer */
struct lisp_object* lisp_create_integer(int64_t value);

/* Create symbol */
struct lisp_object* lisp_create_symbol(const char* name);

/* Create string */
struct lisp_object* lisp_create_string(const char* data, size_t length);

/* Create cons */
struct lisp_object* lisp_create_cons(struct lisp_object* car, struct lisp_object* cdr);

/* Create nil */
struct lisp_object* lisp_nil(void);

/* Increment reference count */
void lisp_incref(struct lisp_object* obj);

/* Decrement reference count */
void lisp_decref(struct lisp_object* obj);

/* Print object */
void lisp_print(struct lisp_object* obj);

/* Object equality */
bool lisp_equal(struct lisp_object* a, struct lisp_object* b);

#endif /* READER_H */
