/* AstraLisp OS Lisp Reader */

#ifndef READER_H
#define READER_H

#include "tagged.h"
#include <stddef.h>
#include <stdbool.h>

/* Reader context */
struct reader_context {
    const char* input;
    size_t pos;     /* Current position */
    size_t len;     /* Total length */
    int line;
    int column;
    lisp_value symbols;  /* Symbol table (list of symbols) */
};

/* Initialize reader */
void reader_init(struct reader_context* ctx, const char* input);

/* Read object from input */
lisp_value reader_read(struct reader_context* ctx);

/* Create object functions (Factories) */
lisp_value lisp_create_cons(lisp_value car, lisp_value cdr);
lisp_value lisp_create_symbol(const char* name);
lisp_value lisp_create_string(const char* data, size_t length);
lisp_value lisp_create_vector(size_t length);
lisp_value lisp_create_function(lisp_value code, lisp_value env, lisp_value args);

/* Print object */
void lisp_print(lisp_value obj);

#endif /* READER_H */
