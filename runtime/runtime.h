/* AstraLisp OS Lisp Runtime */

#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>
#include <stddef.h>

/* Lisp object types */
#define LISP_TYPE_NIL 0
#define LISP_TYPE_INTEGER 1
#define LISP_TYPE_SYMBOL 2
#define LISP_TYPE_CONS 3
#define LISP_TYPE_STRING 4
#define LISP_TYPE_FUNCTION 5

/* Lisp object structure */
struct lisp_object {
    uint32_t type;
    union {
        int64_t integer;
        char* symbol;
        struct {
            struct lisp_object* car;
            struct lisp_object* cdr;
        } cons;
        char* string;
        void* function;
    } value;
};

/* Initialize runtime */
int runtime_init(void);

/* Evaluate Lisp expression */
struct lisp_object* runtime_eval(struct lisp_object* expr);

/* Read Lisp expression from string */
struct lisp_object* runtime_read(const char* input);

/* Print Lisp object */
void runtime_print(struct lisp_object* obj);

#endif /* RUNTIME_H */
