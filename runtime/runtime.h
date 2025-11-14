/* AstraLisp OS Lisp Runtime */

#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>
#include <stddef.h>

/* Forward declaration */
struct lisp_object;

/* Initialize runtime */
int runtime_init(void);

/* Evaluate Lisp expression */
struct lisp_object* runtime_eval(struct lisp_object* expr);

/* Read Lisp expression from string */
struct lisp_object* runtime_read(const char* input);

/* Print Lisp object */
void runtime_print(struct lisp_object* obj);

#endif /* RUNTIME_H */
