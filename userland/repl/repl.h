/* AstraLisp OS System REPL */

#ifndef REPL_H
#define REPL_H

#include <stdint.h>
#include <stddef.h>

/* Initialize REPL */
int repl_init(void);

/* Run REPL */
void repl_run(void);

/* Evaluate Lisp expression */
void* repl_eval(const char* input);

#endif /* REPL_H */
