/* AstraLisp OS Lisp Evaluator */

#ifndef EVALUATOR_H
#define EVALUATOR_H

#include "reader.h"
#include <stdbool.h>

/* Environment (binding frame) */
struct lisp_environment {
    struct lisp_object* bindings;  /* Association list */
    struct lisp_environment* parent;
};

/* Create environment */
struct lisp_environment* env_create(struct lisp_environment* parent);

/* Lookup variable */
struct lisp_object* env_lookup(struct lisp_environment* env, struct lisp_object* symbol);

/* Define variable */
int env_define(struct lisp_environment* env, struct lisp_object* symbol, struct lisp_object* value);

/* Set variable */
int env_set(struct lisp_environment* env, struct lisp_object* symbol, struct lisp_object* value);

/* Evaluate expression */
struct lisp_object* lisp_eval(struct lisp_object* expr, struct lisp_environment* env);

/* Apply function */
struct lisp_object* lisp_apply(struct lisp_object* func, struct lisp_object* args, struct lisp_environment* env);

/* Built-in functions */
struct lisp_object* lisp_builtin_car(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_cdr(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_cons(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_eq(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_add(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_sub(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_mul(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_div(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_list(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_length(struct lisp_object* args, struct lisp_environment* env);
struct lisp_object* lisp_builtin_print(struct lisp_object* args, struct lisp_environment* env);

/* Register built-in */
void lisp_register_builtin(const char* name, struct lisp_object* (*func)(struct lisp_object*, struct lisp_environment*));

/* Initialize evaluator */
int evaluator_init(void);

#endif /* EVALUATOR_H */
