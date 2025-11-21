/* AstraLisp OS Lisp Evaluator */

#ifndef EVALUATOR_H
#define EVALUATOR_H

#include "tagged.h"
#include "reader.h"
#include "objects.h"
#include "hashtable.h"
#include <stdbool.h>

/* Initialize evaluator */
int evaluator_init(void);

/* Create new environment */
lisp_value lisp_env_create(lisp_value parent);

/* Bind variable in environment */
void lisp_env_bind(lisp_value env, lisp_value symbol, lisp_value value);

/* Lookup variable in environment */
lisp_value lisp_env_lookup(lisp_value env, lisp_value symbol);

/* Set variable in environment */
void lisp_env_set(lisp_value env, lisp_value symbol, lisp_value value);

/* Evaluate expression */
lisp_value lisp_eval(lisp_value env, lisp_value expr);

/* Apply function */
lisp_value lisp_apply(lisp_value env, lisp_value func, lisp_value args);

/* Register built-in function */
typedef lisp_value (*lisp_builtin_fn)(lisp_value env, lisp_value args);
void lisp_register_builtin(const char* name, lisp_builtin_fn fn);

/* Built-in functions */
lisp_value builtin_car(lisp_value env, lisp_value args);
lisp_value builtin_cdr(lisp_value env, lisp_value args);
lisp_value builtin_cons(lisp_value env, lisp_value args);
lisp_value builtin_quote(lisp_value env, lisp_value args);
lisp_value builtin_atom(lisp_value env, lisp_value args);
lisp_value builtin_eq(lisp_value env, lisp_value args);
lisp_value builtin_cond(lisp_value env, lisp_value args);
lisp_value builtin_lambda(lisp_value env, lisp_value args);
lisp_value builtin_defun(lisp_value env, lisp_value args);
lisp_value builtin_setq(lisp_value env, lisp_value args);
lisp_value builtin_plus(lisp_value env, lisp_value args);
lisp_value builtin_minus(lisp_value env, lisp_value args);
lisp_value builtin_times(lisp_value env, lisp_value args);
lisp_value builtin_divide(lisp_value env, lisp_value args);
lisp_value builtin_print(lisp_value env, lisp_value args);

#endif /* EVALUATOR_H */
