/* AstraLisp OS Lisp Macro System */

#ifndef MACRO_H
#define MACRO_H

#include "reader.h"
#include "evaluator.h"

/* Macro expander */
struct macro_expander {
    struct lisp_environment* macro_env;
};

/* Create macro expander */
struct macro_expander* macro_expander_create(struct lisp_environment* env);

/* Expand macros in expression */
struct lisp_object* macro_expand(struct macro_expander* expander, struct lisp_object* expr);

/* Expand once */
struct lisp_object* macro_expand_once(struct macro_expander* expander, struct lisp_object* expr);

/* Define macro */
int macro_define(struct macro_expander* expander, struct lisp_object* name, struct lisp_object* macro_func);

/* Check if form is macro call */
bool is_macro_call(struct lisp_object* expr, struct lisp_environment* env);

/* Apply macro */
struct lisp_object* macro_apply(struct lisp_object* macro_func, struct lisp_object* args, struct lisp_environment* env);

/* Initialize macro system */
int macro_init(void);

#endif /* MACRO_H */
