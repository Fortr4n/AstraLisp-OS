/* AstraLisp OS Lisp Macro System Implementation */

#include "macro.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

/* Create macro expander */
struct macro_expander* macro_expander_create(struct lisp_environment* env) {
    struct macro_expander* expander = (struct macro_expander*)kmalloc(sizeof(struct macro_expander));
    if (!expander) {
        return NULL;
    }
    
    expander->macro_env = env_create(env);
    if (!expander->macro_env) {
        kfree(expander);
        return NULL;
    }
    
    return expander;
}

/* Check if form is macro call */
bool is_macro_call(struct lisp_object* expr, struct lisp_environment* env) {
    if (!expr || expr->type != LISP_CONS) {
        return false;
    }
    
    struct lisp_object* car = expr->value.cons.car;
    if (!car || car->type != LISP_SYMBOL) {
        return false;
    }
    
    struct lisp_object* macro = env_lookup(env, car);
    if (macro && macro->type == LISP_FUNCTION && macro->value.function.is_macro) {
        return true;
    }
    
    return false;
}

/* Apply macro */
struct lisp_object* macro_apply(struct lisp_object* macro_func, struct lisp_object* args, struct lisp_environment* env) {
    if (!macro_func || macro_func->type != LISP_FUNCTION || !macro_func->value.function.is_macro) {
        return NULL;
    }
    
    /* Create macro environment */
    struct lisp_environment* macro_env = env_create(macro_func->value.function.env);
    if (!macro_env) {
        return NULL;
    }
    
    /* Bind macro parameters */
    struct lisp_object* params = macro_func->value.function.params;
    struct lisp_object* arg = args;
    
    while (params && params->type == LISP_CONS && arg && arg->type == LISP_CONS) {
        struct lisp_object* param = params->value.cons.car;
        struct lisp_object* arg_val = arg->value.cons.car;
        
        if (param && param->type == LISP_SYMBOL) {
            env_define(macro_env, param, arg_val);
        }
        
        params = params->value.cons.cdr;
        arg = arg->value.cons.cdr;
    }
    
    /* Evaluate macro body */
    struct lisp_object* result = lisp_eval(macro_func->value.function.body, macro_env);
    
    return result;
}

/* Expand once */
struct lisp_object* macro_expand_once(struct macro_expander* expander, struct lisp_object* expr) {
    if (!expander || !expr) {
        return NULL;
    }
    
    if (expr->type != LISP_CONS) {
        lisp_incref(expr);
        return expr;
    }
    
    struct lisp_object* car = expr->value.cons.car;
    if (!car || car->type != LISP_SYMBOL) {
        /* Recursively expand elements */
        struct lisp_object* expanded_car = macro_expand_once(expander, car);
        struct lisp_object* cdr = expr->value.cons.cdr;
        struct lisp_object* expanded_cdr = NULL;
        if (cdr) {
            expanded_cdr = macro_expand_once(expander, cdr);
        }
        return lisp_create_cons(expanded_car, expanded_cdr);
    }
    
    /* Check if it's a macro call */
    if (is_macro_call(expr, expander->macro_env)) {
        struct lisp_object* macro_func = env_lookup(expander->macro_env, car);
        if (macro_func) {
            struct lisp_object* args = expr->value.cons.cdr;
            struct lisp_object* expanded = macro_apply(macro_func, args, expander->macro_env);
            if (expanded) {
                return expanded;
            }
        }
    }
    
    /* Not a macro - recursively expand */
    struct lisp_object* expanded_car = macro_expand_once(expander, car);
    struct lisp_object* cdr = expr->value.cons.cdr;
    struct lisp_object* expanded_cdr = NULL;
    if (cdr) {
        expanded_cdr = macro_expand_once(expander, cdr);
    }
    return lisp_create_cons(expanded_car, expanded_cdr);
}

/* Expand macros in expression */
struct lisp_object* macro_expand(struct macro_expander* expander, struct lisp_object* expr) {
    if (!expander || !expr) {
        return NULL;
    }
    
    struct lisp_object* current = expr;
    struct lisp_object* previous = NULL;
    int iterations = 0;
    const int max_iterations = 100;  /* Prevent infinite expansion */
    
    while (iterations < max_iterations) {
        struct lisp_object* expanded = macro_expand_once(expander, current);
        if (!expanded) {
            break;
        }
        
        if (previous && previous != expr) {
            lisp_decref(previous);
        }
        
        if (lisp_equal(current, expanded)) {
            /* No more expansion possible */
            if (previous && previous != expr) {
                lisp_decref(previous);
            }
            return expanded;
        }
        
        previous = current;
        current = expanded;
        iterations++;
    }
    
    return current;
}

/* Define macro */
int macro_define(struct macro_expander* expander, struct lisp_object* name, struct lisp_object* macro_func) {
    if (!expander || !name || !macro_func) {
        return -1;
    }
    
    if (macro_func->type == LISP_FUNCTION) {
        macro_func->value.function.is_macro = true;
    }
    
    return env_define(expander->macro_env, name, macro_func);
}

/* Initialize macro system */
int macro_init(void) {
    return 0;
}
