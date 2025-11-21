/* AstraLisp OS Evaluator Implementation */

#include "evaluator.h"
#include "objects.h"
#include "types.h"
#include "hashtable.h"
#include "../gc/gc.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Global environment */
static lisp_value global_env = LISP_NIL;

/* Helper predicate for environment type */
static inline bool is_env(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_ENV;
}

#define IS_ENV(x) is_env(x)

/* Initialize evaluator */
int evaluator_init(void) {
    /* Create global environment */
    /* We need to register global_env as a root since it persists */
    gc_add_root(&global_env);
    
    global_env = lisp_env_create(LISP_NIL);
    
    /* Register built-ins */
    lisp_register_builtin("car", builtin_car);
    lisp_register_builtin("cdr", builtin_cdr);
    lisp_register_builtin("cons", builtin_cons);
    lisp_register_builtin("quote", builtin_quote);
    lisp_register_builtin("atom", builtin_atom);
    lisp_register_builtin("eq", builtin_eq);
    lisp_register_builtin("cond", builtin_cond);
    lisp_register_builtin("lambda", builtin_lambda);
    lisp_register_builtin("defun", builtin_defun);
    lisp_register_builtin("setq", builtin_setq);
    lisp_register_builtin("+", builtin_plus);
    lisp_register_builtin("-", builtin_minus);
    lisp_register_builtin("*", builtin_times);
    lisp_register_builtin("/", builtin_divide);
    lisp_register_builtin("print", builtin_print);
    
    /* Register T and NIL in global env */
    lisp_env_bind(global_env, lisp_create_symbol("t"), LISP_T);
    lisp_env_bind(global_env, lisp_create_symbol("nil"), LISP_NIL);
    
    return 0;
}

/* Create new environment */
lisp_value lisp_env_create(lisp_value parent) {
    struct lisp_env* env = (struct lisp_env*)gc_alloc(sizeof(struct lisp_env));
    if (!env) return LISP_NIL;
    
    SET_TYPE(&env->header, TYPE_ENV);
    
    env->parent = parent;
    env->table = ht_create(16); /* Initial capacity */
    
    lisp_value val = PTR_TO_VAL(env);
    gc_write_barrier(val, &env->parent, parent);
    /* Note: table is not a Lisp object, so no write barrier needed for the pointer itself,
       but its contents need tracing. */
       
    return val;
}

/* Bind variable in environment */
void lisp_env_bind(lisp_value env, lisp_value symbol, lisp_value value) {
    if (!IS_ENV(env)) return;
    
    struct lisp_env* e = (struct lisp_env*)PTR_VAL(env);
    if (!e->table) return;
    
    ht_put(e->table, symbol, value);
    /* Write barrier? The table is internal. The GC needs to scan the table. 
       Since we modified the table which is reachable from 'env', and 'value' is new,
       we should technically trigger a barrier if 'env' is old and 'value' is young.
       But 'table' is opaque to the write barrier function unless we expose it.
       
       For now, we rely on the fact that if 'env' is old, we might need to mark it dirty?
       Or we just treat the table as part of the object.
       
       Actually, since 'table' is malloc'd separately (not gc_alloc'd), it's not in the heap.
       So the write barrier logic for "Old Object -> Young Object" applies to 'env' -> 'value'.
       But 'value' is inside 'table'.
       
       We should probably manually call write barrier for 'env' if we can't point to the specific slot.
       Or, we can just assume the GC scans 'env' -> 'table' -> 'value'.
       If 'env' is OLD, and we write a YOUNG 'value' into 'table', we need to record 'env' in remembered set.
       
       Let's conservatively call gc_write_barrier with a dummy slot or just add to remembered set directly?
       gc_write_barrier(env, NULL, value) might work if we update it to handle NULL field.
       
       Let's just call it with &e->parent as a proxy, or better, expose a way to mark object dirty.
       For now, I'll use a hack: pass &e->parent even though we aren't writing there, 
       just to trigger the check "is env old? is value young?".
       Wait, gc_write_barrier checks address of field to see if it's within the object? 
       No, it usually just checks if obj is old and new_val is young.
       
       Let's check gc_write_barrier implementation in gc.c (I viewed it earlier).
       It takes (obj, field, new_value). It doesn't seem to verify field is inside obj, 
       but it might use it for card marking if we had card marking.
       We have card marking? "mark_card_dirty(obj_ptr)".
       So passing &e->parent is safe enough to mark the object dirty.
    */
    gc_write_barrier(env, &e->parent, value);
}

/* Lookup variable in environment */
lisp_value lisp_env_lookup(lisp_value env, lisp_value symbol) {
    while (IS_ENV(env)) {
        struct lisp_env* e = (struct lisp_env*)PTR_VAL(env);
        
        if (e->table && ht_contains(e->table, symbol)) {
            return ht_get(e->table, symbol);
        }
        
        env = e->parent;
    }
    
    /* Not found */
    printf("Error: Unbound variable: ");
    lisp_print(symbol);
    printf("\n");
    return LISP_NIL;
}

/* Set variable in environment */
void lisp_env_set(lisp_value env, lisp_value symbol, lisp_value value) {
    lisp_value curr = env;
    while (IS_ENV(curr)) {
        struct lisp_env* e = (struct lisp_env*)PTR_VAL(curr);
        
        if (e->table && ht_contains(e->table, symbol)) {
            ht_put(e->table, symbol, value);
            gc_write_barrier(curr, &e->parent, value); /* Mark dirty */
            return;
        }
        
        curr = e->parent;
    }
    
    /* Not found */
    printf("Error: Cannot set unbound variable: ");
    lisp_print(symbol);
    printf("\n");
}

/* Apply function */
lisp_value lisp_apply(lisp_value env, lisp_value func, lisp_value args) {
    /* GC Protection for inputs */
    GC_PUSH_3(env, func, args);
    
    lisp_value result = LISP_NIL;
    
    if (IS_FUNCTION(func)) {
        struct lisp_function* f = (struct lisp_function*)PTR_VAL(func);
        lisp_value params = f->args;
        lisp_value body = f->code;
        lisp_value closure_env = f->env;
        
        /* Create new environment extending closure_env */
        lisp_value new_env = lisp_env_create(closure_env);
        GC_PUSH_1(new_env); /* Protect new_env */
        
        /* Bind arguments */
        lisp_value p = params;
        lisp_value a = args;
        
        while (IS_CONS(p) && IS_CONS(a)) {
            lisp_env_bind(new_env, CAR(p), CAR(a));
            p = CDR(p);
            a = CDR(a);
        }
        
        /* Evaluate body */
        while (IS_CONS(body)) {
            result = lisp_eval(new_env, CAR(body));
            body = CDR(body);
        }
        
        GC_POP(); /* new_env */
    } else {
        printf("Error: Not a function: ");
        lisp_print(func);
        printf("\n");
    }
    
    GC_POP(); /* env, func, args */
    return result;
}

/* Evaluate expression */
lisp_value lisp_eval(lisp_value env, lisp_value expr) {
    /* GC Protection */
    GC_PUSH_2(env, expr);
    
    lisp_value result = LISP_NIL;
    
    if (IS_SYMBOL(expr)) {
        result = lisp_env_lookup(env, expr);
    } else if (IS_CONS(expr)) {
        lisp_value func_sym = CAR(expr);
        lisp_value args = CDR(expr);
        
        /* Check for special forms */
        if (IS_SYMBOL(func_sym)) {
            lisp_value name = SYMBOL_NAME(func_sym);
            struct lisp_string* s = (struct lisp_string*)PTR_VAL(name);
            
            if (strcmp(s->data, "quote") == 0) {
                if (IS_CONS(args)) {
                    result = CAR(args);
                }
                goto done;
            } else if (strcmp(s->data, "if") == 0) {
                lisp_value cond = CAR(args);
                lisp_value then_branch = CAR(CDR(args));
                lisp_value else_branch = LISP_NIL;
                if (IS_CONS(CDR(CDR(args)))) {
                    else_branch = CAR(CDR(CDR(args)));
                }
                
                lisp_value cond_val = lisp_eval(env, cond);
                GC_PUSH_1(cond_val);
                
                if (!IS_NIL(cond_val)) {
                    result = lisp_eval(env, then_branch);
                } else {
                    result = lisp_eval(env, else_branch);
                }
                
                GC_POP();
                goto done;
            } else if (strcmp(s->data, "lambda") == 0) {
                lisp_value params = CAR(args);
                lisp_value body = CDR(args);
                result = lisp_create_function(body, env, params);
                goto done;
            } else if (strcmp(s->data, "defun") == 0) {
                lisp_value name_sym = CAR(args);
                lisp_value params = CAR(CDR(args));
                lisp_value body = CDR(CDR(args));
                
                lisp_value func = lisp_create_function(body, env, params);
                GC_PUSH_1(func);
                
                lisp_env_bind(env, name_sym, func);
                
                struct lisp_function* f = (struct lisp_function*)PTR_VAL(func);
                f->name = name_sym;
                gc_write_barrier(func, &f->name, name_sym);
                
                result = name_sym;
                GC_POP();
                goto done;
            } else if (strcmp(s->data, "setq") == 0) {
                lisp_value var = CAR(args);
                lisp_value val_expr = CAR(CDR(args));
                lisp_value val = lisp_eval(env, val_expr);
                GC_PUSH_1(val);
                lisp_env_set(env, var, val);
                result = val;
                GC_POP();
                goto done;
            }
        }
        
        /* Function application */
        lisp_value func = lisp_eval(env, func_sym);
        GC_PUSH_1(func);
        
        /* Evaluate arguments */
        lisp_value eval_args = LISP_NIL;
        
        if (IS_NIL(args)) {
            eval_args = LISP_NIL;
        } else {
            lisp_value head = LISP_NIL;
            lisp_value tail = LISP_NIL;
            
            lisp_value curr = args;
            while (IS_CONS(curr)) {
                lisp_value v = lisp_eval(env, CAR(curr));
                GC_PUSH_1(v); 
                
                lisp_value new_cons = lisp_create_cons(v, LISP_NIL);
                if (IS_NIL(head)) {
                    head = new_cons;
                    tail = new_cons;
                } else {
                    struct lisp_cons* c = (struct lisp_cons*)PTR_VAL(tail);
                    c->cdr = new_cons;
                    gc_write_barrier(tail, &c->cdr, new_cons);
                    tail = new_cons;
                }
                
                GC_POP(); 
                curr = CDR(curr);
            }
            eval_args = head;
        }
        
        GC_PUSH_1(eval_args);
        
        if (IS_FUNCTION(func)) {
            result = lisp_apply(env, func, eval_args);
        } else if (IS_SYMBOL(func_sym)) {
             /* Builtin dispatch (same as before) */
             lisp_value name = SYMBOL_NAME(func_sym);
             struct lisp_string* s = (struct lisp_string*)PTR_VAL(name);
             
             if (strcmp(s->data, "car") == 0) result = builtin_car(env, eval_args);
             else if (strcmp(s->data, "cdr") == 0) result = builtin_cdr(env, eval_args);
             else if (strcmp(s->data, "cons") == 0) result = builtin_cons(env, eval_args);
             else if (strcmp(s->data, "+") == 0) result = builtin_plus(env, eval_args);
             else if (strcmp(s->data, "-") == 0) result = builtin_minus(env, eval_args);
             else if (strcmp(s->data, "*") == 0) result = builtin_times(env, eval_args);
             else if (strcmp(s->data, "/") == 0) result = builtin_divide(env, eval_args);
             else if (strcmp(s->data, "print") == 0) result = builtin_print(env, eval_args);
             else if (strcmp(s->data, "eq") == 0) result = builtin_eq(env, eval_args);
             else if (strcmp(s->data, "atom") == 0) result = builtin_atom(env, eval_args);
             else {
                 printf("Error: Unknown function: ");
                 lisp_print(func);
                 printf("\n");
             }
        } else {
            printf("Error: Invalid function call\n");
        }
        
        GC_POP(); /* eval_args */
        GC_POP(); /* func */
        
    } else {
        /* Self-evaluating */
        result = expr;
    }
    
done:
    GC_POP(); /* env, expr */
    return result;
}

/* Built-in implementations (Same as before) */
lisp_value builtin_car(lisp_value env, lisp_value args) {
    if (IS_CONS(args)) {
        lisp_value list = CAR(args);
        if (IS_CONS(list)) return CAR(list);
    }
    return LISP_NIL;
}

lisp_value builtin_cdr(lisp_value env, lisp_value args) {
    if (IS_CONS(args)) {
        lisp_value list = CAR(args);
        if (IS_CONS(list)) return CDR(list);
    }
    return LISP_NIL;
}

lisp_value builtin_cons(lisp_value env, lisp_value args) {
    if (IS_CONS(args) && IS_CONS(CDR(args))) {
        lisp_value car = CAR(args);
        lisp_value cdr = CAR(CDR(args));
        return lisp_create_cons(car, cdr);
    }
    return LISP_NIL;
}

lisp_value builtin_quote(lisp_value env, lisp_value args) {
    if (IS_CONS(args)) return CAR(args);
    return LISP_NIL;
}

lisp_value builtin_atom(lisp_value env, lisp_value args) {
    if (IS_CONS(args)) {
        lisp_value val = CAR(args);
        return IS_CONS(val) ? LISP_NIL : LISP_T;
    }
    return LISP_T;
}

lisp_value builtin_eq(lisp_value env, lisp_value args) {
    if (IS_CONS(args) && IS_CONS(CDR(args))) {
        lisp_value a = CAR(args);
        lisp_value b = CAR(CDR(args));
        return (a == b) ? LISP_T : LISP_NIL;
    }
    return LISP_NIL;
}

lisp_value builtin_cond(lisp_value env, lisp_value args) {
    return LISP_NIL;
}

lisp_value builtin_lambda(lisp_value env, lisp_value args) {
    return LISP_NIL;
}

lisp_value builtin_defun(lisp_value env, lisp_value args) {
    return LISP_NIL;
}

lisp_value builtin_setq(lisp_value env, lisp_value args) {
    return LISP_NIL;
}

lisp_value builtin_plus(lisp_value env, lisp_value args) {
    int64_t sum = 0;
    while (IS_CONS(args)) {
        lisp_value val = CAR(args);
        if (IS_FIXNUM(val)) {
            sum += FIXNUM_VAL(val);
        }
        args = CDR(args);
    }
    return MAKE_FIXNUM(sum);
}

lisp_value builtin_minus(lisp_value env, lisp_value args) {
    if (!IS_CONS(args)) return MAKE_FIXNUM(0);
    
    lisp_value first = CAR(args);
    int64_t val = IS_FIXNUM(first) ? FIXNUM_VAL(first) : 0;
    
    args = CDR(args);
    if (IS_NIL(args)) {
        return MAKE_FIXNUM(-val);
    }
    
    while (IS_CONS(args)) {
        lisp_value v = CAR(args);
        if (IS_FIXNUM(v)) {
            val -= FIXNUM_VAL(v);
        }
        args = CDR(args);
    }
    return MAKE_FIXNUM(val);
}

lisp_value builtin_times(lisp_value env, lisp_value args) {
    int64_t prod = 1;
    while (IS_CONS(args)) {
        lisp_value val = CAR(args);
        if (IS_FIXNUM(val)) {
            prod *= FIXNUM_VAL(val);
        }
        args = CDR(args);
    }
    return MAKE_FIXNUM(prod);
}

lisp_value builtin_divide(lisp_value env, lisp_value args) {
    if (!IS_CONS(args)) return MAKE_FIXNUM(1);
    
    lisp_value first = CAR(args);
    int64_t val = IS_FIXNUM(first) ? FIXNUM_VAL(first) : 1;
    
    args = CDR(args);
    while (IS_CONS(args)) {
        lisp_value v = CAR(args);
        if (IS_FIXNUM(v)) {
            int64_t divisor = FIXNUM_VAL(v);
            if (divisor != 0) val /= divisor;
        }
        args = CDR(args);
    }
    return MAKE_FIXNUM(val);
}

lisp_value builtin_print(lisp_value env, lisp_value args) {
    while (IS_CONS(args)) {
        lisp_print(CAR(args));
        printf("\n");
        args = CDR(args);
    }
    return LISP_NIL;
}

void lisp_register_builtin(const char* name, lisp_builtin_fn fn) {
    /* Placeholder */
}
