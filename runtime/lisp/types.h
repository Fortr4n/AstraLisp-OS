/* AstraLisp OS Lisp Type System */

#ifndef TYPES_H
#define TYPES_H

#include "tagged.h"
#include "objects.h"
#include <stdbool.h>

/* Type predicates (Inline functions) */
static inline bool is_cons(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_CONS;
}

static inline bool is_symbol(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_SYMBOL;
}

static inline bool is_string(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_STRING;
}

static inline bool is_vector(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_VECTOR;
}

static inline bool is_function(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_FUNCTION;
}

static inline bool is_builtin(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_BUILTIN;
}

static inline bool is_thread(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_THREAD;
}

static inline bool is_vm_closure(lisp_value obj) {
    return IS_POINTER(obj) && GET_TYPE(PTR_VAL(obj)) == TYPE_VM_CLOSURE;
}

static inline bool is_integer(lisp_value obj) {
    return IS_FIXNUM(obj);
}

static inline bool is_character(lisp_value obj) {
    return IS_CHAR(obj);
}

static inline bool is_nil(lisp_value obj) {
    return IS_NIL(obj);
}

static inline bool is_t(lisp_value obj) {
    return IS_T(obj);
}

/* Uppercase Macros for consistency/legacy support */
#define IS_CONS(x)      is_cons(x)
#define IS_SYMBOL(x)    is_symbol(x)
#define IS_STRING(x)    is_string(x)
#define IS_VECTOR(x)    is_vector(x)
#define IS_FUNCTION(x)  is_function(x)
#define IS_BUILTIN(x)   is_builtin(x)
#define IS_THREAD(x)    is_thread(x)
#define IS_VM_CLOSURE(x) is_vm_closure(x)
/* IS_INTEGER, IS_CHAR, IS_NIL, IS_T are usually defined in tagged.h or here */
/* If tagged.h defines IS_FIXNUM, we can alias IS_INTEGER */
#ifndef IS_INTEGER
#define IS_INTEGER(x)   IS_FIXNUM(x)
#endif

#endif /* TYPES_H */
