/* AstraLisp OS Lisp Type System */

#ifndef TYPES_H
#define TYPES_H

#include "reader.h"
#include <stdbool.h>

/* Type predicate */
typedef bool (*type_predicate_t)(struct lisp_object* obj);

/* Type information */
struct lisp_type {
    char* name;
    type_predicate_t predicate;
    struct lisp_type* parent;
    struct lisp_type* next;
};

/* Type system */
struct type_system {
    struct lisp_type* types;
    struct lisp_object* type_registry;
};

/* Initialize type system */
int type_system_init(struct type_system* ts);

/* Register type */
int type_register(struct type_system* ts, const char* name, type_predicate_t predicate, const char* parent);

/* Check type */
bool type_check(struct type_system* ts, struct lisp_object* obj, const char* type_name);

/* Get type name */
const char* type_get_name(struct type_system* ts, struct lisp_object* obj);

/* Type predicates */
bool type_integer_p(struct lisp_object* obj);
bool type_string_p(struct lisp_object* obj);
bool type_symbol_p(struct lisp_object* obj);
bool type_cons_p(struct lisp_object* obj);
bool type_list_p(struct lisp_object* obj);
bool type_function_p(struct lisp_object* obj);
bool type_macro_p(struct lisp_object* obj);

#endif /* TYPES_H */
