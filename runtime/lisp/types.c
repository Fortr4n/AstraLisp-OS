/* AstraLisp OS Lisp Type System Implementation */

#include "types.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

/* Type predicates */
bool type_integer_p(struct lisp_object* obj) {
    return obj && obj->type == LISP_INTEGER;
}

bool type_string_p(struct lisp_object* obj) {
    return obj && obj->type == LISP_STRING;
}

bool type_symbol_p(struct lisp_object* obj) {
    return obj && obj->type == LISP_SYMBOL;
}

bool type_cons_p(struct lisp_object* obj) {
    return obj && obj->type == LISP_CONS;
}

bool type_list_p(struct lisp_object* obj) {
    if (!obj) {
        return false;
    }
    
    if (obj->type == LISP_NIL) {
        return true;
    }
    
    if (obj->type == LISP_CONS) {
        return type_list_p(obj->value.cons.cdr);
    }
    
    return false;
}

bool type_function_p(struct lisp_object* obj) {
    return obj && obj->type == LISP_FUNCTION && !obj->value.function.is_macro;
}

bool type_macro_p(struct lisp_object* obj) {
    return obj && obj->type == LISP_FUNCTION && obj->value.function.is_macro;
}

/* Initialize type system */
int type_system_init(struct type_system* ts) {
    if (!ts) {
        return -1;
    }
    
    ts->types = NULL;
    ts->type_registry = lisp_nil();
    
    return 0;
}

/* Register type */
int type_register(struct type_system* ts, const char* name, type_predicate_t predicate, const char* parent) {
    if (!ts || !name || !predicate) {
        return -1;
    }
    
    struct lisp_type* type = (struct lisp_type*)kmalloc(sizeof(struct lisp_type));
    if (!type) {
        return -1;
    }
    
    type->name = (char*)kmalloc(strlen(name) + 1);
    if (!type->name) {
        kfree(type);
        return -1;
    }
    strcpy(type->name, name);
    
    type->predicate = predicate;
    type->parent = NULL;  /* Would look up parent */
    type->next = ts->types;
    ts->types = type;
    
    return 0;
}

/* Check type */
bool type_check(struct type_system* ts, struct lisp_object* obj, const char* type_name) {
    if (!ts || !obj || !type_name) {
        return false;
    }
    
    struct lisp_type* type = ts->types;
    while (type) {
        if (strcmp(type->name, type_name) == 0) {
            return type->predicate(obj);
        }
        type = type->next;
    }
    
    return false;
}

/* Get type name */
const char* type_get_name(struct type_system* ts, struct lisp_object* obj) {
    if (!ts || !obj) {
        return NULL;
    }
    
    struct lisp_type* type = ts->types;
    while (type) {
        if (type->predicate(obj)) {
            return type->name;
        }
        type = type->next;
    }
    
    switch (obj->type) {
        case LISP_NIL: return "nil";
        case LISP_INTEGER: return "integer";
        case LISP_FLOAT: return "float";
        case LISP_SYMBOL: return "symbol";
        case LISP_STRING: return "string";
        case LISP_CONS: return "cons";
        case LISP_VECTOR: return "vector";
        case LISP_FUNCTION: return "function";
        default: return "unknown";
    }
}
