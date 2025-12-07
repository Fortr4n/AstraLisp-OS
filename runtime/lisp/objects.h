/* AstraLisp OS Heap Object Structures
 * 
 * Specialized structures for heap-allocated objects.
 * All heap objects must start with a `lisp_header`.
 */

#ifndef OBJECTS_H
#define OBJECTS_H

#include "tagged.h"
#include <stdint.h>

/* Forward declaration for hash table */
struct hash_table;

/* Heap Object Types (stored in header) */
typedef enum {
    TYPE_CONS       = 1,
    TYPE_SYMBOL     = 2,
    TYPE_STRING     = 3,
    TYPE_VECTOR     = 4,
    TYPE_FUNCTION   = 5,
    TYPE_BIGNUM     = 6,
    TYPE_FLOAT      = 7,
    TYPE_ENV        = 8,
    TYPE_BUILTIN    = 9,
    TYPE_THREAD     = 10,
    TYPE_VM_CLOSURE = 11   /* Bytecode VM closure */
} heap_type_t;

/* Common Object Header */
/* 64-bit header:
 * [63:32] Size (in bytes)
 * [31:8]  GC Flags / Reserved
 * [7:0]   Type Tag
 */
struct lisp_header {
    uint64_t metadata;
};

/* Header Macros */
/* These macros operate on the raw metadata value (uint64_t) */
#define HEADER_TYPE(val)      ((val) & 0xFF)
#define HEADER_SIZE(val)      ((val) >> 32)
#define MAKE_HEADER(t, s)     (((uint64_t)(s) << 32) | (t))

/* Helper to get type from an object pointer */
#define GET_TYPE(ptr)         (HEADER_TYPE(((struct lisp_header*)(ptr))->metadata))

/* Helper to set type in a header pointer */
/* Note: This assumes we are constructing the header value */
#define SET_TYPE(h, t)        ((h)->metadata = ((h)->metadata & ~0xFFULL) | (t))

/* Cons Cell */
struct lisp_cons {
    struct lisp_header header;
    lisp_value car;
    lisp_value cdr;
};

/* Symbol */
struct lisp_symbol {
    struct lisp_header header;
    lisp_value name;      /* String */
    lisp_value value;     /* Global value */
    lisp_value function;  /* Function binding */
    lisp_value plist;     /* Property list */
    lisp_value package;   /* Package */
    uint32_t hash;
};

/* String */
struct lisp_string {
    struct lisp_header header;
    uint64_t length;
    char data[];          /* Flexible array member */
};

/* Vector */
struct lisp_vector {
    struct lisp_header header;
    uint64_t length;
    lisp_value data[];    /* Flexible array member */
};

/* Function (Closure) */
struct lisp_function {
    struct lisp_header header;
    lisp_value code;      /* Machine code or Bytecode */
    lisp_value env;       /* Closed-over environment */
    lisp_value args;      /* Argument list (lambda list) */
    lisp_value name;      /* Function name (optional) */
};

/* Environment */
struct lisp_env {
    struct lisp_header header;
    lisp_value parent;          /* Parent environment */
    struct hash_table* table;   /* Hash table for bindings */
};

/* Built-in Function */
typedef lisp_value (*lisp_builtin_fn)(lisp_value env, lisp_value args);

struct lisp_builtin {
    struct lisp_header header;
    lisp_builtin_fn fn;
    lisp_value name;      /* Symbol or String name */
};

/* Thread handle (wraps internal lisp_thread pointer) */
struct lisp_thread_handle {
    struct lisp_header header;
    void* thread_ptr;     /* Pointer to struct lisp_thread */
    uint64_t thread_id;   /* Thread ID for debugging */
};

/* Accessor Macros */
#define CAR(x)          (((struct lisp_cons*)PTR_VAL(x))->car)
#define CDR(x)          (((struct lisp_cons*)PTR_VAL(x))->cdr)

#define SYMBOL_NAME(x)  (((struct lisp_symbol*)PTR_VAL(x))->name)
#define SYMBOL_VALUE(x) (((struct lisp_symbol*)PTR_VAL(x))->value)

#endif /* OBJECTS_H */
