/* AstraLisp OS Lisp Evaluator */

#ifndef EVALUATOR_H
#define EVALUATOR_H

#include "reader.h"
#include <stdbool.h>

/* Hash table entry for symbol bindings */
struct env_hash_entry {
    struct lisp_object* key;      /* Symbol */
    struct lisp_object* value;    /* Bound value */
    uint32_t hash;                /* Cached hash value */
    struct env_hash_entry* next;  /* Collision chain */
};

/* Hash table for environment bindings */
struct env_hash_table {
    struct env_hash_entry** buckets;  /* Array of bucket chains */
    uint32_t bucket_count;            /* Number of buckets */
    uint32_t entry_count;             /* Number of entries */
    float load_factor;                /* Load factor threshold for resize */
};

/* Environment (binding frame) */
struct lisp_environment {
    struct env_hash_table* bindings;  /* Hash table of bindings */
    struct lisp_environment* parent;  /* Parent environment */
};

/* Hash table operations */
struct env_hash_table* env_hash_create(uint32_t initial_capacity);
void env_hash_destroy(struct env_hash_table* table);
struct lisp_object* env_hash_get(struct env_hash_table* table, struct lisp_object* key);
int env_hash_put(struct env_hash_table* table, struct lisp_object* key, struct lisp_object* value);
int env_hash_update(struct env_hash_table* table, struct lisp_object* key, struct lisp_object* value);
void env_hash_resize(struct env_hash_table* table, uint32_t new_capacity);

/* Create environment */
struct lisp_environment* env_create(struct lisp_environment* parent);

/* Destroy environment */
void env_destroy(struct lisp_environment* env);

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
