/* AstraLisp OS Lisp Evaluator Implementation */

#include "evaluator.h"
#include "../../kernel/mm/heap.h"
#include "../../runtime/gc/gc.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

static struct lisp_environment* global_env = NULL;

/*
 * ============================================================================
 * HASH TABLE IMPLEMENTATION FOR ENVIRONMENT BINDINGS
 * ============================================================================
 */

#define ENV_HASH_INITIAL_CAPACITY 16
#define ENV_HASH_LOAD_FACTOR 0.75f
#define ENV_HASH_MAX_CHAIN_LENGTH 8

/* FNV-1a hash function for strings (32-bit) */
static uint32_t hash_string_fnv1a(const char* str) {
    if (!str) {
        return 0;
    }

    uint32_t hash = 2166136261u;  /* FNV offset basis */
    const uint8_t* bytes = (const uint8_t*)str;

    while (*bytes) {
        hash ^= *bytes++;
        hash *= 16777619u;  /* FNV prime */
    }

    return hash;
}

/* Hash symbol for hash table */
static uint32_t hash_symbol(struct lisp_object* symbol) {
    if (!symbol || symbol->type != LISP_SYMBOL || !symbol->value.symbol.name) {
        return 0;
    }

    return hash_string_fnv1a(symbol->value.symbol.name);
}

/* Create hash table */
struct env_hash_table* env_hash_create(uint32_t initial_capacity) {
    if (initial_capacity < 4) {
        initial_capacity = ENV_HASH_INITIAL_CAPACITY;
    }

    /* Round up to next power of 2 for efficient modulo */
    uint32_t capacity = 4;
    while (capacity < initial_capacity) {
        capacity *= 2;
    }

    struct env_hash_table* table = (struct env_hash_table*)kmalloc(sizeof(struct env_hash_table));
    if (!table) {
        return NULL;
    }

    table->buckets = (struct env_hash_entry**)kmalloc(capacity * sizeof(struct env_hash_entry*));
    if (!table->buckets) {
        kfree(table);
        return NULL;
    }

    /* Initialize all buckets to NULL */
    for (uint32_t i = 0; i < capacity; i++) {
        table->buckets[i] = NULL;
    }

    table->bucket_count = capacity;
    table->entry_count = 0;
    table->load_factor = ENV_HASH_LOAD_FACTOR;

    return table;
}

/* Destroy hash table */
void env_hash_destroy(struct env_hash_table* table) {
    if (!table) {
        return;
    }

    /* Free all entries */
    for (uint32_t i = 0; i < table->bucket_count; i++) {
        struct env_hash_entry* entry = table->buckets[i];
        while (entry) {
            struct env_hash_entry* next = entry->next;

            /* Decrement reference counts */
            if (entry->key) {
                lisp_decref(entry->key);
            }
            if (entry->value) {
                lisp_decref(entry->value);
            }

            kfree(entry);
            entry = next;
        }
    }

    kfree(table->buckets);
    kfree(table);
}

/* Get value from hash table */
struct lisp_object* env_hash_get(struct env_hash_table* table, struct lisp_object* key) {
    if (!table || !key || key->type != LISP_SYMBOL) {
        return NULL;
    }

    uint32_t hash = hash_symbol(key);
    uint32_t index = hash & (table->bucket_count - 1);  /* Efficient modulo for power of 2 */

    struct env_hash_entry* entry = table->buckets[index];
    while (entry) {
        /* Check hash first (fast path) */
        if (entry->hash == hash) {
            /* Verify key equality */
            if (entry->key && entry->key->type == LISP_SYMBOL &&
                key->value.symbol.name && entry->key->value.symbol.name &&
                strcmp(key->value.symbol.name, entry->key->value.symbol.name) == 0) {
                return entry->value;
            }
        }
        entry = entry->next;
    }

    return NULL;
}

/* Resize hash table */
void env_hash_resize(struct env_hash_table* table, uint32_t new_capacity) {
    if (!table || new_capacity < 4) {
        return;
    }

    /* Round up to next power of 2 */
    uint32_t capacity = 4;
    while (capacity < new_capacity) {
        capacity *= 2;
    }

    /* Allocate new bucket array */
    struct env_hash_entry** new_buckets = (struct env_hash_entry**)kmalloc(
        capacity * sizeof(struct env_hash_entry*));
    if (!new_buckets) {
        return;  /* Resize failed - continue with old table */
    }

    /* Initialize new buckets */
    for (uint32_t i = 0; i < capacity; i++) {
        new_buckets[i] = NULL;
    }

    /* Rehash all entries */
    for (uint32_t i = 0; i < table->bucket_count; i++) {
        struct env_hash_entry* entry = table->buckets[i];
        while (entry) {
            struct env_hash_entry* next = entry->next;

            /* Rehash to new bucket */
            uint32_t new_index = entry->hash & (capacity - 1);
            entry->next = new_buckets[new_index];
            new_buckets[new_index] = entry;

            entry = next;
        }
    }

    /* Replace old buckets with new */
    kfree(table->buckets);
    table->buckets = new_buckets;
    table->bucket_count = capacity;
}

/* Put value into hash table (insert new entry) */
int env_hash_put(struct env_hash_table* table, struct lisp_object* key, struct lisp_object* value) {
    if (!table || !key || key->type != LISP_SYMBOL || !value) {
        return -1;
    }

    /* Check if resize needed */
    float current_load = (float)table->entry_count / (float)table->bucket_count;
    if (current_load > table->load_factor) {
        env_hash_resize(table, table->bucket_count * 2);
    }

    uint32_t hash = hash_symbol(key);
    uint32_t index = hash & (table->bucket_count - 1);

    /* Check for duplicate key */
    struct env_hash_entry* existing = table->buckets[index];
    while (existing) {
        if (existing->hash == hash) {
            if (existing->key && existing->key->type == LISP_SYMBOL &&
                key->value.symbol.name && existing->key->value.symbol.name &&
                strcmp(key->value.symbol.name, existing->key->value.symbol.name) == 0) {
                /* Key already exists - update value */
                if (existing->value) {
                    lisp_decref(existing->value);
                }
                existing->value = value;
                lisp_incref(value);
                return 0;
            }
        }
        existing = existing->next;
    }

    /* Create new entry */
    struct env_hash_entry* entry = (struct env_hash_entry*)kmalloc(sizeof(struct env_hash_entry));
    if (!entry) {
        return -1;
    }

    entry->key = key;
    entry->value = value;
    entry->hash = hash;
    entry->next = table->buckets[index];

    /* Increment reference counts */
    lisp_incref(key);
    lisp_incref(value);

    /* Insert at head of chain */
    table->buckets[index] = entry;
    table->entry_count++;

    return 0;
}

/* Update existing value in hash table */
int env_hash_update(struct env_hash_table* table, struct lisp_object* key, struct lisp_object* value) {
    if (!table || !key || key->type != LISP_SYMBOL || !value) {
        return -1;
    }

    uint32_t hash = hash_symbol(key);
    uint32_t index = hash & (table->bucket_count - 1);

    struct env_hash_entry* entry = table->buckets[index];
    while (entry) {
        if (entry->hash == hash) {
            if (entry->key && entry->key->type == LISP_SYMBOL &&
                key->value.symbol.name && entry->key->value.symbol.name &&
                strcmp(key->value.symbol.name, entry->key->value.symbol.name) == 0) {
                /* Found - update value */
                if (entry->value) {
                    lisp_decref(entry->value);
                }
                entry->value = value;
                lisp_incref(value);
                return 0;
            }
        }
        entry = entry->next;
    }

    return -1;  /* Key not found */
}

/* Built-in function table */
struct builtin_entry {
    char* name;
    struct lisp_object* (*func)(struct lisp_object*, struct lisp_environment*);
    struct builtin_entry* next;
};

static struct builtin_entry* builtin_table = NULL;

/* Create environment */
struct lisp_environment* env_create(struct lisp_environment* parent) {
    struct lisp_environment* env = (struct lisp_environment*)kmalloc(sizeof(struct lisp_environment));
    if (!env) {
        return NULL;
    }

    env->bindings = env_hash_create(ENV_HASH_INITIAL_CAPACITY);
    if (!env->bindings) {
        kfree(env);
        return NULL;
    }

    env->parent = parent;

    return env;
}

/* Destroy environment */
void env_destroy(struct lisp_environment* env) {
    if (!env) {
        return;
    }

    /* Destroy hash table (this decrements all referenced objects) */
    if (env->bindings) {
        env_hash_destroy(env->bindings);
    }

    /* Note: We don't destroy parent - parent is managed separately */
    kfree(env);
}

/* Lookup variable */
struct lisp_object* env_lookup(struct lisp_environment* env, struct lisp_object* symbol) {
    if (!env || !symbol || symbol->type != LISP_SYMBOL) {
        return NULL;
    }

    /* Search current environment and all parents */
    struct lisp_environment* current = env;
    while (current) {
        if (current->bindings) {
            struct lisp_object* value = env_hash_get(current->bindings, symbol);
            if (value) {
                return value;
            }
        }
        current = current->parent;
    }

    return NULL;
}

/* Define variable */
int env_define(struct lisp_environment* env, struct lisp_object* symbol, struct lisp_object* value) {
    if (!env || !symbol || symbol->type != LISP_SYMBOL || !value) {
        return -1;
    }

    if (!env->bindings) {
        env->bindings = env_hash_create(ENV_HASH_INITIAL_CAPACITY);
        if (!env->bindings) {
            return -1;
        }
    }

    return env_hash_put(env->bindings, symbol, value);
}

/* Set variable */
int env_set(struct lisp_environment* env, struct lisp_object* symbol, struct lisp_object* value) {
    if (!env || !symbol || symbol->type != LISP_SYMBOL || !value) {
        return -1;
    }

    /* Search current environment and all parents for existing binding */
    struct lisp_environment* current = env;
    while (current) {
        if (current->bindings) {
            /* Check if symbol exists in this environment */
            struct lisp_object* existing = env_hash_get(current->bindings, symbol);
            if (existing) {
                /* Update existing binding */
                return env_hash_update(current->bindings, symbol, value);
            }
        }
        current = current->parent;
    }

    return -1;  /* Symbol not found in any environment */
}

/* Evaluate expression */
struct lisp_object* lisp_eval(struct lisp_object* expr, struct lisp_environment* env) {
    if (!expr || !env) {
        return NULL;
    }
    
    /* Self-evaluating forms */
    if (expr->type == LISP_INTEGER || expr->type == LISP_FLOAT || 
        expr->type == LISP_STRING || expr->type == LISP_NIL) {
        lisp_incref(expr);
        return expr;
    }
    
    /* Symbol lookup */
    if (expr->type == LISP_SYMBOL) {
        struct lisp_object* value = env_lookup(env, expr);
        if (value) {
            lisp_incref(value);
            return value;
        }
        /* Check built-ins */
        struct builtin_entry* entry = builtin_table;
        while (entry) {
            if (strcmp(expr->value.symbol.name, entry->name) == 0) {
                struct lisp_object* func_obj = lisp_create_object(LISP_FUNCTION);
                if (func_obj) {
                    func_obj->value.function.is_macro = false;
                    /* Store built-in pointer */
                }
                return func_obj;
            }
            entry = entry->next;
        }
        return NULL;
    }
    
    /* Function call */
    if (expr->type == LISP_CONS) {
        struct lisp_object* car = expr->value.cons.car;
        struct lisp_object* cdr = expr->value.cons.cdr;
        
        if (!car) {
            return lisp_nil();
        }
        
        /* Special forms */
        if (car->type == LISP_SYMBOL) {
            if (strcmp(car->value.symbol.name, "quote") == 0) {
                if (cdr && cdr->type == LISP_CONS) {
                    struct lisp_object* quoted = cdr->value.cons.car;
                    lisp_incref(quoted);
                    return quoted;
                }
            } else if (strcmp(car->value.symbol.name, "if") == 0) {
                if (cdr && cdr->type == LISP_CONS) {
                    struct lisp_object* condition = lisp_eval(cdr->value.cons.car, env);
                    struct lisp_object* then_expr = NULL;
                    struct lisp_object* else_expr = NULL;
                    
                    if (cdr->value.cons.cdr && cdr->value.cons.cdr->type == LISP_CONS) {
                        then_expr = cdr->value.cons.cdr->value.cons.car;
                        if (cdr->value.cons.cdr->value.cons.cdr && 
                            cdr->value.cons.cdr->value.cons.cdr->type == LISP_CONS) {
                            else_expr = cdr->value.cons.cdr->value.cons.cdr->value.cons.car;
                        }
                    }
                    
                    bool condition_true = false;
                    if (condition) {
                        if (condition->type == LISP_NIL) {
                            condition_true = false;
                        } else {
                            condition_true = true;
                        }
                        lisp_decref(condition);
                    }
                    
                    if (condition_true && then_expr) {
                        return lisp_eval(then_expr, env);
                    } else if (!condition_true && else_expr) {
                        return lisp_eval(else_expr, env);
                    }
                    return lisp_nil();
                }
            } else if (strcmp(car->value.symbol.name, "lambda") == 0) {
                if (cdr && cdr->type == LISP_CONS) {
                    struct lisp_object* params = cdr->value.cons.car;
                    struct lisp_object* body = NULL;
                    if (cdr->value.cons.cdr && cdr->value.cons.cdr->type == LISP_CONS) {
                        body = cdr->value.cons.cdr->value.cons.car;
                    }
                    
                    struct lisp_object* func = lisp_create_object(LISP_FUNCTION);
                    if (func) {
                        func->value.function.params = params;
                        lisp_incref(params);
                        func->value.function.body = body;
                        if (body) {
                            lisp_incref(body);
                        }
                        func->value.function.env = env;
                        lisp_incref(env);
                        func->value.function.is_macro = false;
                    }
                    return func;
                }
            } else if (strcmp(car->value.symbol.name, "defun") == 0) {
                if (cdr && cdr->type == LISP_CONS) {
                    struct lisp_object* name = cdr->value.cons.car;
                    struct lisp_object* params = NULL;
                    struct lisp_object* body = NULL;
                    
                    if (cdr->value.cons.cdr && cdr->value.cons.cdr->type == LISP_CONS) {
                        params = cdr->value.cons.cdr->value.cons.car;
                        if (cdr->value.cons.cdr->value.cons.cdr && 
                            cdr->value.cons.cdr->value.cons.cdr->type == LISP_CONS) {
                            body = cdr->value.cons.cdr->value.cons.cdr->value.cons.car;
                        }
                    }
                    
                    if (name && name->type == LISP_SYMBOL && body) {
                        struct lisp_object* func = lisp_create_object(LISP_FUNCTION);
                        if (func) {
                            func->value.function.params = params;
                            if (params) {
                                lisp_incref(params);
                            }
                            func->value.function.body = body;
                            lisp_incref(body);
                            func->value.function.env = env;
                            lisp_incref(env);
                            func->value.function.is_macro = false;
                            
                            env_define(env, name, func);
                            return func;
                        }
                    }
                }
            } else if (strcmp(car->value.symbol.name, "setq") == 0) {
                if (cdr && cdr->type == LISP_CONS) {
                    struct lisp_object* symbol = cdr->value.cons.car;
                    struct lisp_object* value_expr = NULL;
                    if (cdr->value.cons.cdr && cdr->value.cons.cdr->type == LISP_CONS) {
                        value_expr = cdr->value.cons.cdr->value.cons.car;
                    }
                    
                    if (symbol && symbol->type == LISP_SYMBOL && value_expr) {
                        struct lisp_object* value = lisp_eval(value_expr, env);
                        if (value) {
                            env_set(env, symbol, value);
                            return value;
                        }
                    }
                }
            } else if (strcmp(car->value.symbol.name, "let") == 0) {
                if (cdr && cdr->type == LISP_CONS) {
                    struct lisp_object* bindings = cdr->value.cons.car;
                    struct lisp_object* body = NULL;
                    if (cdr->value.cons.cdr && cdr->value.cons.cdr->type == LISP_CONS) {
                        body = cdr->value.cons.cdr->value.cons.car;
                    }
                    
                    struct lisp_environment* let_env = env_create(env);
                    if (let_env && bindings) {
                        struct lisp_object* binding = bindings;
                        while (binding && binding->type == LISP_CONS) {
                            struct lisp_object* binding_pair = binding->value.cons.car;
                            if (binding_pair && binding_pair->type == LISP_CONS) {
                                struct lisp_object* var = binding_pair->value.cons.car;
                                struct lisp_object* val_expr = NULL;
                                if (binding_pair->value.cons.cdr && 
                                    binding_pair->value.cons.cdr->type == LISP_CONS) {
                                    val_expr = binding_pair->value.cons.cdr->value.cons.car;
                                }
                                
                                if (var && var->type == LISP_SYMBOL && val_expr) {
                                    struct lisp_object* val = lisp_eval(val_expr, env);
                                    if (val) {
                                        env_define(let_env, var, val);
                                    }
                                }
                            }
                            binding = binding->value.cons.cdr;
                        }
                        
                        if (body) {
                            return lisp_eval(body, let_env);
                        }
                    }
                }
            }
        }
        
        /* Function call */
        struct lisp_object* func = lisp_eval(car, env);
        if (!func) {
            return NULL;
        }
        
        /* Evaluate arguments */
        struct lisp_object* args = lisp_nil();
        struct lisp_object* arg_list = cdr;
        while (arg_list && arg_list->type == LISP_CONS) {
            struct lisp_object* arg = lisp_eval(arg_list->value.cons.car, env);
            if (arg) {
                args = lisp_create_cons(arg, args);
            }
            arg_list = arg_list->value.cons.cdr;
        }
        
        struct lisp_object* result = lisp_apply(func, args, env);
        lisp_decref(func);
        lisp_decref(args);
        
        return result;
    }
    
    return NULL;
}

/* Apply function */
struct lisp_object* lisp_apply(struct lisp_object* func, struct lisp_object* args, struct lisp_environment* env) {
    if (!func || !args) {
        return NULL;
    }
    
    if (func->type == LISP_FUNCTION) {
        /* User-defined function */
        struct lisp_environment* func_env = env_create(func->value.function.env);
        if (!func_env) {
            return NULL;
        }
        
        /* Bind parameters */
        struct lisp_object* params = func->value.function.params;
        struct lisp_object* arg = args;
        
        while (params && params->type == LISP_CONS && arg && arg->type == LISP_CONS) {
            struct lisp_object* param = params->value.cons.car;
            struct lisp_object* arg_val = arg->value.cons.car;
            
            if (param && param->type == LISP_SYMBOL) {
                env_define(func_env, param, arg_val);
            }
            
            params = params->value.cons.cdr;
            arg = arg->value.cons.cdr;
        }
        
        /* Evaluate body */
        struct lisp_object* result = lisp_eval(func->value.function.body, func_env);
        
        return result;
    }
    
    return NULL;
}

/* Built-in functions */
struct lisp_object* lisp_builtin_car(struct lisp_object* args, struct lisp_environment* env) {
    if (args && args->type == LISP_CONS) {
        struct lisp_object* list = args->value.cons.car;
        if (list && list->type == LISP_CONS) {
            struct lisp_object* car = list->value.cons.car;
            lisp_incref(car);
            return car;
        }
    }
    return lisp_nil();
}

struct lisp_object* lisp_builtin_cdr(struct lisp_object* args, struct lisp_environment* env) {
    if (args && args->type == LISP_CONS) {
        struct lisp_object* list = args->value.cons.car;
        if (list && list->type == LISP_CONS) {
            struct lisp_object* cdr = list->value.cons.cdr;
            lisp_incref(cdr);
            return cdr;
        }
    }
    return lisp_nil();
}

struct lisp_object* lisp_builtin_cons(struct lisp_object* args, struct lisp_environment* env) {
    if (args && args->type == LISP_CONS) {
        struct lisp_object* car = args->value.cons.car;
        struct lisp_object* cdr = NULL;
        if (args->value.cons.cdr && args->value.cons.cdr->type == LISP_CONS) {
            cdr = args->value.cons.cdr->value.cons.car;
        }
        return lisp_create_cons(car, cdr);
    }
    return lisp_nil();
}

struct lisp_object* lisp_builtin_eq(struct lisp_object* args, struct lisp_environment* env) {
    if (args && args->type == LISP_CONS) {
        struct lisp_object* a = args->value.cons.car;
        struct lisp_object* b = NULL;
        if (args->value.cons.cdr && args->value.cons.cdr->type == LISP_CONS) {
            b = args->value.cons.cdr->value.cons.car;
        }
        if (a && b && lisp_equal(a, b)) {
            return lisp_create_integer(1);
        }
    }
    return lisp_nil();
}

struct lisp_object* lisp_builtin_add(struct lisp_object* args, struct lisp_environment* env) {
    int64_t sum = 0;
    struct lisp_object* arg = args;
    while (arg && arg->type == LISP_CONS) {
        struct lisp_object* val = arg->value.cons.car;
        if (val && val->type == LISP_INTEGER) {
            sum += val->value.integer;
        }
        arg = arg->value.cons.cdr;
    }
    return lisp_create_integer(sum);
}

struct lisp_object* lisp_builtin_sub(struct lisp_object* args, struct lisp_environment* env) {
    if (!args || args->type != LISP_CONS) {
        return lisp_nil();
    }
    
    struct lisp_object* first = args->value.cons.car;
    if (!first || first->type != LISP_INTEGER) {
        return lisp_nil();
    }
    
    int64_t result = first->value.integer;
    struct lisp_object* arg = args->value.cons.cdr;
    
    if (!arg || arg->type != LISP_CONS) {
        return lisp_create_integer(-result);
    }
    
    while (arg && arg->type == LISP_CONS) {
        struct lisp_object* val = arg->value.cons.car;
        if (val && val->type == LISP_INTEGER) {
            result -= val->value.integer;
        }
        arg = arg->value.cons.cdr;
    }
    
    return lisp_create_integer(result);
}

struct lisp_object* lisp_builtin_mul(struct lisp_object* args, struct lisp_environment* env) {
    int64_t product = 1;
    struct lisp_object* arg = args;
    while (arg && arg->type == LISP_CONS) {
        struct lisp_object* val = arg->value.cons.car;
        if (val && val->type == LISP_INTEGER) {
            product *= val->value.integer;
        }
        arg = arg->value.cons.cdr;
    }
    return lisp_create_integer(product);
}

struct lisp_object* lisp_builtin_div(struct lisp_object* args, struct lisp_environment* env) {
    if (!args || args->type != LISP_CONS) {
        return lisp_nil();
    }
    
    struct lisp_object* first = args->value.cons.car;
    if (!first || first->type != LISP_INTEGER) {
        return lisp_nil();
    }
    
    int64_t result = first->value.integer;
    struct lisp_object* arg = args->value.cons.cdr;
    
    while (arg && arg->type == LISP_CONS) {
        struct lisp_object* val = arg->value.cons.car;
        if (val && val->type == LISP_INTEGER && val->value.integer != 0) {
            result /= val->value.integer;
        } else {
            return lisp_nil();
        }
        arg = arg->value.cons.cdr;
    }
    
    return lisp_create_integer(result);
}

struct lisp_object* lisp_builtin_list(struct lisp_object* args, struct lisp_environment* env) {
    return args;  /* Arguments are already a list */
}

struct lisp_object* lisp_builtin_length(struct lisp_object* args, struct lisp_environment* env) {
    if (!args || args->type != LISP_CONS) {
        return lisp_create_integer(0);
    }
    
    size_t len = 0;
    struct lisp_object* list = args->value.cons.car;
    while (list && list->type == LISP_CONS) {
        len++;
        list = list->value.cons.cdr;
    }
    
    return lisp_create_integer(len);
}

struct lisp_object* lisp_builtin_print(struct lisp_object* args, struct lisp_environment* env) {
    struct lisp_object* arg = args;
    while (arg && arg->type == LISP_CONS) {
        struct lisp_object* obj = arg->value.cons.car;
        lisp_print(obj);
        arg = arg->value.cons.cdr;
    }
    return lisp_nil();
}

/* Register built-in */
void lisp_register_builtin(const char* name, struct lisp_object* (*func)(struct lisp_object*, struct lisp_environment*)) {
    struct builtin_entry* entry = (struct builtin_entry*)kmalloc(sizeof(struct builtin_entry));
    if (entry) {
        entry->name = (char*)kmalloc(strlen(name) + 1);
        if (entry->name) {
            strcpy(entry->name, name);
            entry->func = func;
            entry->next = builtin_table;
            builtin_table = entry;
        } else {
            kfree(entry);
        }
    }
}

/* Initialize evaluator */
int evaluator_init(void) {
    global_env = env_create(NULL);
    if (!global_env) {
        return -1;
    }
    
    /* Register built-ins */
    lisp_register_builtin("car", lisp_builtin_car);
    lisp_register_builtin("cdr", lisp_builtin_cdr);
    lisp_register_builtin("cons", lisp_builtin_cons);
    lisp_register_builtin("eq", lisp_builtin_eq);
    lisp_register_builtin("+", lisp_builtin_add);
    lisp_register_builtin("-", lisp_builtin_sub);
    lisp_register_builtin("*", lisp_builtin_mul);
    lisp_register_builtin("/", lisp_builtin_div);
    lisp_register_builtin("list", lisp_builtin_list);
    lisp_register_builtin("length", lisp_builtin_length);
    lisp_register_builtin("print", lisp_builtin_print);
    
    return 0;
}
