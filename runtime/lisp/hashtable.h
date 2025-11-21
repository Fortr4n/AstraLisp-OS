/* AstraLisp OS Hash Table Implementation */

#ifndef HASHTABLE_H
#define HASHTABLE_H

#include "tagged.h"
#include <stdint.h>
#include <stdbool.h>

/* Hash Table Entry */
struct hash_entry {
    lisp_value key;
    lisp_value value;
    uint32_t hash;
    struct hash_entry* next;
};

/* Hash Table Structure */
struct hash_table {
    struct hash_entry** buckets;
    uint32_t bucket_count;
    uint32_t entry_count;
    float load_factor;
};

/* Create a new hash table */
struct hash_table* ht_create(uint32_t initial_capacity);

/* Destroy hash table */
void ht_destroy(struct hash_table* table);

/* Put value (insert or update) */
int ht_put(struct hash_table* table, lisp_value key, lisp_value value);

/* Get value */
lisp_value ht_get(struct hash_table* table, lisp_value key);

/* Check if key exists */
bool ht_contains(struct hash_table* table, lisp_value key);

/* Remove key */
void ht_remove(struct hash_table* table, lisp_value key);

/* Resize table */
void ht_resize(struct hash_table* table, uint32_t new_capacity);

#endif /* HASHTABLE_H */
