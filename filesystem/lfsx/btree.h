/* AstraLisp OS B+ Tree Implementation for LFSX */

#ifndef BTREE_H
#define BTREE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* B+ tree node structure */
struct btree_node {
    uint32_t node_id;
    bool is_leaf;
    uint32_t key_count;
    uint32_t parent_id;
    uint32_t* keys;
    union {
        uint32_t* child_ids;  /* For internal nodes */
        uint64_t* values;      /* For leaf nodes */
    } data;
    uint32_t next_leaf_id;     /* For leaf nodes */
    uint32_t prev_leaf_id;     /* For leaf nodes */
};

/* B+ tree structure */
struct btree {
    uint32_t root_id;
    uint32_t node_size;
    uint32_t max_keys;
    uint32_t min_keys;
    uint32_t next_node_id;
    struct btree_node* (*alloc_node)(uint32_t node_id);
    void (*free_node)(struct btree_node* node);
    void (*read_node)(uint32_t node_id, struct btree_node* node);
    void (*write_node)(uint32_t node_id, struct btree_node* node);
    int (*compare_keys)(uint32_t key1, uint32_t key2);
};

/* Initialize B+ tree */
int btree_init(struct btree* tree, uint32_t node_size,
               struct btree_node* (*alloc_node)(uint32_t),
               void (*free_node)(struct btree_node*),
               void (*read_node)(uint32_t, struct btree_node*),
               void (*write_node)(uint32_t, struct btree_node*),
               int (*compare_keys)(uint32_t, uint32_t));

/* Insert key-value pair */
int btree_insert(struct btree* tree, uint32_t key, uint64_t value);

/* Delete key */
int btree_delete(struct btree* tree, uint32_t key);

/* Search for key */
int btree_search(struct btree* tree, uint32_t key, uint64_t* value);

/* Get minimum key */
int btree_min(struct btree* tree, uint32_t* key, uint64_t* value);

/* Get maximum key */
int btree_max(struct btree* tree, uint32_t* key, uint64_t* value);

/* Range query */
int btree_range_query(struct btree* tree, uint32_t start_key, uint32_t end_key,
                      uint32_t* keys, uint64_t* values, size_t max_results, size_t* result_count);

#endif /* BTREE_H */
