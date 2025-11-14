/* AstraLisp OS B+ Tree Implementation */

#include "btree.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define BTREE_DEFAULT_ORDER 4

/* Allocate new node */
static struct btree_node* btree_alloc_node_internal(struct btree* tree) {
    struct btree_node* node = tree->alloc_node(tree->next_node_id++);
    if (!node) {
        return NULL;
    }
    
    node->node_id = tree->next_node_id - 1;
    node->is_leaf = true;
    node->key_count = 0;
    node->parent_id = 0;
    node->keys = (uint32_t*)kmalloc(sizeof(uint32_t) * tree->max_keys);
    node->data.values = (uint64_t*)kmalloc(sizeof(uint64_t) * tree->max_keys);
    node->next_leaf_id = 0;
    node->prev_leaf_id = 0;
    
    if (!node->keys || !node->data.values) {
        if (node->keys) kfree(node->keys);
        if (node->data.values) kfree(node->data.values);
        tree->free_node(node);
        return NULL;
    }
    
    return node;
}

/* Split leaf node */
static int btree_split_leaf(struct btree* tree, struct btree_node* node) {
    struct btree_node* new_node = btree_alloc_node_internal(tree);
    if (!new_node) {
        return -1;
    }
    
    new_node->is_leaf = true;
    uint32_t split_point = tree->min_keys;
    
    /* Move half the keys to new node */
    for (uint32_t i = split_point; i < node->key_count; i++) {
        new_node->keys[new_node->key_count] = node->keys[i];
        new_node->data.values[new_node->key_count] = node->data.values[i];
        new_node->key_count++;
    }
    
    node->key_count = split_point;
    
    /* Update leaf chain */
    new_node->next_leaf_id = node->next_leaf_id;
    new_node->prev_leaf_id = node->node_id;
    if (node->next_leaf_id != 0) {
        struct btree_node* next = tree->alloc_node(node->next_leaf_id);
        if (next) {
            next->prev_leaf_id = new_node->node_id;
            tree->write_node(next->node_id, next);
            tree->free_node(next);
        }
    }
    node->next_leaf_id = new_node->node_id;
    
    /* Promote middle key to parent */
    uint32_t promote_key = new_node->keys[0];
    
    if (node->parent_id == 0) {
        /* Create new root */
        struct btree_node* root = btree_alloc_node_internal(tree);
        if (!root) {
            return -1;
        }
        
        root->is_leaf = false;
        root->keys = (uint32_t*)kmalloc(sizeof(uint32_t) * tree->max_keys);
        root->data.child_ids = (uint32_t*)kmalloc(sizeof(uint32_t) * (tree->max_keys + 1));
        
        if (!root->keys || !root->data.child_ids) {
            if (root->keys) kfree(root->keys);
            if (root->data.child_ids) kfree(root->data.child_ids);
            tree->free_node(root);
            return -1;
        }
        
        root->keys[0] = promote_key;
        root->data.child_ids[0] = node->node_id;
        root->data.child_ids[1] = new_node->node_id;
        root->key_count = 1;
        
        node->parent_id = root->node_id;
        new_node->parent_id = root->node_id;
        
        tree->root_id = root->node_id;
        tree->write_node(root->node_id, root);
        tree->free_node(root);
    } else {
        /* Insert into parent */
        struct btree_node* parent = tree->alloc_node(node->parent_id);
        if (!parent) {
            return -1;
        }
        
        /* Find insertion point */
        uint32_t insert_pos = 0;
        while (insert_pos < parent->key_count && 
               tree->compare_keys(parent->keys[insert_pos], promote_key) < 0) {
            insert_pos++;
        }
        
        /* Shift keys and children */
        for (uint32_t i = parent->key_count; i > insert_pos; i--) {
            parent->keys[i] = parent->keys[i - 1];
            parent->data.child_ids[i + 1] = parent->data.child_ids[i];
        }
        
        parent->keys[insert_pos] = promote_key;
        parent->data.child_ids[insert_pos + 1] = new_node->node_id;
        parent->key_count++;
        
        new_node->parent_id = parent->node_id;
        
        tree->write_node(parent->node_id, parent);
        
        /* Check if parent needs splitting */
        if (parent->key_count > tree->max_keys) {
            /* Split parent (recursive) */
            /* This would require implementing split_internal */
        }
        
        tree->free_node(parent);
    }
    
    tree->write_node(node->node_id, node);
    tree->write_node(new_node->node_id, new_node);
    tree->free_node(new_node);
    
    return 0;
}

/* Insert key-value pair */
int btree_insert(struct btree* tree, uint32_t key, uint64_t value) {
    if (!tree) {
        return -1;
    }
    
    if (tree->root_id == 0) {
        /* Create root */
        struct btree_node* root = btree_alloc_node_internal(tree);
        if (!root) {
            return -1;
        }
        
        root->keys[0] = key;
        root->data.values[0] = value;
        root->key_count = 1;
        
        tree->root_id = root->node_id;
        tree->write_node(root->node_id, root);
        tree->free_node(root);
        
        return 0;
    }
    
    /* Find leaf node */
    struct btree_node* node = tree->alloc_node(tree->root_id);
    if (!node) {
        return -1;
    }
    
    /* Traverse to leaf */
    while (!node->is_leaf) {
        uint32_t child_idx = 0;
        for (uint32_t i = 0; i < node->key_count; i++) {
            if (tree->compare_keys(key, node->keys[i]) < 0) {
                break;
            }
            child_idx = i + 1;
        }
        
        uint32_t next_id = node->data.child_ids[child_idx];
        tree->free_node(node);
        node = tree->alloc_node(next_id);
        if (!node) {
            return -1;
        }
    }
    
    /* Insert into leaf */
    uint32_t insert_pos = 0;
    while (insert_pos < node->key_count && 
           tree->compare_keys(node->keys[insert_pos], key) < 0) {
        insert_pos++;
    }
    
    /* Check if key already exists */
    if (insert_pos < node->key_count && 
        tree->compare_keys(node->keys[insert_pos], key) == 0) {
        /* Update value */
        node->data.values[insert_pos] = value;
        tree->write_node(node->node_id, node);
        tree->free_node(node);
        return 0;
    }
    
    /* Shift keys and values */
    for (uint32_t i = node->key_count; i > insert_pos; i--) {
        node->keys[i] = node->keys[i - 1];
        node->data.values[i] = node->data.values[i - 1];
    }
    
    node->keys[insert_pos] = key;
    node->data.values[insert_pos] = value;
    node->key_count++;
    
    /* Check if node needs splitting */
    if (node->key_count > tree->max_keys) {
        btree_split_leaf(tree, node);
    } else {
        tree->write_node(node->node_id, node);
    }
    
    tree->free_node(node);
    
    return 0;
}

/* Search for key */
int btree_search(struct btree* tree, uint32_t key, uint64_t* value) {
    if (!tree || !value) {
        return -1;
    }
    
    if (tree->root_id == 0) {
        return -1;
    }
    
    struct btree_node* node = tree->alloc_node(tree->root_id);
    if (!node) {
        return -1;
    }
    
    /* Traverse to leaf */
    while (!node->is_leaf) {
        uint32_t child_idx = 0;
        for (uint32_t i = 0; i < node->key_count; i++) {
            int cmp = tree->compare_keys(key, node->keys[i]);
            if (cmp < 0) {
                break;
            }
            if (cmp == 0) {
                child_idx = i + 1;
                break;
            }
            child_idx = i + 1;
        }
        
        uint32_t next_id = node->data.child_ids[child_idx];
        tree->free_node(node);
        node = tree->alloc_node(next_id);
        if (!node) {
            return -1;
        }
    }
    
    /* Search in leaf */
    for (uint32_t i = 0; i < node->key_count; i++) {
        if (tree->compare_keys(node->keys[i], key) == 0) {
            *value = node->data.values[i];
            tree->free_node(node);
            return 0;
        }
        if (tree->compare_keys(node->keys[i], key) > 0) {
            break;
        }
    }
    
    tree->free_node(node);
    return -1;
}

/* Delete key */
int btree_delete(struct btree* tree, uint32_t key) {
    if (!tree) {
        return -1;
    }
    
    if (tree->root_id == 0) {
        return -1;
    }
    
    /* Find leaf node */
    struct btree_node* node = tree->alloc_node(tree->root_id);
    if (!node) {
        return -1;
    }
    
    /* Traverse to leaf */
    while (!node->is_leaf) {
        uint32_t child_idx = 0;
        for (uint32_t i = 0; i < node->key_count; i++) {
            if (tree->compare_keys(key, node->keys[i]) < 0) {
                break;
            }
            child_idx = i + 1;
        }
        
        uint32_t next_id = node->data.child_ids[child_idx];
        tree->free_node(node);
        node = tree->alloc_node(next_id);
        if (!node) {
            return -1;
        }
    }
    
    /* Find key in leaf */
    uint32_t key_pos = node->key_count;
    for (uint32_t i = 0; i < node->key_count; i++) {
        if (tree->compare_keys(node->keys[i], key) == 0) {
            key_pos = i;
            break;
        }
    }
    
    if (key_pos == node->key_count) {
        /* Key not found */
        tree->free_node(node);
        return -1;
    }
    
    /* Remove key */
    for (uint32_t i = key_pos; i < node->key_count - 1; i++) {
        node->keys[i] = node->keys[i + 1];
        node->data.values[i] = node->data.values[i + 1];
    }
    node->key_count--;
    
    /* Check if node needs merging */
    if (node->key_count < tree->min_keys && node->parent_id != 0) {
        /* Merge with sibling if needed */
        /* Full implementation would handle merging */
    }
    
    tree->write_node(node->node_id, node);
    tree->free_node(node);
    
    return 0;
}

/* Initialize B+ tree */
int btree_init(struct btree* tree, uint32_t node_size,
               struct btree_node* (*alloc_node)(uint32_t),
               void (*free_node)(struct btree_node*),
               void (*read_node)(uint32_t, struct btree_node*),
               void (*write_node)(uint32_t, struct btree_node*),
               int (*compare_keys)(uint32_t, uint32_t)) {
    if (!tree || !alloc_node || !free_node || !read_node || !write_node || !compare_keys) {
        return -1;
    }
    
    tree->root_id = 0;
    tree->node_size = node_size;
    tree->max_keys = BTREE_DEFAULT_ORDER - 1;
    tree->min_keys = (BTREE_DEFAULT_ORDER - 1) / 2;
    tree->next_node_id = 1;
    tree->alloc_node = alloc_node;
    tree->free_node = free_node;
    tree->read_node = read_node;
    tree->write_node = write_node;
    tree->compare_keys = compare_keys;
    
    return 0;
}

/* Get minimum key */
int btree_min(struct btree* tree, uint32_t* key, uint64_t* value) {
    if (!tree || !key || !value) {
        return -1;
    }
    
    if (tree->root_id == 0) {
        return -1;
    }
    
    struct btree_node* node = tree->alloc_node(tree->root_id);
    if (!node) {
        return -1;
    }
    
    /* Traverse to leftmost leaf */
    while (!node->is_leaf) {
        uint32_t next_id = node->data.child_ids[0];
        tree->free_node(node);
        node = tree->alloc_node(next_id);
        if (!node) {
            return -1;
        }
    }
    
    if (node->key_count == 0) {
        tree->free_node(node);
        return -1;
    }
    
    *key = node->keys[0];
    *value = node->data.values[0];
    
    tree->free_node(node);
    return 0;
}

/* Get maximum key */
int btree_max(struct btree* tree, uint32_t* key, uint64_t* value) {
    if (!tree || !key || !value) {
        return -1;
    }
    
    if (tree->root_id == 0) {
        return -1;
    }
    
    struct btree_node* node = tree->alloc_node(tree->root_id);
    if (!node) {
        return -1;
    }
    
    /* Traverse to rightmost leaf */
    while (!node->is_leaf) {
        uint32_t next_id = node->data.child_ids[node->key_count];
        tree->free_node(node);
        node = tree->alloc_node(next_id);
        if (!node) {
            return -1;
        }
    }
    
    if (node->key_count == 0) {
        tree->free_node(node);
        return -1;
    }
    
    *key = node->keys[node->key_count - 1];
    *value = node->data.values[node->key_count - 1];
    
    tree->free_node(node);
    return 0;
}

/* Range query */
int btree_range_query(struct btree* tree, uint32_t start_key, uint32_t end_key,
                      uint32_t* keys, uint64_t* values, size_t max_results, size_t* result_count) {
    if (!tree || !keys || !values || !result_count) {
        return -1;
    }
    
    *result_count = 0;
    
    if (tree->root_id == 0) {
        return 0;
    }
    
    /* Find starting leaf */
    struct btree_node* node = tree->alloc_node(tree->root_id);
    if (!node) {
        return -1;
    }
    
    /* Traverse to leaf containing start_key */
    while (!node->is_leaf) {
        uint32_t child_idx = 0;
        for (uint32_t i = 0; i < node->key_count; i++) {
            if (tree->compare_keys(start_key, node->keys[i]) < 0) {
                break;
            }
            child_idx = i + 1;
        }
        
        uint32_t next_id = node->data.child_ids[child_idx];
        tree->free_node(node);
        node = tree->alloc_node(next_id);
        if (!node) {
            return -1;
        }
    }
    
    /* Scan from start_key to end_key */
    bool found_start = false;
    while (node && *result_count < max_results) {
        for (uint32_t i = 0; i < node->key_count && *result_count < max_results; i++) {
            if (!found_start) {
                if (tree->compare_keys(node->keys[i], start_key) >= 0) {
                    found_start = true;
                } else {
                    continue;
                }
            }
            
            if (tree->compare_keys(node->keys[i], end_key) > 0) {
                tree->free_node(node);
                return 0;
            }
            
            keys[*result_count] = node->keys[i];
            values[*result_count] = node->data.values[i];
            (*result_count)++;
        }
        
        if (node->next_leaf_id != 0) {
            uint32_t next_id = node->next_leaf_id;
            tree->free_node(node);
            node = tree->alloc_node(next_id);
        } else {
            tree->free_node(node);
            node = NULL;
        }
    }
    
    if (node) {
        tree->free_node(node);
    }
    
    return 0;
}
