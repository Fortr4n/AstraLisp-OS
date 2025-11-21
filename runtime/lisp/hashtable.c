/* AstraLisp OS Hash Table Implementation */

#include "hashtable.h"
#include "objects.h"
#include "types.h"
#include "../gc/gc.h"
#include <stdlib.h>
#include <string.h>

#define HT_INITIAL_CAPACITY 16
#define HT_LOAD_FACTOR 0.75f

/* FNV-1a Hash Function */
static uint32_t hash_string(const char* str) {
    uint32_t hash = 2166136261u;
    while (*str) {
        hash ^= (uint8_t)*str++;
        hash *= 16777619u;
    }
    return hash;
}

static uint32_t hash_value(lisp_value key) {
    if (IS_SYMBOL(key)) {
        lisp_value name = SYMBOL_NAME(key);
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(name);
        return hash_string(s->data);
    } else if (IS_STRING(key)) {
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(key);
        return hash_string(s->data);
    } else if (IS_FIXNUM(key)) {
        return (uint32_t)FIXNUM_VAL(key);
    }
    /* Default: use pointer value mixed */
    return (uint32_t)((uintptr_t)key >> 3);
}

static bool keys_equal(lisp_value a, lisp_value b) {
    if (a == b) return true;
    
    if (IS_SYMBOL(a) && IS_SYMBOL(b)) {
        /* If interned, pointer equality is enough. 
           But during bootstrapping/interning, we might compare strings. */
        lisp_value n1 = SYMBOL_NAME(a);
        lisp_value n2 = SYMBOL_NAME(b);
        struct lisp_string* s1 = (struct lisp_string*)PTR_VAL(n1);
        struct lisp_string* s2 = (struct lisp_string*)PTR_VAL(n2);
        return strcmp(s1->data, s2->data) == 0;
    }
    
    if (IS_STRING(a) && IS_STRING(b)) {
        struct lisp_string* s1 = (struct lisp_string*)PTR_VAL(a);
        struct lisp_string* s2 = (struct lisp_string*)PTR_VAL(b);
        return strcmp(s1->data, s2->data) == 0;
    }
    
    return false;
}

struct hash_table* ht_create(uint32_t initial_capacity) {
    if (initial_capacity < 4) initial_capacity = HT_INITIAL_CAPACITY;
    
    /* Power of 2 check/adjustment */
    uint32_t cap = 4;
    while (cap < initial_capacity) cap *= 2;
    
    struct hash_table* ht = (struct hash_table*)kmalloc(sizeof(struct hash_table));
    if (!ht) return NULL;
    
    ht->buckets = (struct hash_entry**)kmalloc(cap * sizeof(struct hash_entry*));
    if (!ht->buckets) {
        kfree(ht);
        return NULL;
    }
    
    memset(ht->buckets, 0, cap * sizeof(struct hash_entry*));
    ht->bucket_count = cap;
    ht->entry_count = 0;
    ht->load_factor = HT_LOAD_FACTOR;
    
    return ht;
}

void ht_destroy(struct hash_table* ht) {
    if (!ht) return;
    
    for (uint32_t i = 0; i < ht->bucket_count; i++) {
        struct hash_entry* entry = ht->buckets[i];
        while (entry) {
            struct hash_entry* next = entry->next;
            kfree(entry);
            entry = next;
        }
    }
    kfree(ht->buckets);
    kfree(ht);
}

void ht_resize(struct hash_table* ht, uint32_t new_capacity) {
    if (!ht) return;
    
    struct hash_entry** new_buckets = (struct hash_entry**)kmalloc(new_capacity * sizeof(struct hash_entry*));
    if (!new_buckets) return;
    
    memset(new_buckets, 0, new_capacity * sizeof(struct hash_entry*));
    
    for (uint32_t i = 0; i < ht->bucket_count; i++) {
        struct hash_entry* entry = ht->buckets[i];
        while (entry) {
            struct hash_entry* next = entry->next;
            
            uint32_t idx = entry->hash & (new_capacity - 1);
            entry->next = new_buckets[idx];
            new_buckets[idx] = entry;
            
            entry = next;
        }
    }
    
    kfree(ht->buckets);
    ht->buckets = new_buckets;
    ht->bucket_count = new_capacity;
}

int ht_put(struct hash_table* ht, lisp_value key, lisp_value value) {
    if (!ht) return -1;
    
    if ((float)ht->entry_count / ht->bucket_count > ht->load_factor) {
        ht_resize(ht, ht->bucket_count * 2);
    }
    
    uint32_t hash = hash_value(key);
    uint32_t idx = hash & (ht->bucket_count - 1);
    
    struct hash_entry* entry = ht->buckets[idx];
    while (entry) {
        if (entry->hash == hash && keys_equal(entry->key, key)) {
            entry->value = value;
            /* Write barrier needed? 
               This is a raw C hash table, not a Lisp object.
               The GC scans roots. If 'ht' is reachable, we need to ensure 
               its contents are marked.
               
               For now, we assume the owner of 'ht' (e.g. Environment object)
               will have a custom marker function that iterates this table.
            */
            return 0;
        }
        entry = entry->next;
    }
    
    struct hash_entry* new_entry = (struct hash_entry*)kmalloc(sizeof(struct hash_entry));
    if (!new_entry) return -1;
    
    new_entry->key = key;
    new_entry->value = value;
    new_entry->hash = hash;
    new_entry->next = ht->buckets[idx];
    ht->buckets[idx] = new_entry;
    ht->entry_count++;
    
    return 0;
}

lisp_value ht_get(struct hash_table* ht, lisp_value key) {
    if (!ht) return LISP_NIL; // Or unbound?
    
    uint32_t hash = hash_value(key);
    uint32_t idx = hash & (ht->bucket_count - 1);
    
    struct hash_entry* entry = ht->buckets[idx];
    while (entry) {
        if (entry->hash == hash && keys_equal(entry->key, key)) {
            return entry->value;
        }
        entry = entry->next;
    }
    
    return LISP_NIL; /* Not found */
}

bool ht_contains(struct hash_table* ht, lisp_value key) {
    if (!ht) return false;
    
    uint32_t hash = hash_value(key);
    uint32_t idx = hash & (ht->bucket_count - 1);
    
    struct hash_entry* entry = ht->buckets[idx];
    while (entry) {
        if (entry->hash == hash && keys_equal(entry->key, key)) {
            return true;
        }
        entry = entry->next;
    }
    return false;
}
