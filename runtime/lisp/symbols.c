/* AstraLisp OS Symbol Interning */

#include "objects.h"
#include "types.h"
#include "hashtable.h"
#include "../gc/gc.h"
#include <string.h>
#include <stdlib.h>

/* Global Symbol Table */
static struct hash_table* symbol_table = NULL;

/* Initialize symbol system */
int symbols_init(void) {
    symbol_table = ht_create(1024);
    if (!symbol_table) return -1;
    return 0;
}

/* Intern a symbol */
lisp_value lisp_intern(const char* name) {
    if (!symbol_table) {
        if (symbols_init() != 0) return LISP_NIL;
    }
    
    /* Create temporary string key for lookup */
    /* Note: In a real system we might want to avoid alloc if possible, 
       but our hash table expects lisp_value keys. 
       We can't easily create a stack-based lisp_value string.
       
       Optimization: Check if we can lookup by raw char*?
       Our ht_get takes lisp_value.
       
       Let's create a temporary string object.
       If found, we discard the temp (GC handles it).
       If not found, we use it for the new symbol.
    */
    
    size_t len = strlen(name);
    struct lisp_string* str = (struct lisp_string*)gc_alloc(sizeof(struct lisp_string) + len + 1);
    if (!str) return LISP_NIL;
    
    SET_TYPE(&str->header, TYPE_STRING);
    str->length = len;
    strcpy(str->data, name);
    
    lisp_value name_val = PTR_TO_VAL(str);
    GC_PUSH_1(name_val);
    
    /* Lookup */
    lisp_value existing = ht_get(symbol_table, name_val);
    if (!IS_NIL(existing)) {
        GC_POP();
        return existing;
    }
    
    /* Not found, create new symbol */
    struct lisp_symbol* sym = (struct lisp_symbol*)gc_alloc(sizeof(struct lisp_symbol));
    if (!sym) {
        GC_POP();
        return LISP_NIL;
    }
    
    SET_TYPE(&sym->header, TYPE_SYMBOL);
    sym->name = name_val;
    sym->value = LISP_NIL;
    sym->function = LISP_NIL;
    sym->plist = LISP_NIL;
    sym->package = LISP_NIL;
    sym->hash = 0; /* Todo: store hash */

    
    lisp_value sym_val = PTR_TO_VAL(sym);
    gc_write_barrier(sym_val, &sym->name, name_val);
    
    /* Add to table */
    ht_put(symbol_table, name_val, sym_val);
    
    /* Note: Symbol table is a root? 
       We need to register symbol_table as a root or have a global root pointing to it.
       Currently gc.c doesn't know about symbol_table.
       We should add it.
    */
    
    GC_POP();
    return sym_val;
}

/* Accessor for symbol table (for GC) */
struct hash_table* lisp_get_symbol_table(void) {
    return symbol_table;
}
