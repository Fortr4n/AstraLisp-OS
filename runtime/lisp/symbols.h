/* AstraLisp OS Symbol Management */

#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "tagged.h"

/* Initialize symbol system */
int symbols_init(void);

/* Intern a symbol (find existing or create new) */
lisp_value lisp_intern(const char* name);

/* Get symbol table (for GC) */
struct hash_table* lisp_get_symbol_table(void);

#endif /* SYMBOLS_H */
