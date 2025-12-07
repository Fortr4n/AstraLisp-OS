/* AstraLisp OS Bytecode Compiler
 * 
 * Compile Lisp AST to bytecode for VM execution.
 */

#ifndef BC_COMPILER_H
#define BC_COMPILER_H

#include "bytecode.h"
#include "tagged.h"

/* Compile expression to bytecode chunk */
struct bc_chunk* bc_compile(lisp_value expr);

/* Compile source string to bytecode chunk */
struct bc_chunk* bc_compile_string(const char* source);

#endif /* BC_COMPILER_H */
