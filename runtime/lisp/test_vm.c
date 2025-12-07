/* AstraLisp OS VM Test Harness */

#include "bytecode.h"
#include "bc_compiler.h"
#include "vm.h"
#include "reader.h"
#include "evaluator.h"
#include "../gc/gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Kernel stubs */
void* kmalloc(size_t size) { return malloc(size); }
void kfree(void* ptr) { free(ptr); }

/* Test helper */
static void test_vm_expression(const char* source, const char* expected) {
    printf("VM Test: %s\n", source);
    
    /* Compile to bytecode */
    struct bc_chunk* chunk = bc_compile_string(source);
    if (!chunk) {
        printf("  FAIL: Compilation failed\n");
        return;
    }
    
    /* Disassemble for debugging */
    printf("  Bytecode:\n");
    bc_disassemble(chunk->main_function);
    
    /* Execute in VM */
    struct vm_context vm;
    if (vm_init(&vm) != 0) {
        printf("  FAIL: VM init failed\n");
        bc_chunk_free(chunk);
        return;
    }
    
    vm_result_t result = vm_execute(&vm, chunk);
    
    if (result == VM_OK) {
        printf("  Result: ");
        lisp_value val = vm_get_result(&vm);
        lisp_print(val);
        printf("\n");
        
        printf("  Stats: %llu instructions, %llu calls\n", 
               (unsigned long long)vm.instructions_executed,
               (unsigned long long)vm.calls_made);
    } else {
        printf("  FAIL: VM execution error (%d)\n", result);
        if (vm.last_error) {
            printf("  Error: %s\n", vm.last_error);
        }
    }
    
    vm_free(&vm);
    bc_chunk_free(chunk);
    printf("\n");
}

int main() {
    setvbuf(stdout, NULL, _IONBF, 0);
    printf("=== AstraLisp VM Test Suite ===\n\n");
    
    /* Initialize GC */
    if (gc_init() != 0) {
        printf("Failed to init GC\n");
        return 1;
    }
    
    /* Initialize evaluator (for globals) */
    if (evaluator_init() != 0) {
        printf("Failed to init Evaluator\n");
        return 1;
    }
    
    /* Test 1: Simple integer */
    test_vm_expression("42", "42");
    
    /* Test 2: Arithmetic */
    test_vm_expression("(+ 1 2)", "3");
    test_vm_expression("(* 3 4)", "12");
    test_vm_expression("(- 10 5)", "5");
    test_vm_expression("(/ 20 4)", "5");
    
    /* Test 3: Nested arithmetic */
    test_vm_expression("(+ (* 2 3) (- 10 5))", "11");
    
    /* Test 4: Comparisons */
    test_vm_expression("(< 1 2)", "t");
    test_vm_expression("(> 1 2)", "nil");
    
    /* Test 5: If expression */
    test_vm_expression("(if t 1 2)", "1");
    test_vm_expression("(if nil 1 2)", "2");
    
    /* Test 6: Lambda */
    test_vm_expression("((lambda (x) (+ x 1)) 10)", "11");
    
    /* Test 7: List operations */
    test_vm_expression("(car (cons 1 2))", "1");
    test_vm_expression("(cdr (cons 1 2))", "2");
    
    printf("=== VM Tests Complete ===\n");
    
    return 0;
}
