/* AstraLisp OS Runtime Test Harness */

#include "reader.h"
#include "evaluator.h"
#include "tagged.h"
#include "../gc/gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Mock kernel functions for standalone testing */
void* kmalloc(size_t size) {
    return malloc(size);
}

void kfree(void* ptr) {
    free(ptr);
}

void test_expression(const char* input, const char* expected_output) {
    printf("Testing: %s\n", input);
    
    struct reader_context ctx;
    reader_init(&ctx, input);
    
    lisp_value expr = reader_read(&ctx);
    
    /* Protect expr from GC */
    GC_PUSH_1(expr);
    
    /* Use global environment */
    lisp_value env = lisp_get_global_env();
    /* No need to push env as it is a root */

    
    lisp_value result = lisp_eval(env, expr);


    
    printf("Result: ");
    lisp_print(result);
    printf("\n");

    GC_POP(); /* expr */
    
    /* Trigger GC to check for crashes */
    gc_collect();
}



int main() {
    setvbuf(stdout, NULL, _IONBF, 0); /* Disable buffering */
    printf("Initializing AstraLisp Runtime...\n");
    
    if (gc_init() != 0) {
        printf("Failed to init GC\n");
        return 1;
    }
    

    
    if (evaluator_init() != 0) {
        printf("Failed to init Evaluator\n");
        return 1;
    }
    
    printf("Runtime Initialized.\n");

    /* Test 1: Integers */
    test_expression("123", "123");
    
    /* Test 2: Arithmetic */
    test_expression("(+ 1 2)", "3");
    test_expression("(* 3 4)", "12");
    test_expression("(- 10 5)", "5");
    test_expression("(/ 20 2)", "10");

    /* Test 3: Cons/List */
    test_expression("(cons 1 2)", "(1 . 2)");
    test_expression("(quote (1 2 3))", "(1 2 3)");

    /* Test 4: Car/Cdr */
    test_expression("(car (quote (1 2)))", "1");
    test_expression("(cdr (quote (1 2)))", "(2)");

    /* Test 5: Lambda/Application */
    test_expression("((lambda (x) (+ x 1)) 10)", "11");

    /* Test 6: Define and Use */
    test_expression("(defun add2 (x) (+ x 2))", "add2");
    test_expression("(add2 5)", "7");

    /* Test 7: Error Handling */
    test_expression("(try (error \"boom\") (lambda (e) \"caught\"))", "\"caught\"");
    test_expression("(try 123 (lambda (e) \"fail\"))", "123");








    
    printf("All tests completed.\n");
    
    /* Final GC stats */
    struct gc_stats stats;
    gc_get_stats(&stats);
    printf("GC Stats: Alloc=%zu, Freed=%zu, Collections=%zu\n", 
           stats.total_allocated, stats.total_freed, stats.collection_count);
    
    return 0;
}
