/* AstraLisp OS Unit Test Framework Implementation */

#include "test.h"
#include "../../kernel/hal/serial.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>

static struct test* test_list = NULL;
static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

/* Register test */
void test_register(const char* name, test_func_t func) {
    struct test* test = (struct test*)kmalloc(sizeof(struct test));
    if (!test) {
        return;
    }
    
    test->name = name;
    test->func = func;
    test->next = test_list;
    test_list = test;
}

/* Run all tests */
int test_run_all(void) {
    tests_run = 0;
    tests_passed = 0;
    tests_failed = 0;
    
    serial_puts("Running unit tests...\n");
    
    struct test* test = test_list;
    while (test) {
        serial_puts("Test: ");
        serial_puts(test->name);
        serial_puts("... ");
        
        tests_run++;
        
        /* Run test */
        test->func();
        
        tests_passed++;
        serial_puts("PASS\n");
        
        test = test->next;
    }
    
    serial_puts("\nTest Results:\n");
    serial_puts("  Total: ");
    /* Print numbers (simplified) */
    serial_puts("  Passed: ");
    serial_puts("  Failed: ");
    serial_puts("\n");
    
    return tests_failed == 0 ? 0 : 1;
}

/* Internal functions */
void test_fail(const char* file, int line, const char* condition) {
    tests_failed++;
    tests_passed--;
    
    serial_puts("FAIL\n");
    serial_puts("  File: ");
    serial_puts(file);
    serial_puts("\n  Line: ");
    /* Print line number (simplified) */
    serial_puts("\n  Condition: ");
    serial_puts(condition);
    serial_puts("\n");
}
