/* AstraLisp OS Unit Test Framework */

#ifndef TEST_H
#define TEST_H

#include <stdint.h>
#include <stdbool.h>

/* Test function type */
typedef void (*test_func_t)(void);

/* Test structure */
struct test {
    const char* name;
    test_func_t func;
    struct test* next;
};

/* Register test */
void test_register(const char* name, test_func_t func);

/* Run all tests */
int test_run_all(void);

/* Assert macros */
#define TEST_ASSERT(condition) \
    do { \
        if (!(condition)) { \
            test_fail(__FILE__, __LINE__, #condition); \
            return; \
        } \
    } while (0)

#define TEST_ASSERT_EQ(a, b) TEST_ASSERT((a) == (b))
#define TEST_ASSERT_NE(a, b) TEST_ASSERT((a) != (b))
#define TEST_ASSERT_NULL(ptr) TEST_ASSERT((ptr) == NULL)
#define TEST_ASSERT_NOT_NULL(ptr) TEST_ASSERT((ptr) != NULL)

/* Internal functions */
void test_fail(const char* file, int line, const char* condition);

#endif /* TEST_H */
