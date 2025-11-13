/* AstraLisp OS Integration Tests */

#ifndef INTEGRATION_H
#define INTEGRATION_H

#include <stdint.h>

/* Run integration tests */
int integration_test_boot(void);
int integration_test_memory(void);
int integration_test_filesystem(void);
int integration_test_network(void);

/* Run all integration tests */
int integration_run_all(void);

#endif /* INTEGRATION_H */
