/* AstraLisp OS Shell */

#ifndef SHELL_H
#define SHELL_H

#include <stdint.h>
#include <stddef.h>

/* Initialize shell */
int shell_init(void);

/* Run shell */
void shell_run(void);

/* Execute command */
int shell_execute(const char* command);

#endif /* SHELL_H */
