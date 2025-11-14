/* AstraLisp OS Shell Implementation */

#include "shell.h"
#include "../../kernel/hal/serial.h"
#include <stddef.h>
#include <string.h>

static char prompt[] = "astralisp> ";

/* Initialize shell */
int shell_init(void) {
    return 0;
}

/* Run shell */
void shell_run(void) {
    char buffer[256];
    
    while (1) {
        serial_puts(prompt);
        
        /* Read input (placeholder) */
        buffer[0] = '\0';
        
        if (strlen(buffer) > 0) {
            shell_execute(buffer);
        }
    }
}

/* Execute command */
int shell_execute(const char* command) {
    if (!command) {
        return -1;
    }
    
    /* Placeholder - full implementation would:
     * 1. Parse command
     * 2. Execute built-in or external command
     * 3. Handle pipes and redirection
     */
    
    serial_puts("Command: ");
    serial_puts(command);
    serial_puts("\n");
    
    return 0;
}
