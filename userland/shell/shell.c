/* AstraLisp OS Shell Complete Implementation */

#include "shell.h"
#include "../../kernel/hal/serial.h"
#include "../../kernel/mm/heap.h"
#include "../../runtime/runtime.h"
#include "../../runtime/lisp/reader.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

static char prompt[] = "astralisp$ ";
static bool shell_initialized = false;

/* Command structure */
struct shell_command {
    char* name;
    int (*func)(int argc, char** argv);
    struct shell_command* next;
};

static struct shell_command* command_list = NULL;

/* Built-in commands */
static int cmd_echo(int argc, char** argv) {
    for (int i = 1; i < argc; i++) {
        serial_puts(argv[i]);
        if (i < argc - 1) {
            serial_puts(" ");
        }
    }
    serial_puts("\n");
    return 0;
}

static int cmd_lisp(int argc, char** argv) {
    if (argc < 2) {
        serial_puts("Usage: lisp <expression>\n");
        return -1;
    }
    
    char expr[1024] = {0};
    for (int i = 1; i < argc; i++) {
        strcat(expr, argv[i]);
        if (i < argc - 1) {
            strcat(expr, " ");
        }
    }
    
    struct lisp_object* result = runtime_read(expr);
    if (result) {
        struct lisp_object* eval_result = runtime_eval(result);
        if (eval_result) {
            runtime_print(eval_result);
            serial_puts("\n");
            lisp_decref(eval_result);
        }
        lisp_decref(result);
    }
    
    return 0;
}

static int cmd_help(int argc, char** argv) {
    serial_puts("Available commands:\n");
    serial_puts("  echo <text>     - Print text\n");
    serial_puts("  lisp <expr>     - Evaluate Lisp expression\n");
    serial_puts("  help            - Show this help\n");
    return 0;
}

/* Register command */
static void register_command(const char* name, int (*func)(int, char**)) {
    struct shell_command* cmd = (struct shell_command*)kmalloc(sizeof(struct shell_command));
    if (cmd) {
        cmd->name = (char*)kmalloc(strlen(name) + 1);
        if (cmd->name) {
            strcpy(cmd->name, name);
            cmd->func = func;
            cmd->next = command_list;
            command_list = cmd;
        } else {
            kfree(cmd);
        }
    }
}

/* Parse command line */
static int parse_command(const char* line, char** argv, int max_args) {
    if (!line || !argv || max_args <= 0) {
        return 0;
    }
    
    int argc = 0;
    const char* p = line;
    bool in_quotes = false;
    char* arg_start = NULL;
    
    while (*p && argc < max_args - 1) {
        if (*p == '"') {
            in_quotes = !in_quotes;
            p++;
        } else if (*p == ' ' && !in_quotes) {
            if (arg_start) {
                size_t len = p - arg_start;
                argv[argc] = (char*)kmalloc(len + 1);
                if (argv[argc]) {
                    memcpy(argv[argc], arg_start, len);
                    argv[argc][len] = '\0';
                    argc++;
                }
                arg_start = NULL;
            }
            p++;
        } else {
            if (!arg_start) {
                arg_start = (char*)p;
            }
            p++;
        }
    }
    
    if (arg_start && argc < max_args - 1) {
        size_t len = p - arg_start;
        argv[argc] = (char*)kmalloc(len + 1);
        if (argv[argc]) {
            memcpy(argv[argc], arg_start, len);
            argv[argc][len] = '\0';
            argc++;
        }
    }
    
    argv[argc] = NULL;
    return argc;
}

/* Read line */
static int read_line(char* buffer, size_t size) {
    if (!buffer || size == 0) {
        return -1;
    }
    
    size_t pos = 0;
    while (pos < size - 1) {
        int c = serial_getchar();
        if (c < 0) {
            continue;
        }
        
        if (c == '\n' || c == '\r') {
            buffer[pos] = '\0';
            serial_puts("\n");
            return (int)pos;
        } else if (c == '\b' || c == 127) {
            if (pos > 0) {
                pos--;
                serial_putchar('\b');
                serial_putchar(' ');
                serial_putchar('\b');
            }
        } else if (c >= 32 && c < 127) {
            buffer[pos++] = (char)c;
            serial_putchar(c);
        }
    }
    
    buffer[pos] = '\0';
    return (int)pos;
}

/* Initialize shell */
int shell_init(void) {
    if (shell_initialized) {
        return 0;
    }
    
    if (runtime_init() != 0) {
        return -1;
    }
    
    /* Register built-in commands */
    register_command("echo", cmd_echo);
    register_command("lisp", cmd_lisp);
    register_command("help", cmd_help);
    
    shell_initialized = true;
    return 0;
}

/* Run shell */
void shell_run(void) {
    if (!shell_initialized) {
        if (shell_init() != 0) {
            serial_puts("Failed to initialize shell\n");
            return;
        }
    }
    
    char buffer[1024];
    char* argv[64];
    
    while (1) {
        serial_puts(prompt);
        
        int len = read_line(buffer, sizeof(buffer));
        if (len <= 0) {
            continue;
        }
        
        if (strlen(buffer) == 0) {
            continue;
        }
        
        /* Parse command */
        int argc = parse_command(buffer, argv, 64);
        if (argc == 0) {
            continue;
        }
        
        /* Find and execute command */
        struct shell_command* cmd = command_list;
        bool found = false;
        
        while (cmd) {
            if (strcmp(cmd->name, argv[0]) == 0) {
                cmd->func(argc, argv);
                found = true;
                break;
            }
            cmd = cmd->next;
        }
        
        if (!found) {
            serial_puts("Command not found: ");
            serial_puts(argv[0]);
            serial_puts("\n");
        }
        
        /* Free arguments */
        for (int i = 0; i < argc; i++) {
            if (argv[i]) {
                kfree(argv[i]);
            }
        }
    }
}

/* Execute command */
int shell_execute(const char* command) {
    if (!command) {
        return -1;
    }
    
    char* argv[64];
    int argc = parse_command(command, argv, 64);
    if (argc == 0) {
        return -1;
    }
    
    struct shell_command* cmd = command_list;
    while (cmd) {
        if (strcmp(cmd->name, argv[0]) == 0) {
            int result = cmd->func(argc, argv);
            for (int i = 0; i < argc; i++) {
                if (argv[i]) {
                    kfree(argv[i]);
                }
            }
            return result;
        }
        cmd = cmd->next;
    }
    
    for (int i = 0; i < argc; i++) {
        if (argv[i]) {
            kfree(argv[i]);
        }
    }
    
    return -1;
}
