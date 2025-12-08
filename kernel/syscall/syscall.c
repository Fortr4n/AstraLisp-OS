/* AstraLisp OS - System Call Dispatcher */

#include "syscall.h"
#include "../arch/ppc64/regs.h"
#include "../process/process.h" /* For thread_exit */
#include "../arch/ppc64/smp.h"
#include <stddef.h>

/* Syscall Numbers */
#define SYS_EXIT    1
#define SYS_READ    3
#define SYS_WRITE   4
#define SYS_OPEN    5
#define SYS_CLOSE   6
#define SYS_YIELD   120
#define SYS_SPAWN   121

/* Forward declarations of handlers */
void sys_exit(int status);
int sys_write(int fd, const char* buf, int len);
int sys_read(int fd, char* buf, int len);
void sys_yield(void);

/* Handler implementation */
void sys_exit(int status) {
    (void)status;
    thread_exit();
}

void sys_yield(void) {
    /* schedule(); - requires exposing schedule() */
    extern void schedule(void);
    schedule();
}

int sys_write(int fd, const char* buf, int len) {
    if (fd == 1 || fd == 2) {
        /* Stdout/Stderr -> OPAL Console */
        extern void opal_putc(char c);
        for (int i = 0; i < len; i++) {
            opal_putc(buf[i]);
        }
        return len;
    }
    return -1;
}

int sys_read(int fd, char* buf, int len) {
    (void)fd; (void)buf; (void)len;
    return -1; /* TODO: Input */
}

/* Dispatcher */
void syscall_handler(struct pt_regs* regs) {
    uint64_t syscall_num = regs->gpr[0];
    uint64_t ret = 0;
    
    switch (syscall_num) {
        case SYS_EXIT:
            sys_exit((int)regs->gpr[3]);
            break;
        case SYS_WRITE:
            ret = sys_write((int)regs->gpr[3], (const char*)regs->gpr[4], (int)regs->gpr[5]);
            break;
        case SYS_READ:
            ret = sys_read((int)regs->gpr[3], (char*)regs->gpr[4], (int)regs->gpr[5]);
            break;
        case SYS_YIELD:
            sys_yield();
            break;
        case 11: /* SYS_EXEC */
            {
                const char* path = (const char*)regs->gpr[3];
                ret = process_exec(path, NULL, NULL);
            }
            break;
        default:
            ret = -1; /* ENOSYS */
            break;
    }
    
    /* Set Return Value */
    regs->gpr[3] = ret;
}
