#ifndef SYSCALL_H
#define SYSCALL_H

#include "../arch/ppc64/regs.h"

void syscall_handler(struct pt_regs* regs);

#endif
