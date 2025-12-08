/* AstraLisp OS - PowerISA Register Structures */

#ifndef ARCH_PPC64_REGS_H
#define ARCH_PPC64_REGS_H

#include <stdint.h>

/*
 * pt_regs: Stack frame layout for exceptions/syscalls.
 * This MUST match the stack layout created by vectors.S
 */
struct pt_regs {
    uint64_t gpr[32];   /* General Purpose Registers 0-31 */
    uint64_t nip;       /* Next Instruction Pointer (SRR0) */
    uint64_t msr;       /* Machine State Register (SRR1) */
    uint64_t ctr;       /* Count Register */
    uint64_t lr;        /* Link Register */
    uint64_t xer;       /* Fixed Point Exception Register */
    uint64_t ccr;       /* Condition Register */
    uint64_t dar;       /* Data Address Register (Fault addr) */
    uint64_t dsisr;     /* Data Storage Interrupt Status */
    uint64_t orig_gpr3; /* Original R3 (Syscall arg0/num) */
    uint64_t trap;      /* Trap Number (Exception Vector) */
    uint64_t result;    /* Result (for syscall return) */
};

#endif
