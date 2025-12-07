/* AstraLisp OS - CPU Context Structure */
/* Target: PowerISA v3.1 LE */

#ifndef _KERNEL_ASM_CONTEXT_SWITCH_H
#define _KERNEL_ASM_CONTEXT_SWITCH_H

#include <stdint.h>

/* Context structure for context switching */
/* Saved by context_switch assembly */
struct cpu_context {
    uint64_t r1;  /* Stack Pointer */
    uint64_t r2;  /* TOC Pointer */
    uint64_t r13; /* Thread-Local Storage / PCR */
    uint64_t r14;
    uint64_t r15;
    uint64_t r16;
    uint64_t r17;
    uint64_t r18;
    uint64_t r19;
    uint64_t r20;
    uint64_t r21;
    uint64_t r22;
    uint64_t r23;
    uint64_t r24;
    uint64_t r25;
    uint64_t r26;
    uint64_t r27;
    uint64_t r28;
    uint64_t r29;
    uint64_t r30;
    uint64_t r31; /* Frame Pointer usually */
    
    uint64_t lr;  /* Link Register (Return Address) */
    uint64_t cr;  /* Condition Register */
    uint64_t pc;  /* Program Counter (for new threads) */
    uint64_t msr; /* Machine State Register */
};

/* Function prototype */
/* void context_switch(struct cpu_context* prev, struct cpu_context* next); */
/* Assembly implemented */
void context_switch(struct cpu_context* prev, struct cpu_context* next);

#endif
