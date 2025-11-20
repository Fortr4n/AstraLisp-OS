#ifndef CONTEXT_SWITCH_H
#define CONTEXT_SWITCH_H

#include <stdint.h>

struct cpu_context {
    uint64_t gpr[32];
    uint64_t cr;
    uint64_t xer;
    uint64_t ctr;
    uint64_t lr;
    uint64_t pc;
    uint64_t msr;
};

void context_switch(struct cpu_context* prev, struct cpu_context* next);

#endif /* CONTEXT_SWITCH_H */
