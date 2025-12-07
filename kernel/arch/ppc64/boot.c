/* AstraLisp OS - PowerISA Early Boot Initialization */

#include <stdint.h>
#include <stddef.h>

/* Linker symbols for BSS section */
extern uint8_t __bss_start;
extern uint8_t __bss_end;
extern uint8_t _end;

/* Early Initialization */
/* Called from start.S before kernel_main */
void early_init(void) {
    /* 1. Clear BSS Section */
    /* Accessing linker symbols directly */
    uint8_t* bss = &__bss_start;
    uint8_t* bss_end = &__bss_end;

    while (bss < bss_end) {
        *bss++ = 0;
    }

    /* 2. Future: Enable FPU/Altivec/VSX/Radix translation here or in start.S */
    /* This ensures a clean environment for kernel_main */
}
