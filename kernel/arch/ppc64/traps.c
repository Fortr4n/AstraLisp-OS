/* AstraLisp OS - PowerISA Exception Handlers */

#include <stdint.h>

/* Forward declaration for output */
extern void early_printf(const char* format, ...);
extern void kernel_panic(const char* message);

/* Trap names for debugging */
static const char* get_trap_name(uint32_t trap_num) {
    switch (trap_num) {
        case 0x100: return "System Reset";
        case 0x200: return "Machine Check";
        case 0x300: return "Data Storage";
        case 0x400: return "Instruction Storage";
        case 0x500: return "External Interrupt";
        case 0x600: return "Alignment";
        case 0x700: return "Program";
        case 0x800: return "FPU Unavailable";
        case 0x900: return "Decrementer";
        case 0xC00: return "System Call";
        default:    return "Unknown Exception";
    }
}

/* Main exception handler called from vectors.S */
/* Arg1 (r3): trap_num */
void handle_exception(uint32_t trap_num) {
    /* For Phase 0, we largely just report and panic/loop, 
       unless it's a benign interrupt we aren't handling yet. */
       
    const char* name = get_trap_name(trap_num);
    
    early_printf("\n!!! EXCEPTION: %s (0x%x) !!!\n", name, trap_num);
    
    if (trap_num == 0x200 || trap_num == 0x300 || trap_num == 0x400) {
        /* Fatal faults */
        kernel_panic("Fatal Exception");
    }
    
    /* For now, just hang on everything else too to avoid return-to-nowhere */
    for (;;) {
        __asm__ volatile ("nop");
    }
}
