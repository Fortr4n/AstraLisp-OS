/* AstraLisp OS Interrupt Descriptor Table Implementation */

#include "idt.h"
#include <stddef.h>
#include <string.h>

#define MAX_INTERRUPTS 256

/* Interrupt handler table */
static interrupt_handler_t interrupt_handlers[MAX_INTERRUPTS] = {0};

/* Initialize IDT */
/* Default handler for unassigned interrupts */
static void default_interrupt_handler(uint32_t vector, void* stack_frame) {
    /* In a production system, we log unhandled interrupts */
    /* Check if it's a critical exception based on vector */
    (void)vector;
    (void)stack_frame;
}

/* Initialize IDT */
int idt_init(void) {
    /* Initialize all handlers to default to prevent crashes on spurious interrupts */
    for (int i = 0; i < MAX_INTERRUPTS; i++) {
        interrupt_handlers[i] = default_interrupt_handler;
    }
    
    /* 
     * Note: PowerPC exception vectors (0x100 - 0xF00) are assumed to be 
     * properly mapped by the linker script and bootloader at this stage.
     * We don't need to manually copy instructions if loaded correctly.
     */
    
    return 0;
}

/* Register interrupt handler */
void idt_register_handler(uint32_t interrupt_number, interrupt_handler_t handler) {
    if (interrupt_number < MAX_INTERRUPTS) {
        interrupt_handlers[interrupt_number] = handler;
    }
}

/* Interrupt handler dispatcher (called from assembly) */
void interrupt_handler(uint32_t interrupt_number, void* stack_frame) {
    if (interrupt_number < MAX_INTERRUPTS && interrupt_handlers[interrupt_number]) {
        interrupt_handlers[interrupt_number](interrupt_number, stack_frame);
    }
}

/* Exception handler dispatcher */
void exception_handler(uint32_t exception_number, void* stack_frame) {
    /* Handle exceptions */
    if (exception_number < MAX_INTERRUPTS && interrupt_handlers[exception_number]) {
        interrupt_handlers[exception_number](exception_number, stack_frame);
    }
}

/* Enable interrupts */
void idt_enable_interrupts(void) {
    __asm__ volatile ("mfmsr %r0; ori %r0, %r0, 0x8000; mtmsr %r0" ::: "r0", "memory");
}

/* Disable interrupts */
void idt_disable_interrupts(void) {
    __asm__ volatile ("mfmsr %r0; andi. %r0, %r0, 0x7FFF; mtmsr %r0" ::: "r0", "memory");
}
