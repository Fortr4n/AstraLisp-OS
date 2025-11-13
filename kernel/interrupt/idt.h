/* AstraLisp OS Interrupt Descriptor Table */

#ifndef IDT_H
#define IDT_H

#include <stdint.h>

/* Interrupt handler type */
typedef void (*interrupt_handler_t)(uint32_t interrupt_number, void* stack_frame);

/* Initialize IDT */
int idt_init(void);

/* Register interrupt handler */
void idt_register_handler(uint32_t interrupt_number, interrupt_handler_t handler);

/* Enable interrupts */
void idt_enable_interrupts(void);

/* Disable interrupts */
void idt_disable_interrupts(void);

#endif /* IDT_H */
