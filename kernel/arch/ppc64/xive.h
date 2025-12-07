/* AstraLisp OS - PowerISA XIVE Interrupt Controller */
/* Target: PowerISA v3.0+ (POWER9/POWER10) */

#ifndef _KERNEL_ARCH_PPC64_XIVE_H
#define _KERNEL_ARCH_PPC64_XIVE_H

#include <stdint.h>

/* Helper Constants */
#define XIVE_ESB_PAGE_SIZE  0x10000 /* 64KB */
#define XIVE_TIMA_PAGE_SIZE 0x10000 /* 64KB */

/* OPAL Return values specific to XIVE might exist, but we use standard OPAL */

/* Thread Interrupt Management Area (TIMA) Rings */
#define TM_QW0_USER     0x00 /* User Ring */
#define TM_QW1_OS       0x10 /* OS Ring */
#define TM_QW2_POOL     0x20 /* Pool Ring */
#define TM_QW3_PHYS     0x30 /* Physical Ring */

/* Offsets within a Ring (Quad Word) */
#define TM_NSR      0x00 /* Notification Source Register */
#define TM_CPPR     0x01 /* Current Processor Priority Register */
#define TM_IPB      0x02 /* Interrupt Pending Buffer */
#define TM_LSMFB    0x03 /* ... */
#define TM_WT       0x04 /* W0 Tag */
#define TM_NVT      0x05 /* ... */
#define TM_PIP      0x06 /* ... */
#define TM_IPF      0x07 /* ... */
#define TM_SPC_PULL 0x08 /* Special Pull */

/* Global XIVE Operations */
int xive_init(void* fdt);
void xive_cpu_init(uint32_t cpu_id);
int xive_register_irq(uint32_t global_irq, uint32_t target_cpu, uint8_t priority);

#endif
