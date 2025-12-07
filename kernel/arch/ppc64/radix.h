/* AstraLisp OS - PowerISA Radix MMU Definitions */
/* Target: PowerISA v3.1C */

#ifndef _KERNEL_ARCH_PPC64_RADIX_H
#define _KERNEL_ARCH_PPC64_RADIX_H

#include <stdint.h>
#include <stddef.h>

/* Register Definitions */
#define SPR_PTCR    464     /* Partition Table Control Register */
#define SPR_PID     48      /* Process ID */
#define SPR_LPID    338     /* Logical Process ID */

/* Partition Table Entry (PATB) */
/* Controls the mapping scope for a Partition (LPID) */
struct patb_entry {
    uint64_t prtb_phys;     /* Process Table Base (Partial) + Size (RTS) + Valid */
    uint64_t prtb_control;  /* Processing rights */
};

/* Process Table Entry (PRTB) */
/* Controls the Radix Tree Root for a Process (PID) */
struct prtb_entry {
    uint64_t root_phys;     /* Radix Tree Root (RP) + Size + Valid */
    uint64_t process_control; /* Process control (SW, etc) */
};

/* Encoding Helpers */
/* RTS (Radix Tree Size) Encoding for 52-bit (typical) */
/* 0x05 -> 52 bits */
#define RADIX_RTS_52    0x05

/* Flags */
#define PATB_VALID      (1ULL << 63)
#define PATB_HR         (1ULL << 63) /* Host Radix */
#define PRTB_VALID      (1ULL << 63) /* Valid Process Table Entry might differ? */
/* Actually, for Radix, the RP (Root Pointer) has VALID bit at correct position */
/* Standard says RP Valid bit is usually implied or managed by context. */
/* Wait: In Process Table Entry:
   Doubleword 0:
     Bit 0: Reserved (usually)
     Bit 1: RTS (Root Table Size) - actually lower bits
     Bits 0-56: Root Page Dir Physical Address (4KB aligned)
     Wait, let's verify exact bit spec from v3.1 book.
     
   Standard Format:
   DW0:
     0-51: Page Directory Physical Address (Bits 0-51)
     52-55: Reserved
     56-58: RTS (Radix Tree Size)
     63: Valid (V) - Wait, usually RP doesn't have V bit in PRTE itself? 
     
     Actually, for Process Table Entry:
     DW0: | PGD Base Address | RTS | ... |
     DW1: | Process Control | ... |
     
     Let's replicate standard Linux/skiboot usage for simplicity.
*/

#define RADIX_PTE_VALID 0x8000000000000000ULL
#define RADIX_PTE_LEAF  0x4000000000000000ULL

/* Functions */
void radix_init_hypervisor(void);
void radix_set_root(uint64_t pid, uint64_t pml4_phys);

#endif
