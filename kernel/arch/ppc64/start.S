/* AstraLisp OS - PowerISA v3.1C Kernel Entry */
/* Target: POWER10 (ppc64le) */

.section .multiboot
.align 3
multiboot_header:
    /* Magic */
    .long 0xE85250D6
    /* Architecture: 0 (i386) - using generic ID as PPC specific is implementation defined in some GRUB versions */
    .long 0
    /* Header Length */
    .long multiboot_header_end - multiboot_header
    /* Checksum */
    .long -(0xE85250D6 + 0 + (multiboot_header_end - multiboot_header))

    /* Tags can go here */

    /* End Tag */
    .short 0
    .short 0
    .long 8
multiboot_header_end:

.section .text
.global _start
.type _start, @function

_start:
    /* 0. Load TOC (Table of Contents) pointer into r2 */
    /* This is critical for ppc64le ELFv2 ABI */
    addis 2, 12, .TOC.-_start@ha
    addi  2, 2, .TOC.-_start@l
    .localentry _start, .-_start

    /* 1. Setup Stack */
    /* Load stack_top address into r1 */
    lis 1, stack_top@highest
    ori 1, 1, stack_top@higher
    rldicr 1, 1, 32, 31
    oris 1, 1, stack_top@h
    ori 1, 1, stack_top@l
    
    /* Zero backchain pointer (terminates stack traces) */
    li 0, 0
    std 0, 0(1)

    /* 2. Preserve Boot Information */
    /* r3 contains Magic, r4 contains Multiboot Info Pointer */
    /* Save to non-volatile registers (r20, r21) */
    mr 20, 3
    mr 21, 4

    /* 3. Verify CPU Architecture (POWER10) */
    bl verify_power10
    nop

    /* 4. Call Early Init (C code) */
    bl early_init
    nop /* ELFv2 linker stub placeholder */

    /* 4. Prepare Arguments for Kernel Main */
    /* r3 already contains FDT pointer from Skiboot/firmware entry */
    /* We saved it in r20. Restore it to r3. */
    mr 3, 20
    
    /* 5. Jump to Kernel Main */
    bl kernel_main
    nop

    /* 6. Safety Loop */
_hang:
    /* PowerISA 'doze' or equivalent could go here inside loop */
    b _hang

.section .bss
.align 16
.global stack_bottom
stack_bottom:
    .skip 16384 /* 16KB Boot Stack */
.global stack_top
stack_top:
