/* AstraLisp OS - PowerISA v3.1C Exception Vectors */
/* Target: ppc64le */

.section .text
.global _vectors_start

/* Macro for exception vector entry */
/* Saves minimal state and jumps to common handler */
.macro EXCEPTION_VECTOR offset, name
    . = \offset
    .global vector_\name
vector_\name:
    mtsprg1 13        /* Save r13 to SPRG1 (scratch) */
    mfspr 13, 272     /* Read SPRG0 (kernel stack pointer) - assuming set up already */
    /* If SPRG0 is 0, we are already in kernel, so use current stack? 
       For simplicity in phase 0, assuming we are always converting from "userspace" or just using current stack if kernel.
       Actually, standard practice: Check MSR[PR]. If 0 (kernel), use current r1. If 1 (user), load kernel stack.
       For now, let's assume simple kernel-only operation for Phase 0.
    */
    
    /* Simple save of r0-r3 to check what's happening */
    std 0, -32(1)
    std 1, -24(1)     /* Save original r1 */
    std 2, -16(1)
    std 3, -8(1)
    
    li 3, \offset     /* Load vector offset into r3 (arg1) */
    b exception_common
.endm

_vectors_start:

/* 0x100 - System Reset */
EXCEPTION_VECTOR 0x100, system_reset

/* 0x200 - Machine Check */
EXCEPTION_VECTOR 0x200, machine_check

/* 0x300 - Data Storage (Page Fault) */
EXCEPTION_VECTOR 0x300, data_storage

/* 0x400 - Instruction Storage (Page Fault) */
EXCEPTION_VECTOR 0x400, instruction_storage

/* 0x500 - External Interrupt */
EXCEPTION_VECTOR 0x500, external_interrupt

/* 0x600 - Alignment */
EXCEPTION_VECTOR 0x600, alignment

/* 0x700 - Program Exception */
EXCEPTION_VECTOR 0x700, program

/* 0x800 - FPU Unavailable */
EXCEPTION_VECTOR 0x800, fpu_unavailable

/* 0x900 - Decrementer */
EXCEPTION_VECTOR 0x900, decrementer

/* 0xC00 - System Call */
EXCEPTION_VECTOR 0xC00, syscall

/* Common Exception Handler Stub */
exception_common:
    /* Note: r3 contains trap number */
    /* Stack frame setup would go here (save volatile regs, etc) */
    /* For Phase 0, we just want to catch the trap and panic/log */
    
    /* Restore r13 (we clobbered it in macro, but for this simple stub we saved it to SPRG1) */
    mfsprg1 13
    
    /* Call C handler: void handle_exception(uint32_t trap_num) */
    /* r3 is already trap_num */
    
    /* We need to save context if we want to return. 
       For Phase 0 "Panic on exception", we don't strictly need to save everything yet.
       But let's establish a basic stack frame.
    */
    stdu 1, -288(1)   /* Allocate stack frame (approx size) */
    
    bla handle_exception
    nop
    
    /* Loop forever if handler returns */
_trap_hang:
    b _trap_hang
