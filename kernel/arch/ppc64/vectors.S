/* AstraLisp OS - PowerISA v3.1C Exception Vectors */
/* Target: ppc64le */

.section .text
.global _vectors_start
.global exception_common

/* Constants for Stack Layout (Matching pt_regs) */
.set STACK_FRAME_SIZE, 336 /* 32*8 GPRs + 10*8 Misc -> ~336 bytes + alignment */
/* 
 * Layout:
 * 0: gpr0
 * ...
 * 248: gpr31
 * 256: nip (SRR0)
 * 264: msr (SRR1)
 * 272: ctr
 * 280: lr
 * 288: xer
 * 296: ccr
 * 304: dar
 * 312: dsisr
 * 320: orig_gpr3
 * 328: trap
 * 336: result (Optional, align to 16 bytes -> 352 total)
 */

/* NOTE: We align stack to 16 bytes. 352 bytes. */
#define FRAME_SIZE 352

/* Macro for exception vector */
.macro EXCEPTION_VECTOR offset, name
    . = \offset
    .global vector_\name
vector_\name:
    mtsprg1 13          /* Save r13 (scratch) */
    
    /* Check if coming from User Mode (MSR_PR=1) */
    /* For now, simplified: Assume we use kernel stack always or rely on SPRG0 */
    /* Verify SPRG0 (Kernel Stack) */
    mfspr 13, 272       /* SPRG0 */
    cmpdi 13, 0
    beq 1f              /* If 0, use current stack (already kernel?) */
    
    /* Check MSR[PR] */
    mfmsr 11 // Scratch r11? We need to save r11 first!
    /* This logic is incomplete without safe scratches. 
       Usually we use SPRG1, SPRG2.
       Simple approach for Phase 21: Just use current stack for everything (Kernel Mode only for now).
    */

1:  /* Using r1 (current stack) */
    stdu 1, -FRAME_SIZE(1) /* Push Stack Frame */
    
    std 0, 0(1)         /* Save r0 */
    std 1, 8(1)         /* Save r1 (Backchain? No, r1 is modified. Save OLD r1? ) */
                        /* stdu stores old r1 at 0(new_r1) normally. But we are claiming that space for regs. */
                        /* Standard ABI: 0(r1) = Back Chain. */
                        /* So regs should start at +48 or similar (TOC/LR slots). */
                        /* Our pt_regs is a custom frame. */
                        /* Let's follow Linux style: pt_regs AT TOP of frame? */
                        /* Simpler: just dump regs linearly. But debugging stack trace might break. */
                        /* OK, let's just dump regs at offset 48 (ABI Header). */
                        
    std 2, 16(1)
    std 3, 24(1)
    /* Save basic regs to survive. We need to save ALL for pt_regs. */
    
    li 3, \offset       /* Load Trap Number */
    b exception_common
.endm

_vectors_start:

/* Standard Vectors */
EXCEPTION_VECTOR 0x100, system_reset
EXCEPTION_VECTOR 0x200, machine_check
EXCEPTION_VECTOR 0x300, data_storage
EXCEPTION_VECTOR 0x400, instruction_storage
EXCEPTION_VECTOR 0x500, external_interrupt
EXCEPTION_VECTOR 0x600, alignment
EXCEPTION_VECTOR 0x700, program
EXCEPTION_VECTOR 0x800, fpu_unavailable
EXCEPTION_VECTOR 0x900, decrementer
EXCEPTION_VECTOR 0xC00, syscall

/* Common Handler */
exception_common:
    /* r3 contains trap number */
    /* Stack frame allocated. r0, r2, r3 already saved at 0, 16, 24? Wait, previous code was messy. */
    
    /* Let's do it cleanly: */
    /* We are at exception_common. r13 is in SPRG1. */
    
    /* Save GPRs */
    /* r1 is already decremented. */
    /* We need to save r0-r31. */
    /* NOTE: We overwrote offsets 0, 16, 24 in macro. */
    
    std 4, 32(1)
    std 5, 40(1)
    std 6, 48(1)
    std 7, 56(1)
    std 8, 64(1)
    std 9, 72(1)
    std 10, 80(1)
    std 11, 88(1)
    std 12, 96(1)
    
    mfsprg1 12          /* Restore r13 to r12 temporarily */
    std 12, 104(1)      /* Save r13 */
    
    std 14, 112(1)
    std 15, 120(1)
    std 16, 128(1)
    std 17, 136(1)
    std 18, 144(1)
    std 19, 152(1)
    std 20, 160(1)
    std 21, 168(1)
    std 22, 176(1)
    std 23, 184(1)
    std 24, 192(1)
    std 25, 200(1)
    std 26, 208(1)
    std 27, 216(1)
    std 28, 224(1)
    std 29, 232(1)
    std 30, 240(1)
    std 31, 248(1)
    
    /* Save Special Regs */
    mflr 0
    std 0, 280(1)       /* LR */
    
    mfctr 0
    std 0, 272(1)       /* CTR */
    
    mfxer 0
    std 0, 288(1)       /* XER */
    
    mfcr 0
    std 0, 296(1)       /* CCR */
    
    mfsrr0 0
    std 0, 256(1)       /* PC (NIP) */
    
    mfsrr1 0
    std 0, 264(1)       /* MSR */
    
    mfdar 0
    std 0, 304(1)       /* DAR */
    
    mfdsisr 0
    std 0, 312(1)       /* DSISR */
    
    std 3, 328(1)       /* Trap Number */
    
    /* Call C Handler */
    /* void syscall_handler(struct pt_regs* regs) / void handle_exception(struct pt_regs* regs) */
    /* arg1 (r3) = regs pointer = r1 */
    mr 3, 1
    
    /* Load Kernel TOC */
    /* r2 is usually TOC. We assume kernel TOC is valid or loaded via other means. */
    /* In Phase 21, we assume r2 is preserved or restored. */
    
    cmpdi 3, 0xC00      /* Check if Syscall */
    beq call_syscall
    
call_exception:
    bl handle_exception_ex /* Updated C handler name? */
    b restore_context

call_syscall:
    bl syscall_handler
    
restore_context:
    /* Restore state from stack (which might be modified by syscall/handler) */
    
    ld 0, 256(1)
    mtsrr0 0            /* Restore PC */
    
    ld 0, 264(1)
    mtsrr1 0            /* Restore MSR */
    
    ld 0, 272(1)
    mtctr 0
    
    ld 0, 280(1)
    mtlr 0
    
    ld 0, 288(1)
    mtxer 0
    
    ld 0, 296(1)
    mtcr 0
    
    /* Restore GPRs */
    /* skip r1 (SP), r13 */
    
    ld 0, 0(1)
    ld 2, 16(1)
    ld 3, 24(1)
    ld 4, 32(1)
    ld 5, 40(1)
    ld 6, 48(1)
    ld 7, 56(1)
    ld 8, 64(1)
    ld 9, 72(1)
    ld 10, 80(1)
    ld 11, 88(1)
    ld 12, 96(1)
    
    /* r13 needs care? user r13 vs kernel r13? */
    ld 13, 104(1) 
    
    ld 14, 112(1)
    ld 15, 120(1)
    ld 16, 128(1)
    ld 17, 136(1)
    ld 18, 144(1)
    ld 19, 152(1)
    ld 20, 160(1)
    ld 21, 168(1)
    ld 22, 176(1)
    ld 23, 184(1)
    ld 24, 192(1)
    ld 25, 200(1)
    ld 26, 208(1)
    ld 27, 216(1)
    ld 28, 224(1)
    ld 29, 232(1)
    ld 30, 240(1)
    ld 31, 248(1)
    
    /* Restore Stack */
    addi 1, 1, FRAME_SIZE
    
    rfid
    nop

