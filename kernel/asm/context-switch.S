/* AstraLisp OS - Context Switch Assembly */
/* Target: PowerISA v3.1 LE (ELFv2 ABI) */

.section .text
.global context_switch
.type context_switch, @function

/* void context_switch(struct cpu_context* prev, struct cpu_context* next) */
/* r3 = prev, r4 = next */

context_switch:
    /* 1. Save Current Context into 'prev' (r3) */
    /* struct cpu_context layout:
       0: r1
       8: r2
       16: r13
       ...
       160: r31
       168: lr
       176: cr
       184: pc (unused for save, implicitly LR)
       192: msr
    */
    
    /* Save r1 (Stack) */
    std 1, 0(3)
    
    /* Save r2 (TOC) */
    std 2, 8(3)
    
    /* Save Non-Volatile GPRs (r13-r31) */
    std 13, 16(3)
    std 14, 24(3)
    std 15, 32(3)
    std 16, 40(3)
    std 17, 48(3)
    std 18, 56(3)
    std 19, 64(3)
    std 20, 72(3)
    std 21, 80(3)
    std 22, 88(3)
    std 23, 96(3)
    std 24, 104(3)
    std 25, 112(3)
    std 26, 120(3)
    std 27, 128(3)
    std 28, 136(3)
    std 29, 144(3)
    std 30, 152(3)
    std 31, 160(3)
    
    /* Save LR/CR */
    mflr 0
    std 0, 168(3)
    
    mfcr 0
    std 0, 176(3)
    
    /* Save MSR */
    mfmsr 0
    std 0, 192(3)
    
    /* 2. Restore New Context from 'next' (r4) */
    
    /* Restore MSR? Usually we run in same privilege, 
       but if task switch involves privilege change, we might need 'rfid'.
       For kernel threads, we just skip MSR restore or assume it's same.
    */
    
    /* Restore Non-Volatile GPRs */
    ld 13, 16(4)
    ld 14, 24(4)
    ld 15, 32(4)
    ld 16, 40(4)
    ld 17, 48(4)
    ld 18, 56(4)
    ld 19, 64(4)
    ld 20, 72(4)
    ld 21, 80(4)
    ld 22, 88(4)
    ld 23, 96(4)
    ld 24, 104(4)
    ld 25, 112(4)
    ld 26, 120(4)
    ld 27, 128(4)
    ld 28, 136(4)
    ld 29, 144(4)
    ld 30, 152(4)
    ld 31, 160(4)
    
    /* Restore CR */
    ld 0, 176(4)
    mtcr 0
    
    /* Restore LR (Return Address) */
    /* Note: For new threads, this points to their entry point wrapper */
    ld 0, 168(4)
    mtlr 0
    
    /* Restore TOC (r2) */
    ld 2, 8(4)
    
    /* Restore Stack (r1) */
    ld 1, 0(4)
    
    /* 3. Return to new context */
    blr
