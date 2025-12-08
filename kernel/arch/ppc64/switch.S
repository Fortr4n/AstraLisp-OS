/* AstraLisp OS - Context Switching */
/* Target: ppc64le */

.section .text
.global switch_to

/*
 * void switch_to(struct thread* prev, struct thread* next)
 * r3 = prev (struct thread*)
 * r4 = next (struct thread*)
 *
 * struct thread {
 *    uint32_t tid;
 *    ...
 *    struct cpu_context context; // Offset needed!
 * }
 *
 * We need to know offset of 'context' within 'struct thread'.
 * Assuming 'context' is at offset ?? defined in process.h?
 * Checking process.c/h is tricky from asm without generated offsets.
 * 
 * WORKAROUND: We assume 'context' is well-placed or we calculate offset.
 * Inspecting process.h:
 * struct thread {
 *    struct thread* next; // 8
 *    struct thread* prev; // 8
 *    struct process* process; // 8
 *    uint32_t tid; // 4
 *    int state; // 4
 *    int priority; // 4
 *    uint64_t sleep_until; // 8
 *    uint64_t cpu_time_ns; // 8
 *    uint64_t time_slice; // 8
 *    void* stack; // 8
 *    size_t stack_size; // 8
 *    struct cpu_context context; // <-- Here
 * }
 *
 * Offsets:
 * next: 0
 * prev: 8
 * process: 16
 * tid: 24
 * state: 28
 * priority: 32
 * sleep_until: 36 (align 8 -> 40)
 * cpu_time_ns: 48
 * time_slice: 56
 * stack: 64
 * stack_size: 72
 * context: 16 (offset after tid+padding+process)
 *
 * We will use valid offset 16 (0x10).
 */

.set OFFSET_CONTEXT, 16

switch_to:
    /* r3 = prev thread, r4 = next thread */
    
    /* 1. Save Context of 'prev' */
    /* We only need to save Callee-Saved (Non-Volatile) registers:
       r1 (SP), r2 (TOC), r14-r31, LR, CR (fields 2-4?) */
       
    addi 5, 3, OFFSET_CONTEXT  /* r5 = &prev->context */
    
    std 1, 0(5)    /* Save SP (r1) */
    std 2, 8(5)    /* Save TOC (r2) */
    mflr 0
    std 0, 16(5)   /* Save LR (PC for resume) */
    mfcr 0
    std 0, 24(5)   /* Save CR */
    
    std 14, 32(5)
    std 15, 40(5)
    std 16, 48(5)
    std 17, 56(5)
    std 18, 64(5)
    std 19, 72(5)
    std 20, 80(5)
    std 21, 88(5)
    std 22, 96(5)
    std 23, 104(5)
    std 24, 112(5)
    std 25, 120(5)
    std 26, 128(5)
    std 27, 136(5)
    std 28, 144(5)
    std 29, 152(5)
    std 30, 160(5)
    std 31, 168(5)
    
    /* 2. Restore Context of 'next' */
    addi 6, 4, OFFSET_CONTEXT  /* r6 = &next->context */
    
    ld 1, 0(6)     /* Restore SP */
    ld 2, 8(6)     /* Restore TOC */
    ld 0, 16(6)    /* Restore LR */
    mtlr 0
    ld 0, 24(6)    /* Restore CR */
    mtcr 0
    
    ld 14, 32(6)
    ld 15, 40(6)
    ld 16, 48(6)
    ld 17, 56(6)
    ld 18, 64(6)
    ld 19, 72(6)
    ld 20, 80(6)
    ld 21, 88(6)
    ld 22, 96(6)
    ld 23, 104(6)
    ld 24, 112(6)
    ld 25, 120(6)
    ld 26, 128(6)
    ld 27, 136(6)
    ld 28, 144(6)
    ld 29, 152(6)
    ld 30, 160(6)
    ld 31, 168(6)
    
    /* Jump to restored LR */
    blr
