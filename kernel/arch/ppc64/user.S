/* AstraLisp OS - User Mode Entry */
/* Target: ppc64le */

.section .text
.global enter_user_mode
.global start_user_thread

/*
 * void enter_user_mode(uintptr_t entry, uintptr_t user_sp)
 * r3 = entry point (user space PC)
 * r4 = user stack pointer
 *
 * Sets up SRR0/SRR1 for user mode and executes rfid.
 * Does not return.
 */
enter_user_mode:
    /* Set SRR0 (Return Address) to entry point */
    mtsrr0 3
    
    /* Build User MSR:
     * SF (64-bit): Bit 63 = 1
     * HV (Hypervisor): Bit 60 = 0 (Not HV for user)
     * PR (Problem State/User): Bit 49 = 1
     * EE (External Interrupts): Bit 48 = 1
     * ME (Machine Check): Bit 52 = 1
     * IR (Instruction Relocation): Bit 58 = 1
     * DR (Data Relocation): Bit 59 = 1
     * LE (Little Endian): Bit 0 = 1
     *
     * MSR value for user mode (approximate):
     * 0x8000 0000 0000 9033 (SF, EE, PR, IR, DR, LE)
     *
     * Let's build it:
     * SF = 0x8000000000000000
     * PR = 0x0000000000004000 (Bit 14 from LSB, which is bit 49 in IBM notation)
     * EE = 0x0000000000008000 (Bit 15 from LSB, which is bit 48)
     * ME = 0x0000000000001000 (Bit 12 from LSB, which is bit 51? Check)
     * IR = 0x0000000000000020 (Bit 5 from LSB, which is bit 58)
     * DR = 0x0000000000000010 (Bit 4 from LSB, which is bit 59)
     * LE = 0x0000000000000001 (Bit 0, which is bit 63 IBM = LE)
     *
     * Actually, PowerPC MSR bit numbering:
     * Bit 0 (IBM) = MSB = SF
     * Bit 63 (IBM) = LSB
     *
     * For practical purposes (little-endian Linux-like):
     * MSR_SF  = 0x8000000000000000ULL  (bit 0)
     * MSR_EE  = 0x0000000000008000ULL  (bit 48 IBM = bit 15 LE)
     * MSR_PR  = 0x0000000000004000ULL  (bit 49 IBM = bit 14 LE)
     * MSR_FP  = 0x0000000000002000ULL
     * MSR_ME  = 0x0000000000001000ULL
     * MSR_IR  = 0x0000000000000020ULL
     * MSR_DR  = 0x0000000000000010ULL
     * MSR_LE  = 0x0000000000000001ULL
     *
     * User MSR = SF | EE | PR | ME | IR | DR | LE
     *          = 0x800000000000D031
     */
    
    /* Load MSR value */
    lis 5, 0x8000       /* High 16 bits of SF */
    sldi 5, 5, 48       /* Shift to get 0x8000000000000000 */
    
    /* Add lower bits */
    ori 6, 0, 0xD031    /* EE | PR | ME | IR | DR | LE */
    or 5, 5, 6
    
    /* Set SRR1 */
    mtsrr1 5
    
    /* Set Stack Pointer */
    mr 1, 4
    
    /* Clear non-volatile registers for clean state */
    li 2, 0     /* TOC - User will set this */
    li 13, 0
    li 14, 0
    li 15, 0
    li 16, 0
    li 17, 0
    li 18, 0
    li 19, 0
    li 20, 0
    li 21, 0
    li 22, 0
    li 23, 0
    li 24, 0
    li 25, 0
    li 26, 0
    li 27, 0
    li 28, 0
    li 29, 0
    li 30, 0
    li 31, 0
    
    /* Jump to User Mode */
    rfid
    nop

/*
 * void start_user_thread(struct thread* thread)
 * Sets up context from thread structure and enters user mode.
 * Used by scheduler for first run of user thread.
 */
start_user_thread:
    /* r3 = thread pointer */
    /* Load entry from thread->context.pc */
    /* Assuming offset 176 for pc in cpu_context (from context-switch.h) */
    /* Let's recalculate: context at offset 16. pc at offset ? in context? */
    /* cpu_context: r1, r2, r13, r14-r31, lr, cr, pc, msr */
    /* 0:r1, 8:r2, 16:r13, 24:r14, 32:r15... 160:r31, 168:lr, 176:cr, 184:pc, 192:msr */
    
    ld 4, 16+0(3)    /* r1 (stack) = context.r1 = thread + 16 + 0 */
    ld 5, 16+184(3)  /* pc = context.pc = thread + 16 + 184 */
    
    /* Set SRR0 = PC */
    mtsrr0 5
    
    /* Set user MSR */
    ld 6, 16+192(3)  /* msr = context.msr */
    mtsrr1 6
    
    /* Set SP */
    mr 1, 4
    
    /* Clear other regs */
    li 2, 0
    li 13, 0
    
    rfid
    nop
