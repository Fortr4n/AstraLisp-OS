/* AstraLisp OS - Secondary CPU Entry Point */
/* Target: PowerISA v3.1 LE (ELFv2 ABI) */

.section .text
.global secondary_cpu_entry
.type secondary_cpu_entry, @function

/* Entry: r3 = pointer to struct per_cpu for this CPU */
secondary_cpu_entry:
    /* Save per_cpu pointer in r13 (TLS/PCR register) */
    mr 13, 3
    
    /* Load stack pointer from per_cpu->stack (offset 16) */
    ld 1, 16(3)
    
    /* Align stack to 16 bytes */
    clrrdi 1, 1, 4
    
    /* Create minimal stack frame */
    stdu 1, -128(1)
    
    /* Load TOC pointer */
    /* Kernel TOC is at symbol .TOC. */
    lis 2, .TOC.@ha
    addi 2, 2, .TOC.@l
    
    /* Mark CPU as ONLINE */
    /* per_cpu->state is at offset 8 (after cpu_id and pir) */
    li 4, 2  /* CPU_STATE_ONLINE = 2 */
    stw 4, 8(13)
    
    /* Synchronize */
    sync
    isync
    
    /* Call secondary_main(per_cpu*) */
    mr 3, 13
    bl secondary_main
    
    /* If secondary_main returns (shouldn't), idle loop */
.secondary_idle:
    or 27, 27, 27  /* yield */
    b .secondary_idle
