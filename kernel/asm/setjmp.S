/* AstraLisp OS setjmp/longjmp for PowerPC 64 (Little Endian) */

.global setjmp
.global longjmp

/* int setjmp(jmp_buf env) 
   r3 = env pointer
*/
setjmp:
    /* Save non-volatile registers */
    std 1, 0(3)   /* r1 (SP) */
    std 2, 8(3)   /* r2 (TOC) */
    std 13, 16(3) /* r13 (Thread Pointer) */
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
    
    /* Save LR */
    mflr 0
    std 0, 168(3)
    
    /* Save CR */
    mfcr 0
    std 0, 176(3)
    
    /* Return 0 */
    li 3, 0
    blr

/* void longjmp(jmp_buf env, int val) 
   r3 = env pointer
   r4 = val
*/
longjmp:
    /* Restore non-volatile registers */
    ld 1, 0(3)
    ld 2, 8(3)
    ld 13, 16(3)
    ld 14, 24(3)
    ld 15, 32(3)
    ld 16, 40(3)
    ld 17, 48(3)
    ld 18, 56(3)
    ld 19, 64(3)
    ld 20, 72(3)
    ld 21, 80(3)
    ld 22, 88(3)
    ld 23, 96(3)
    ld 24, 104(3)
    ld 25, 112(3)
    ld 26, 120(3)
    ld 27, 128(3)
    ld 28, 136(3)
    ld 29, 144(3)
    ld 30, 152(3)
    ld 31, 160(3)
    
    /* Restore LR */
    ld 0, 168(3)
    mtlr 0
    
    /* Restore CR */
    ld 0, 176(3)
    mtcr 0
    
    /* Return val */
    mr 3, 4
    
    /* If val is 0, return 1 */
    cmpdi 3, 0
    bne 1f
    li 3, 1
1:
    blr
