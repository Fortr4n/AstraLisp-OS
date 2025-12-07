/* AstraLisp OS - OPAL Call Wrapper */
/* Target: ppc64le */

.section .text
.global opal_entry_point
.section .data
.align 3
opal_entry_point:
    .quad 0
.global opal_base
opal_base:
    .quad 0

.section .text

/* int64_t opal_call(int64_t token, ...) */
.global opal_call
.type opal_call, @function
opal_call:
    /* r3 = token, r4-r10 = args */
    
    /* Load OPAL entry */
    ld 12, opal_entry_point@got(2)
    ld 12, 0(12)
    
    cmpdi 12, 0
    beq 1f
    
    /* Save TOC */
    std 2, 24(1)
    
    /* Load OPAL base into r2 (TOC) */
    ld 2, opal_base@got(2)
    ld 2, 0(2)
    
    /* Call OPAL */
    mtctr 12
    bctrl
    
    /* Restore TOC */
    ld 2, 24(1)
    blr
    
1:
    /* Error: OPAL not initialized */
    li 3, -1 /* OPAL_PARAMETER */
    blr
