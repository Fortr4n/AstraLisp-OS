/* AstraLisp OS - CPU Setup & Verification */
/* Target: POWER10 (ppc64le) */

.section .text
.global verify_power10
.type verify_power10, @function

verify_power10:
    /* Read Processor Version Register (PVR) */
    mfspr 3, 287
    
    /* Check for POWER10 (PVR upper 16 bits = 0x0080) */
    /* PVR format: Version (16) | Revision (16) */
    
    srdi 3, 3, 16      /* Shift right 16 to get version */
    
    /* Load 0x0080 into r4 */
    li 4, 0x0080
    
    cmpd 3, 4
    beq 1f             /* Branch if equal (supported) */
    
    /* Verification Failed */
    /* Load error code or setup state for debug if possible */
    /* Since we are extremely early, we just hang loop. */
    /* In a real scenario, we might try to output to OPAL if initialized, but we might be before OPAL init. */
_cpu_mismatch:
    /* Fail loop */
    b _cpu_mismatch
    
1:
    /* Check successful */
    blr
