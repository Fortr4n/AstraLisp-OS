/* AstraLisp OS - PowerISA FFI Trampolines */
/* Target: ppc64le */

.section .text

/* uint64_t ffi_read_spr(uint16_t spr) */
.global ffi_read_spr
.type ffi_read_spr, @function
ffi_read_spr:
    /* r3 = SPR index */
    /* Switch on common SPRs */
    cmpwi 3, 287 /* PVR */
    beq .Lread_pvr
    cmpwi 3, 286 /* PIR */
    beq .Lread_pir
    cmpwi 3, 19  /* DAR */
    beq .Lread_dar
    cmpwi 3, 18  /* DSISR */
    beq .Lread_dsisr
    cmpwi 3, 26  /* SRR0 */
    beq .Lread_srr0
    cmpwi 3, 27  /* SRR1 */
    beq .Lread_srr1
    
    /* Unknown SPR */
    li 3, -1
    blr

.Lread_pvr:
    mfspr 3, 287
    blr
.Lread_pir:
    mfspr 3, 286
    blr
.Lread_dar:
    mfspr 3, 19
    blr
.Lread_dsisr:
    mfspr 3, 18
    blr
.Lread_srr0:
    mfspr 3, 26
    blr
.Lread_srr1:
    mfspr 3, 27
    blr

/* void ffi_write_spr(uint16_t spr, uint64_t value) */
.global ffi_write_spr
.type ffi_write_spr, @function
ffi_write_spr:
    /* r3 = SPR index, r4 = value */
    cmpwi 3, 19  /* DAR */
    beq .Lwrite_dar
    cmpwi 3, 18  /* DSISR */
    beq .Lwrite_dsisr
    cmpwi 3, 26  /* SRR0 */
    beq .Lwrite_srr0
    cmpwi 3, 27  /* SRR1 */
    beq .Lwrite_srr1
    
    blr /* Unknown - ignore */

.Lwrite_dar:
    mtspr 19, 4
    blr
.Lwrite_dsisr:
    mtspr 18, 4
    blr
.Lwrite_srr0:
    mtspr 26, 4
    blr
.Lwrite_srr1:
    mtspr 27, 4
    blr

/* Specific optimized accessors that are actually implementable efficiently */

/* uint64_t ffi_read_pvr(void) */
.global ffi_read_pvr
ffi_read_pvr:
    mfspr 3, 287
    blr

/* uint64_t ffi_read_pir(void) */
.global ffi_read_pir
ffi_read_pir:
    mfspr 3, 286
    blr

/* uint64_t ffi_read_tb(void) */
.global ffi_read_tb
ffi_read_tb:
    mftb 3
    blr

/* void ffi_cpu_halt(void) */
.global ffi_cpu_halt
.type ffi_cpu_halt, @function
ffi_cpu_halt:
    /* PowerISA 'doze' or simply hang waiting for interrupt */
    /* MSR[EE] should be enabled for interrupts to wake us */
    /* For now, just a loop is safer than potentially crashing with invalid power states */
1:  nop
    b 1b

/* void ffi_cpu_pause(void) */
.global ffi_cpu_pause
.type ffi_cpu_pause, @function
ffi_cpu_pause:
    /* Yield / Priority low */
    or 27, 27, 27
    blr

/* void ffi_test_altivec(void) */
/* Returns if successful, crashes if not (handled by exception handler) */
.global ffi_test_altivec
.type ffi_test_altivec, @function
ffi_test_altivec:
    /* Simple Altivec instruction */
    /* vor v0, v0, v0 */
    .long 0x10000484
    blr

/* void ffi_test_vsx(void) */
.global ffi_test_vsx
.type ffi_test_vsx, @function
ffi_test_vsx:
    /* XSADDDP (VSX Scalar Add Double-Precision) */
    /* xsadddp 0, 0, 0 */
    .long 0xF0000100
    blr
