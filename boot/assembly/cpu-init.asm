; AstraLisp OS CPU Initialization
; Complete PowerISA CPU setup

section .text
global cpu_init_complete

; Complete CPU initialization
cpu_init_complete:
    ; Save link register
    mflr r0
    stwu r1, -0x100(r1)  ; Allocate stack frame
    stw r0, 0x104(r1)

    ; Disable all interrupts
    mfmsr r3
    rlwinm r3, r3, 0, 17, 15  ; Clear MSR[EE]
    rlwinm r3, r3, 0, 18, 16  ; Clear MSR[CE]
    mtmsr r3
    isync

    ; Set exception vector prefix
    ; Exception vectors at 0x1000000
    lis r3, 0x0100
    mtspr 0x1F, r3  ; IVPR
    isync

    ; Initialize cache
    bl init_cache

    ; Initialize time base
    bl init_timebase

    ; Initialize performance counters
    bl init_perf_counters

    ; Set up SMT (Simultaneous Multi-Threading) if available
    bl init_smt

    ; Restore and return
    lwz r0, 0x104(r1)
    addi r1, r1, 0x100
    mtlr r0
    blr

; Initialize cache
init_cache:
    ; Get cache parameters
    mfspr r3, 0x3F0  ; HID0

    ; Enable instruction cache
    oris r3, r3, 0x8000  ; ICE
    mtspr 0x3F0, r3
    isync

    ; Enable data cache
    oris r3, r3, 0x4000  ; DCE
    mtspr 0x3F0, r3
    isync

    ; Invalidate caches
    ; Instruction cache
    li r4, 0
    li r5, 0x8000  ; 32KB typical
icache_invalidate:
    dcbst 0, r4
    sync
    icbi 0, r4
    addi r4, r4, 0x20  ; Cache line size
    cmpw r4, r5
    blt icache_invalidate

    ; Data cache
    li r4, 0
dcache_invalidate:
    dcbf 0, r4
    sync
    addi r4, r4, 0x20
    cmpw r4, r5
    blt dcache_invalidate

    blr

; Initialize time base
init_timebase:
    ; Clear time base
    li r3, 0
    mtspr 0x11C, r3  ; TBL
    mtspr 0x11D, r3  ; TBU
    blr

; Initialize performance counters
init_perf_counters:
    ; Clear performance counters
    li r3, 0
    mtspr 0x1F8, r3  ; PMC1
    mtspr 0x1F9, r3  ; PMC2
    mtspr 0x1FA, r3  ; PMC3
    mtspr 0x1FB, r3  ; PMC4
    mtspr 0x1FC, r3  ; PMC5
    mtspr 0x1FD, r3  ; PMC6
    blr

; Initialize SMT
init_smt:
    ; Check if SMT is available
    mfspr r3, 0x1F3  ; PVR (Processor Version Register)
    rlwinm r4, r3, 16, 0xFFFF  ; Extract major version
    cmpwi r4, 0x004E  ; POWER8 or later
    blt init_smt_done

    ; Enable SMT (if not already enabled)
    ; This is typically done by firmware, but we verify
    mfspr r3, 0x3F0  ; HID0
    rlwinm r4, r3, 0, 0, 0  ; Check SMT enable bit
    cmpwi r4, 0
    bne init_smt_done

    ; SMT is disabled - we'll leave it disabled for now
    ; (SMT can be enabled later by kernel if needed)

init_smt_done:
    blr

