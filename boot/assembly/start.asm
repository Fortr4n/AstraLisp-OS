; AstraLisp OS Boot Entry Point
; PowerISA assembly entry point for kernel

section .text
global _start
extern kernel_main
extern kernel_stack_top

; Boot entry point
_start:
    ; Save multiboot information pointer (passed in r3 on PowerISA)
    ; For PowerISA, bootloader passes info in r3
    mr r31, r3  ; Save multiboot info pointer in r31 (non-volatile)

    ; Initialize stack pointer
    lis r1, kernel_stack_top@h
    ori r1, r1, kernel_stack_top@l
    addi r1, r1, -0x100  ; Stack grows downward, leave some space

    ; Clear BSS section
    lis r4, _bss_start@h
    ori r4, r4, _bss_start@l
    lis r5, _bss_end@h
    ori r5, r5, _bss_end@l
    li r6, 0

clear_bss_loop:
    cmpw r4, r5
    bge clear_bss_done
    stb r6, 0(r4)
    addi r4, r4, 1
    b clear_bss_loop

clear_bss_done:
    ; Initialize CPU
    bl cpu_init

    ; Initialize MMU
    bl mmu_init

    ; Call kernel main (pass multiboot info in r3)
    mr r3, r31
    bl kernel_main

    ; If kernel_main returns, halt
halt_loop:
    b halt_loop

; CPU initialization
cpu_init:
    ; Disable interrupts
    mfmsr r0
    rlwinm r0, r0, 0, 17, 15  ; Clear MSR[EE] (external interrupt enable)
    mtmsr r0
    isync

    ; Set up exception vectors
    ; Exception vector base address (0x1000000 for kernel)
    lis r0, 0x0100
    mtspr 0x1F, r0  ; Set IVPR (Interrupt Vector Prefix Register)
    isync

    ; Initialize cache
    ; Enable instruction cache
    mfspr r0, 0x3F0  ; HID0
    oris r0, r0, 0x8000  ; Set ICE (Instruction Cache Enable)
    mtspr 0x3F0, r0
    isync

    ; Enable data cache
    mfspr r0, 0x3F0  ; HID0
    oris r0, r0, 0x4000  ; Set DCE (Data Cache Enable)
    mtspr 0x3F0, r0
    isync

    ; Set up time base
    li r0, 0
    mtspr 0x11C, r0  ; Clear TBL (Time Base Lower)
    mtspr 0x11D, r0  ; Clear TBU (Time Base Upper)

    blr

; MMU initialization
mmu_init:
    ; For PowerISA, we need to set up the MMU (Memory Management Unit)
    ; This is a simplified version - full implementation will be in kernel

    ; Set up page tables
    ; For now, identity map first 256MB
    lis r3, 0x0100  ; Page table base at 0x1000000
    mtspr 0x3D, r3   ; Set SDR1 (Storage Description Register 1)
    isync

    ; Initialize page table entries
    ; Each entry maps 4KB pages
    li r4, 0x01000000  ; Start address
    li r5, 0x10000000  ; End address (256MB)
    li r6, 0x000000C0  ; Page attributes: Valid, Write, Execute

init_page_table:
    cmpw r4, r5
    bge mmu_init_done

    ; Create page table entry
    ; For simplicity, using direct store
    ; Full implementation will use proper page table structure
    or r7, r4, r6
    stw r7, 0(r3)
    addi r3, r3, 8  ; Next entry
    addi r4, r4, 0x1000  ; Next 4KB page
    b init_page_table

mmu_init_done:
    ; Enable MMU
    mfmsr r0
    oris r0, r0, 0x0010  ; Set MSR[IR] (Instruction Relocation)
    oris r0, r0, 0x0008  ; Set MSR[DR] (Data Relocation)
    mtmsr r0
    isync

    blr

; Exception handlers (stubs for now)
.global exception_handler
exception_handler:
    ; Save all registers
    ; Full implementation will be in kernel
    b halt_loop

.global interrupt_handler
interrupt_handler:
    ; Save all registers
    ; Full implementation will be in kernel
    b halt_loop

