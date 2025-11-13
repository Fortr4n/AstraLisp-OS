; AstraLisp OS Multiboot 2.0 Bootloader
; PowerISA implementation with GRUB integration

section .multiboot_header
align 8

multiboot_header_start:
    ; Magic number
    dd 0xE85250D6                    ; Multiboot 2 magic
    dd 0                              ; Architecture: 0 = i386, 4 = MIPS32, 8 = PowerPC
    dd multiboot_header_end - multiboot_header_start  ; Header length
    dd -(0xE85250D6 + 0 + (multiboot_header_end - multiboot_header_start))  ; Checksum

    ; Information request tag
    align 8
    dw 1                              ; Type: information request
    dw 0                              ; Flags
    dd 20                             ; Size
    dd 6                              ; Memory map
    dd 8                              ; Boot device
    dd 2                              ; Command line
    dd 4                              ; Modules
    dd 5                              ; ELF symbols
    dd 0                              ; End tag

    ; Address tag
    align 8
    dw 2                              ; Type: address
    dw 0                              ; Flags
    dd 24                             ; Size
    dd multiboot_header_start         ; Header addr
    dd 0x100000                       ; Load addr
    dd 0                              ; Load end addr
    dd 0                              ; BSS end addr

    ; Entry address tag
    align 8
    dw 3                              ; Type: entry address
    dw 0                              ; Flags
    dd 12                             ; Size
    dd _start                         ; Entry point

    ; Framebuffer tag
    align 8
    dw 5                              ; Type: framebuffer
    dw 0                              ; Flags
    dd 20                             ; Size
    dd 0                              ; Width (0 = prefer)
    dd 0                              ; Height (0 = prefer)
    dd 0                              ; Depth (0 = prefer)

    ; End tag
    align 8
    dw 0                              ; Type: end
    dw 0                              ; Flags
    dd 8                              ; Size

multiboot_header_end:

section .text
global _start
extern kernel_main

_start:
    ; PowerISA initialization
    ; Set up stack pointer
    lis %r1, stack_top@h
    ori %r1, %r1, stack_top@l
    
    ; Clear BSS
    lis %r3, __bss_start@h
    ori %r3, %r3, __bss_start@l
    lis %r4, __bss_end@h
    ori %r4, %r4, __bss_end@l
    subf %r4, %r3, %r4
    li %r5, 0
    
clear_bss_loop:
    cmpdi %r4, 0
    beq clear_bss_done
    stb %r5, 0(%r3)
    addi %r3, %r3, 1
    subi %r4, %r4, 1
    b clear_bss_loop
    
clear_bss_done:
    ; Save multiboot info
    ; r3 = magic, r4 = info structure
    mr %r3, %r3                       ; Magic number
    mr %r4, %r4                       ; Multiboot info structure
    
    ; Call kernel main
    bl kernel_main
    
    ; If kernel returns, halt
halt_loop:
    b halt_loop

section .bss
align 16
stack_bottom:
    resb 16384                        ; 16KB stack
stack_top:
