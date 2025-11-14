; AstraLisp OS Multiboot 2.0 Header
; Compliant with Multiboot 2.0 specification

section .multiboot_header
align 8

; Multiboot 2.0 magic number
MULTIBOOT2_MAGIC equ 0xE85250D6
; Architecture: i386 (0) - we'll use this for PowerISA compatibility layer
MULTIBOOT_ARCHITECTURE_I386 equ 0
; Header length
MULTIBOOT_HEADER_LENGTH equ (multiboot_header_end - multiboot_header_start)
; Checksum
MULTIBOOT_CHECKSUM equ -(MULTIBOOT2_MAGIC + MULTIBOOT_ARCHITECTURE_I386 + MULTIBOOT_HEADER_LENGTH)

multiboot_header_start:
    ; Magic number
    dd MULTIBOOT2_MAGIC
    ; Architecture
    dd MULTIBOOT_ARCHITECTURE_I386
    ; Header length
    dd MULTIBOOT_HEADER_LENGTH
    ; Checksum
    dd MULTIBOOT_CHECKSUM

    ; Information request tag
    align 8
    dw 1  ; Type: Information request
    dw 0  ; Flags
    dd 20 ; Size
    dd 4  ; Memory map
    dd 5  ; Boot device
    dd 6  ; Boot command line
    dd 8  ; Modules
    dd 9  ; ELF symbols

    ; Address tag
    align 8
    dw 2  ; Type: Address
    dw 0  ; Flags
    dd 24 ; Size
    dd 0x01000000 ; Header address (16MB)
    dd 0x01000000 ; Load address
    dd 0x01000000 ; Load end address
    dd 0x01000000 ; BSS end address

    ; Entry address tag
    align 8
    dw 3  ; Type: Entry address
    dw 0  ; Flags
    dd 12 ; Size
    dd _start ; Entry address

    ; Framebuffer tag (optional)
    align 8
    dw 5  ; Type: Framebuffer
    dw 0  ; Flags
    dd 20 ; Size
    dd 0  ; Width (0 = prefer bootloader)
    dd 0  ; Height (0 = prefer bootloader)
    dd 32 ; Depth (32-bit)

    ; End tag
    align 8
    dw 0  ; Type: End
    dw 0  ; Flags
    dd 8  ; Size

multiboot_header_end:

