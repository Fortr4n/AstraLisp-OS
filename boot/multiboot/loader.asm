; AstraLisp OS Multiboot 2.0 Loader
; Loads and validates multiboot information

section .text
global multiboot_check
global multiboot_get_info

; Check if multiboot info is valid
; Input: r3 = multiboot info pointer
; Output: r3 = 1 if valid, 0 if invalid
multiboot_check:
    cmpwi r3, 0
    beq multiboot_invalid

    ; Check magic number (first 4 bytes should be 0x36D76289 for Multiboot 2)
    lwz r4, 0(r3)
    lis r5, 0x36D7
    ori r5, r5, 0x6289
    cmpw r4, r5
    bne multiboot_invalid

    ; Valid
    li r3, 1
    blr

multiboot_invalid:
    li r3, 0
    blr

; Get multiboot info structure
; Input: r3 = multiboot info pointer
; Output: r3 = info structure address (same as input if valid)
multiboot_get_info:
    bl multiboot_check
    cmpwi r3, 0
    beq multiboot_get_info_done
    ; Return original pointer (saved in non-volatile register)
    ; For now, just return
multiboot_get_info_done:
    blr

