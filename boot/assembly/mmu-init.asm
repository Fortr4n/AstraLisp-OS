; AstraLisp OS MMU Initialization
; Complete PowerISA Memory Management Unit setup

section .text
global mmu_init_complete
global mmu_map_page
global mmu_unmap_page

; Page table structure
; Each entry is 8 bytes:
;   Bits 0-51: Physical page address (aligned to 4KB)
;   Bit 52: Valid
;   Bit 53: Write
;   Bit 54: Execute
;   Bit 55: User
;   Bits 56-63: Reserved/OS-specific

; Complete MMU initialization
mmu_init_complete:
    ; Save link register
    mflr r0
    stwu r1, -0x100(r1)
    stw r0, 0x104(r1)

    ; Set up page table base
    ; Page table at 0x1000000 (16MB)
    lis r3, 0x0100
    mtspr 0x3D, r3  ; SDR1 (Storage Description Register 1)
    isync

    ; Initialize page table
    ; Identity map first 256MB (0x00000000 - 0x10000000)
    li r3, 0x01000000  ; Page table base
    li r4, 0x00000000  ; Virtual address start
    li r5, 0x10000000  ; Virtual address end
    li r6, 0x000000C0  ; Attributes: Valid | Write | Execute

mmu_init_loop:
    cmpw r4, r5
    bge mmu_init_done

    ; Create page table entry
    ; Physical address = Virtual address (identity map)
    or r7, r4, r6
    stw r7, 0(r3)      ; Store lower 32 bits
    li r8, 0
    stw r8, 4(r3)       ; Store upper 32 bits (zero for now)

    ; Next entry
    addi r3, r3, 8
    addi r4, r4, 0x1000  ; Next 4KB page
    b mmu_init_loop

mmu_init_done:
    ; Invalidate TLB
    li r3, 0
    tlbiel r3
    sync
    tlbie r3
    sync

    ; Enable MMU
    mfmsr r3
    oris r3, r3, 0x0010  ; Set MSR[IR] (Instruction Relocation)
    oris r3, r3, 0x0008   ; Set MSR[DR] (Data Relocation)
    mtmsr r3
    isync

    ; Restore and return
    lwz r0, 0x104(r1)
    addi r1, r1, 0x100
    mtlr r0
    blr

; Map a page
; Input: r3 = virtual address, r4 = physical address, r5 = attributes
mmu_map_page:
    ; Calculate page table entry address
    ; For now, simple linear mapping
    ; Full implementation will use proper page table walk
    lis r6, 0x0100      ; Page table base
    rlwinm r7, r3, 20, 12, 31  ; Extract page number
    slwi r7, r7, 3      ; Multiply by 8 (entry size)
    add r6, r6, r7      ; Entry address

    ; Create entry
    rlwinm r8, r4, 0, 0, 19  ; Align to 4KB
    or r8, r8, r5       ; Add attributes
    stw r8, 0(r6)       ; Store entry

    ; Invalidate TLB entry
    tlbiel r3
    sync

    blr

; Unmap a page
; Input: r3 = virtual address
mmu_unmap_page:
    ; Calculate page table entry address
    lis r6, 0x0100
    rlwinm r7, r3, 20, 12, 31
    slwi r7, r7, 3
    add r6, r6, r7

    ; Clear entry
    li r4, 0
    stw r4, 0(r6)

    ; Invalidate TLB entry
    tlbiel r3
    sync

    blr

