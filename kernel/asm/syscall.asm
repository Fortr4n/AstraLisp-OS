; AstraLisp OS System Call Interface
; PowerISA syscall entry point with privilege checking

section .text

extern syscall_table
extern syscall_count

; System call entry point
; Called via 'sc' instruction
; r3 = syscall number, r4-r10 = arguments
global syscall_entry
syscall_entry:
    ; Check if in user mode (MSR[PR] = 1) - syscalls only allowed from user space
    mfmsr %r11
    andi. %r11, %r11, 0x2000          ; Check PR bit (0x2000 = bit 14, problem state)
    beq syscall_invalid               ; If kernel mode (PR=0), invalid - syscalls are for user→kernel transition
    
    ; Validate syscall number
    cmpdi %r3, 0
    blt syscall_invalid
    lis %r11, syscall_count@h
    ori %r11, %r11, syscall_count@l
    ld %r11, 0(%r11)
    cmp %r3, %r11
    bge syscall_invalid
    
    ; Get syscall handler address
    lis %r11, syscall_table@h
    ori %r11, %r11, syscall_table@l
    sldi %r12, %r3, 3                 ; Multiply by 8 (pointer size)
    add %r11, %r11, %r12
    ld %r11, 0(%r11)                  ; Load handler address
    
    ; Check if handler is NULL
    cmpdi %r11, 0
    beq syscall_invalid
    
    ; Save return address
    mflr %r0
    std %r0, 16(%r1)
    
    ; Call handler (r3-r10 already set up)
    mtctr %r11
    bctrl
    
    ; Restore return address
    ld %r0, 16(%r1)
    mtlr %r0
    
    ; Return (r3 contains return value)
    sc
    
syscall_invalid:
    li %r3, -1                         ; Invalid syscall
    sc

; User-space syscall stub
; This is linked into user programs
global syscall
syscall:
    ; r3 = syscall number, r4-r10 = arguments
    sc
    blr
