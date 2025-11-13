; AstraLisp OS Interrupt Handlers
; PowerISA interrupt entry/exit sequences with proper register saving

section .text

extern interrupt_handler
extern exception_handler
extern syscall_handler

; Common interrupt entry point
; Saves all registers and calls C handler
%macro INTERRUPT_ENTRY 1
global interrupt_%1
interrupt_%1:
    ; Save registers on stack
    stdu %r1, -256(%r1)               ; Allocate stack frame
    
    ; Save GPRs
    std %r0, 0(%r1)
    std %r3, 24(%r1)
    std %r4, 32(%r1)
    std %r5, 40(%r1)
    std %r6, 48(%r1)
    std %r7, 56(%r1)
    std %r8, 64(%r1)
    std %r9, 72(%r1)
    std %r10, 80(%r1)
    std %r11, 88(%r1)
    std %r12, 96(%r1)
    
    ; Save special registers
    mflr %r0
    std %r0, 104(%r1)
    mfcr %r0
    std %r0, 112(%r1)
    mfxer %r0
    std %r0, 120(%r1)
    mfctr %r0
    std %r0, 128(%r1)
    
    ; Save FPRs (if needed)
    stfd %f0, 136(%r1)
    stfd %f1, 144(%r1)
    
    ; Call C handler
    li %r3, %1                        ; Interrupt number
    addi %r4, %r1, 0                  ; Stack pointer
    bl interrupt_handler
    
    ; Restore registers
    lfd %f1, 144(%r1)
    lfd %f0, 136(%r1)
    ld %r0, 128(%r1)
    mtctr %r0
    ld %r0, 120(%r1)
    mtxer %r0
    ld %r0, 112(%r1)
    mtcr %r0
    ld %r0, 104(%r1)
    mtlr %r0
    ld %r12, 96(%r1)
    ld %r11, 88(%r1)
    ld %r10, 80(%r1)
    ld %r9, 72(%r1)
    ld %r8, 64(%r1)
    ld %r7, 56(%r1)
    ld %r6, 48(%r1)
    ld %r5, 40(%r1)
    ld %r4, 32(%r1)
    ld %r3, 24(%r1)
    ld %r0, 0(%r1)
    
    ; Restore stack
    addi %r1, %r1, 256
    
    ; Return from interrupt
    rfid
%endmacro

; Exception entry point
%macro EXCEPTION_ENTRY 1
global exception_%1
exception_%1:
    stdu %r1, -256(%r1)
    
    ; Save all registers
    std %r0, 0(%r1)
    std %r3, 24(%r1)
    std %r4, 32(%r1)
    std %r5, 40(%r1)
    std %r6, 48(%r1)
    std %r7, 56(%r1)
    std %r8, 64(%r1)
    std %r9, 72(%r1)
    std %r10, 80(%r1)
    std %r11, 88(%r1)
    std %r12, 96(%r1)
    
    mflr %r0
    std %r0, 104(%r1)
    mfcr %r0
    std %r0, 112(%r1)
    mfxer %r0
    std %r0, 120(%r1)
    mfctr %r0
    std %r0, 128(%r1)
    
    ; Call exception handler
    li %r3, %1
    addi %r4, %r1, 0
    bl exception_handler
    
    ; Restore and return
    ld %r0, 128(%r1)
    mtctr %r0
    ld %r0, 120(%r1)
    mtxer %r0
    ld %r0, 112(%r1)
    mtcr %r0
    ld %r0, 104(%r1)
    mtlr %r0
    ld %r12, 96(%r1)
    ld %r11, 88(%r1)
    ld %r10, 80(%r1)
    ld %r9, 72(%r1)
    ld %r8, 64(%r1)
    ld %r7, 56(%r1)
    ld %r6, 48(%r1)
    ld %r5, 40(%r1)
    ld %r4, 32(%r1)
    ld %r3, 24(%r1)
    ld %r0, 0(%r1)
    
    addi %r1, %r1, 256
    rfid
%endmacro

; System call entry point
global syscall_entry
syscall_entry:
    stdu %r1, -256(%r1)
    
    ; Save user registers
    std %r0, 0(%r1)
    std %r3, 24(%r1)
    std %r4, 32(%r1)
    std %r5, 40(%r1)
    std %r6, 48(%r1)
    std %r7, 56(%r1)
    std %r8, 64(%r1)
    std %r9, 72(%r1)
    std %r10, 80(%r1)
    std %r11, 88(%r1)
    std %r12, 96(%r1)
    
    mflr %r0
    std %r0, 104(%r1)
    mfcr %r0
    std %r0, 112(%r1)
    
    ; Call syscall handler (r3 already has syscall number)
    bl syscall_handler
    
    ; Return value in r3, restore and return
    ld %r0, 112(%r1)
    mtcr %r0
    ld %r0, 104(%r1)
    mtlr %r0
    ld %r12, 96(%r1)
    ld %r11, 88(%r1)
    ld %r10, 80(%r1)
    ld %r9, 72(%r1)
    ld %r8, 64(%r1)
    ld %r7, 56(%r1)
    ld %r6, 48(%r1)
    ld %r5, 40(%r1)
    ld %r4, 32(%r1)
    ld %r0, 0(%r1)
    
    addi %r1, %r1, 256
    sc                              ; Return to user mode

; Generate interrupt handlers for common interrupts
INTERRUPT_ENTRY 0
INTERRUPT_ENTRY 1
INTERRUPT_ENTRY 2
INTERRUPT_ENTRY 3
INTERRUPT_ENTRY 4
INTERRUPT_ENTRY 5
INTERRUPT_ENTRY 6
INTERRUPT_ENTRY 7
INTERRUPT_ENTRY 8
INTERRUPT_ENTRY 9
INTERRUPT_ENTRY 10
INTERRUPT_ENTRY 11
INTERRUPT_ENTRY 12
INTERRUPT_ENTRY 13
INTERRUPT_ENTRY 14
INTERRUPT_ENTRY 15

; Generate exception handlers
EXCEPTION_ENTRY 0   ; System reset
EXCEPTION_ENTRY 1   ; Machine check
EXCEPTION_ENTRY 2   ; Data storage
EXCEPTION_ENTRY 3   ; Instruction storage
EXCEPTION_ENTRY 4   ; External interrupt
EXCEPTION_ENTRY 5   ; Alignment
EXCEPTION_ENTRY 6   ; Program
EXCEPTION_ENTRY 7   ; Floating-point unavailable
EXCEPTION_ENTRY 8   ; Decrementer
EXCEPTION_ENTRY 9   ; System call
EXCEPTION_ENTRY 10  ; Auxiliary processor unavailable
EXCEPTION_ENTRY 11  ; Decrementer
EXCEPTION_ENTRY 12  ; Fixed interval timer
EXCEPTION_ENTRY 13  ; Watchdog timer
EXCEPTION_ENTRY 14  ; Data TLB miss
EXCEPTION_ENTRY 15  ; Instruction TLB miss
