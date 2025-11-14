; AstraLisp OS Context Switch Implementation
; PowerISA register save/restore for all 32 GPRs, 32 FPRs, 32 VRs

section .text

; Context structure layout:
; Offset 0-255:  32 GPRs (r0-r31), 8 bytes each
; Offset 256-511: 32 FPRs (f0-f31), 8 bytes each
; Offset 512-767: 32 VRs (v0-v31), 16 bytes each
; Offset 768:     LR (Link Register), 8 bytes
; Offset 776:     CR (Condition Register), 8 bytes
; Offset 784:     XER, 8 bytes
; Offset 792:     CTR, 8 bytes
; Offset 800:     FPSCR, 8 bytes
; Offset 808:     VSCR, 8 bytes

%define CONTEXT_SIZE 816

global context_save
global context_restore
global context_switch

; Save current context to structure at r3
context_save:
    ; Save GPRs (r0-r31)
    std %r0, 0(%r3)
    std %r1, 8(%r3)
    std %r2, 16(%r3)
    std %r4, 32(%r3)
    std %r5, 40(%r3)
    std %r6, 48(%r3)
    std %r7, 56(%r3)
    std %r8, 64(%r3)
    std %r9, 72(%r3)
    std %r10, 80(%r3)
    std %r11, 88(%r3)
    std %r12, 96(%r3)
    std %r13, 104(%r3)
    std %r14, 112(%r3)
    std %r15, 120(%r3)
    std %r16, 128(%r3)
    std %r17, 136(%r3)
    std %r18, 144(%r3)
    std %r19, 152(%r3)
    std %r20, 160(%r3)
    std %r21, 168(%r3)
    std %r22, 176(%r3)
    std %r23, 184(%r3)
    std %r24, 192(%r3)
    std %r25, 200(%r3)
    std %r26, 208(%r3)
    std %r27, 216(%r3)
    std %r28, 224(%r3)
    std %r29, 232(%r3)
    std %r30, 240(%r3)
    std %r31, 248(%r3)
    
    ; Save FPRs (f0-f31)
    stfd %f0, 256(%r3)
    stfd %f1, 264(%r3)
    stfd %f2, 272(%r3)
    stfd %f3, 280(%r3)
    stfd %f4, 288(%r3)
    stfd %f5, 296(%r3)
    stfd %f6, 304(%r3)
    stfd %f7, 312(%r3)
    stfd %f8, 320(%r3)
    stfd %f9, 328(%r3)
    stfd %f10, 336(%r3)
    stfd %f11, 344(%r3)
    stfd %f12, 352(%r3)
    stfd %f13, 360(%r3)
    stfd %f14, 368(%r3)
    stfd %f15, 376(%r3)
    stfd %f16, 384(%r3)
    stfd %f17, 392(%r3)
    stfd %f18, 400(%r3)
    stfd %f19, 408(%r3)
    stfd %f20, 416(%r3)
    stfd %f21, 424(%r3)
    stfd %f22, 432(%r3)
    stfd %f23, 440(%r3)
    stfd %f24, 448(%r3)
    stfd %f25, 456(%r3)
    stfd %f26, 464(%r3)
    stfd %f27, 472(%r3)
    stfd %f28, 480(%r3)
    stfd %f29, 488(%r3)
    stfd %f30, 496(%r3)
    stfd %f31, 504(%r3)
    
    ; Save VRs (v0-v31) - 16 bytes each
    stvx %v0, %r0, %r3
    addi %r4, %r3, 512
    stvx %v1, %r0, %r4
    addi %r4, %r4, 16
    stvx %v2, %r0, %r4
    addi %r4, %r4, 16
    stvx %v3, %r0, %r4
    addi %r4, %r4, 16
    stvx %v4, %r0, %r4
    addi %r4, %r4, 16
    stvx %v5, %r0, %r4
    addi %r4, %r4, 16
    stvx %v6, %r0, %r4
    addi %r4, %r4, 16
    stvx %v7, %r0, %r4
    addi %r4, %r4, 16
    stvx %v8, %r0, %r4
    addi %r4, %r4, 16
    stvx %v9, %r0, %r4
    addi %r4, %r4, 16
    stvx %v10, %r0, %r4
    addi %r4, %r4, 16
    stvx %v11, %r0, %r4
    addi %r4, %r4, 16
    stvx %v12, %r0, %r4
    addi %r4, %r4, 16
    stvx %v13, %r0, %r4
    addi %r4, %r4, 16
    stvx %v14, %r0, %r4
    addi %r4, %r4, 16
    stvx %v15, %r0, %r4
    addi %r4, %r4, 16
    stvx %v16, %r0, %r4
    addi %r4, %r4, 16
    stvx %v17, %r0, %r4
    addi %r4, %r4, 16
    stvx %v18, %r0, %r4
    addi %r4, %r4, 16
    stvx %v19, %r0, %r4
    addi %r4, %r4, 16
    stvx %v20, %r0, %r4
    addi %r4, %r4, 16
    stvx %v21, %r0, %r4
    addi %r4, %r4, 16
    stvx %v22, %r0, %r4
    addi %r4, %r4, 16
    stvx %v23, %r0, %r4
    addi %r4, %r4, 16
    stvx %v24, %r0, %r4
    addi %r4, %r4, 16
    stvx %v25, %r0, %r4
    addi %r4, %r4, 16
    stvx %v26, %r0, %r4
    addi %r4, %r4, 16
    stvx %v27, %r0, %r4
    addi %r4, %r4, 16
    stvx %v28, %r0, %r4
    addi %r4, %r4, 16
    stvx %v29, %r0, %r4
    addi %r4, %r4, 16
    stvx %v30, %r0, %r4
    addi %r4, %r4, 16
    stvx %v31, %r0, %r4
    
    ; Save special registers
    mflr %r4
    std %r4, 768(%r3)                 ; LR
    mfcr %r4
    std %r4, 776(%r3)                 ; CR
    mfxer %r4
    std %r4, 784(%r3)                 ; XER
    mfctr %r4
    std %r4, 792(%r3)                 ; CTR
    mffs %f0
    stfd %f0, 800(%r3)                ; FPSCR
    mfvscr %v0
    stvx %v0, %r0, %r3
    addi %r4, %r3, 808
    stvx %v0, %r0, %r4                ; VSCR
    
    blr

; Restore context from structure at r3
context_restore:
    ; Restore GPRs (r0-r31)
    ld %r0, 0(%r3)
    ld %r1, 8(%r3)
    ld %r2, 16(%r3)
    ld %r4, 32(%r3)
    ld %r5, 40(%r3)
    ld %r6, 48(%r3)
    ld %r7, 56(%r3)
    ld %r8, 64(%r3)
    ld %r9, 72(%r3)
    ld %r10, 80(%r3)
    ld %r11, 88(%r3)
    ld %r12, 96(%r3)
    ld %r13, 104(%r3)
    ld %r14, 112(%r3)
    ld %r15, 120(%r3)
    ld %r16, 128(%r3)
    ld %r17, 136(%r3)
    ld %r18, 144(%r3)
    ld %r19, 152(%r3)
    ld %r20, 160(%r3)
    ld %r21, 168(%r3)
    ld %r22, 176(%r3)
    ld %r23, 184(%r3)
    ld %r24, 192(%r3)
    ld %r25, 200(%r3)
    ld %r26, 208(%r3)
    ld %r27, 216(%r3)
    ld %r28, 224(%r3)
    ld %r29, 232(%r3)
    ld %r30, 240(%r3)
    ld %r31, 248(%r3)
    
    ; Restore FPRs (f0-f31)
    lfd %f0, 256(%r3)
    lfd %f1, 264(%r3)
    lfd %f2, 272(%r3)
    lfd %f3, 280(%r3)
    lfd %f4, 288(%r3)
    lfd %f5, 296(%r3)
    lfd %f6, 304(%r3)
    lfd %f7, 312(%r3)
    lfd %f8, 320(%r3)
    lfd %f9, 328(%r3)
    lfd %f10, 336(%r3)
    lfd %f11, 344(%r3)
    lfd %f12, 352(%r3)
    lfd %f13, 360(%r3)
    lfd %f14, 368(%r3)
    lfd %f15, 376(%r3)
    lfd %f16, 384(%r3)
    lfd %f17, 392(%r3)
    lfd %f18, 400(%r3)
    lfd %f19, 408(%r3)
    lfd %f20, 416(%r3)
    lfd %f21, 424(%r3)
    lfd %f22, 432(%r3)
    lfd %f23, 440(%r3)
    lfd %f24, 448(%r3)
    lfd %f25, 456(%r3)
    lfd %f26, 464(%r3)
    lfd %f27, 472(%r3)
    lfd %f28, 480(%r3)
    lfd %f29, 488(%r3)
    lfd %f30, 496(%r3)
    lfd %f31, 504(%r3)
    
    ; Restore VRs (v0-v31)
    addi %r4, %r3, 512
    lvx %v0, %r0, %r4
    addi %r4, %r4, 16
    lvx %v1, %r0, %r4
    addi %r4, %r4, 16
    lvx %v2, %r0, %r4
    addi %r4, %r4, 16
    lvx %v3, %r0, %r4
    addi %r4, %r4, 16
    lvx %v4, %r0, %r4
    addi %r4, %r4, 16
    lvx %v5, %r0, %r4
    addi %r4, %r4, 16
    lvx %v6, %r0, %r4
    addi %r4, %r4, 16
    lvx %v7, %r0, %r4
    addi %r4, %r4, 16
    lvx %v8, %r0, %r4
    addi %r4, %r4, 16
    lvx %v9, %r0, %r4
    addi %r4, %r4, 16
    lvx %v10, %r0, %r4
    addi %r4, %r4, 16
    lvx %v11, %r0, %r4
    addi %r4, %r4, 16
    lvx %v12, %r0, %r4
    addi %r4, %r4, 16
    lvx %v13, %r0, %r4
    addi %r4, %r4, 16
    lvx %v14, %r0, %r4
    addi %r4, %r4, 16
    lvx %v15, %r0, %r4
    addi %r4, %r4, 16
    lvx %v16, %r0, %r4
    addi %r4, %r4, 16
    lvx %v17, %r0, %r4
    addi %r4, %r4, 16
    lvx %v18, %r0, %r4
    addi %r4, %r4, 16
    lvx %v19, %r0, %r4
    addi %r4, %r4, 16
    lvx %v20, %r0, %r4
    addi %r4, %r4, 16
    lvx %v21, %r0, %r4
    addi %r4, %r4, 16
    lvx %v22, %r0, %r4
    addi %r4, %r4, 16
    lvx %v23, %r0, %r4
    addi %r4, %r4, 16
    lvx %v24, %r0, %r4
    addi %r4, %r4, 16
    lvx %v25, %r0, %r4
    addi %r4, %r4, 16
    lvx %v26, %r0, %r4
    addi %r4, %r4, 16
    lvx %v27, %r0, %r4
    addi %r4, %r4, 16
    lvx %v28, %r0, %r4
    addi %r4, %r4, 16
    lvx %v29, %r0, %r4
    addi %r4, %r4, 16
    lvx %v30, %r0, %r4
    addi %r4, %r4, 16
    lvx %v31, %r0, %r4
    
    ; Restore special registers
    ld %r4, 768(%r3)                  ; LR
    mtlr %r4
    ld %r4, 776(%r3)                  ; CR
    mtcr %r4
    ld %r4, 784(%r3)                  ; XER
    mtxer %r4
    ld %r4, 792(%r3)                  ; CTR
    mtctr %r4
    lfd %f0, 800(%r3)                 ; FPSCR
    mtfsf 0xFF, %f0
    addi %r4, %r3, 808
    lvx %v0, %r0, %r4                 ; VSCR
    mtvscr %v0
    
    blr

; Switch context: r3 = from, r4 = to
context_switch:
    ; Save current context
    mr %r5, %r3
    bl context_save
    
    ; Restore new context
    mr %r3, %r4
    b context_restore
