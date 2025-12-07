;; AstraLisp OS Runtime - PowerISA Code Generation
;; Production PowerISA code generator with full instruction encoding

(defpackage :astralisp-codegen
  (:use :cl)
  (:export :generate-powerisa
           :emit-instruction
           :allocate-registers
           :encode-instruction))

(in-package :astralisp-codegen)

;; PowerISA register mappings
(defconstant +r0+ 0)   ; Zero/temp
(defconstant +r1+ 1)   ; Stack pointer
(defconstant +r2+ 2)   ; TOC pointer
(defconstant +r3+ 3)   ; First arg/return
(defconstant +r4+ 4)   ; Second arg
(defconstant +r5+ 5)   ; Third arg
(defconstant +r6+ 6)   ; Fourth arg
(defconstant +r7+ 7)   ; Fifth arg
(defconstant +r8+ 8)   ; Sixth arg
(defconstant +lr+ 32)  ; Link register (pseudo)

;; PowerISA opcode constants
(defconstant +opcode-addi+ #x38000000)  ; addi rt,ra,si
(defconstant +opcode-add+ #x7C000214)   ; add rt,ra,rb
(defconstant +opcode-subf+ #x7C000050)  ; subf rt,ra,rb
(defconstant +opcode-mulli+ #x1C000000) ; mulli rt,ra,si
(defconstant +opcode-mullw+ #x7C0001D6) ; mullw rt,ra,rb
(defconstant +opcode-divw+ #x7C0003D6)  ; divw rt,ra,rb
(defconstant +opcode-ld+ #xE8000000)    ; ld rt,ds(ra)
(defconstant +opcode-std+ #xF8000000)  ; std rs,ds(ra)
(defconstant +opcode-cmpd+ #x7C000000)  ; cmpd bf,ra,rb
(defconstant +opcode-beq+ #x41820000)   ; beq target
(defconstant +opcode-bne+ #x40820000)   ; bne target
(defconstant +opcode-b+ #x48000000)     ; b target
(defconstant +opcode-bl+ #x48000001)    ; bl target
(defconstant +opcode-blr+ #x4E800020)   ; blr
(defconstant +opcode-mflr+ #x7C0802A6)  ; mflr rt
(defconstant +opcode-mtlr+ #x7C0803A6)  ; mtlr rs
(defconstant +opcode-or+ #x7C000378)    ; or ra,rs,rb
(defconstant +opcode-stdu+ #xF8000001)  ; stdu rs,ds(ra)

;; Encode D-form instruction: opcode | RT | RA | SI
(defun encode-d-form (opcode rt ra si)
  (logior opcode
          (ash (logand rt #x1F) 21)
          (ash (logand ra #x1F) 16)
          (logand si #xFFFF)))

;; Encode X-form instruction: opcode | RT | RA | RB
(defun encode-x-form (opcode rt ra rb)
  (logior opcode
          (ash (logand rt #x1F) 21)
          (ash (logand ra #x1F) 16)
          (ash (logand rb #x1F) 11)))

;; Encode I-form instruction: opcode | LI
(defun encode-i-form (opcode li)
  (logior opcode (logand li #x03FFFFFC)))

;; Encode B-form instruction: opcode | BD
(defun encode-b-form (opcode bd)
  (logior opcode (logand bd #x0000FFFC)))

;; Emit PowerISA instruction based on IR opcode
(defun emit-instruction (ir-inst)
  "Emit PowerISA instruction from IR instruction."
  (let ((op (first ir-inst))
        (dst (second ir-inst))
        (src1 (third ir-inst))
        (src2 (fourth ir-inst)))
    (case op
      (:add
       (if (numberp src2)
           (encode-d-form +opcode-addi+ dst src1 src2)
           (encode-x-form +opcode-add+ dst src1 src2)))
      (:sub
       (encode-x-form +opcode-subf+ dst src2 src1))  ; subf swaps args
      (:mul
       (if (numberp src2)
           (encode-d-form +opcode-mulli+ dst src1 src2)
           (encode-x-form +opcode-mullw+ dst src1 src2)))
      (:div
       (encode-x-form +opcode-divw+ dst src1 src2))
      (:load
       (encode-d-form +opcode-ld+ dst src1 src2))
      (:store
       (encode-d-form +opcode-std+ dst src1 src2))
      (:mov
       (encode-x-form +opcode-or+ dst src1 src1))   ; or ra,rs,rs = mov
      (:cmp
       (encode-x-form +opcode-cmpd+ 0 src1 src2))
      (:beq
       (encode-b-form +opcode-beq+ dst))
      (:bne
       (encode-b-form +opcode-bne+ dst))
      (:jmp
       (encode-i-form +opcode-b+ dst))
      (:call
       (encode-i-form +opcode-bl+ dst))
      (:return
       +opcode-blr+)
      (:prologue
       (encode-d-form +opcode-stdu+ +r1+ +r1+ (- (* dst 8))))
      (:epilogue
       (encode-d-form +opcode-addi+ +r1+ +r1+ (* dst 8)))
      (otherwise
       (error "Unknown IR opcode: ~A" op)))))

;; Generate PowerISA code from IR
(defun generate-powerisa (ir)
  "Generate PowerISA code from IR."
  (let ((code nil))
    (dolist (instruction ir)
      (push (emit-instruction instruction) code))
    (nreverse code)))

;; Register allocation using linear scan
(defun allocate-registers (ir)
  "Allocate registers using linear scan algorithm."
  (let ((live-intervals (make-hash-table))
        (reg-pool '(3 4 5 6 7 8 9 10 11 12))  ; Available GPRs
        (allocated (make-hash-table))
        (free-regs nil))
    
    ;; Build live intervals
    (let ((pos 0))
      (dolist (inst ir)
        (let ((def (second inst))
              (uses (cddr inst)))
          ;; Extend interval for definition
          (when (symbolp def)
            (multiple-value-bind (interval found) (gethash def live-intervals)
              (if found
                  (setf (cdr interval) pos)
                  (setf (gethash def live-intervals) (cons pos pos)))))
          ;; Extend intervals for uses
          (dolist (use uses)
            (when (symbolp use)
              (multiple-value-bind (interval found) (gethash use live-intervals)
                (when found
                  (setf (cdr interval) pos))))))
        (incf pos)))
    
    ;; Allocate registers in order of interval start
    (setf free-regs (copy-list reg-pool))
    (let ((sorted-vars nil))
      (maphash (lambda (var interval)
                 (push (cons var interval) sorted-vars))
               live-intervals)
      (setf sorted-vars (sort sorted-vars #'< :key (lambda (x) (cadr x))))
      
      (dolist (entry sorted-vars)
        (let ((var (car entry)))
          (if free-regs
              (let ((reg (pop free-regs)))
                (setf (gethash var allocated) reg))
              ;; Spill - allocate to memory (simplified)
              (setf (gethash var allocated) :spill)))))
    
    ;; Rewrite IR with allocated registers
    (mapcar (lambda (inst)
              (cons (first inst)
                    (mapcar (lambda (op)
                              (if (symbolp op)
                                  (or (gethash op allocated) op)
                                  op))
                            (cdr inst))))
            ir)))

;; Encode instruction wrapper for compatibility
(defun encode-instruction (opcode operands)
  "Encode PowerISA instruction (compatibility wrapper)."
  (emit-instruction (cons opcode operands)))
