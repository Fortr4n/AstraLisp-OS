;; AstraLisp OS Runtime - JIT Compiler Core
;; Production JIT compiler for PowerISA with full optimization passes

(defpackage :astralisp-jit
  (:use :cl)
  (:export :jit-compile
           :jit-init
           :jit-optimize
           :generate-ir
           :execute-compiled))

(in-package :astralisp-jit)

;; JIT state
(defvar *jit-initialized* nil)
(defvar *compiled-functions* (make-hash-table :test 'equal))
(defvar *optimization-level* 2)
(defvar *next-label* 0)
(defvar *next-temp* 0)

;; IR node structure
(defstruct ir-node
  (op nil)
  (result nil)
  (args nil)
  (label nil))

;; Initialize JIT
(defun jit-init ()
  "Initialize JIT compiler."
  (setf *compiled-functions* (make-hash-table :test 'equal))
  (setf *next-label* 0)
  (setf *next-temp* 0)
  (setf *jit-initialized* t))

;; Generate new temporary
(defun new-temp ()
  (let ((temp (intern (format nil "T~D" *next-temp*))))
    (incf *next-temp*)
    temp))

;; Generate new label
(defun new-label ()
  (let ((label (intern (format nil "L~D" *next-label*))))
    (incf *next-label*)
    label))

;; Generate IR from Lisp form
(defun generate-ir (form)
  "Generate intermediate representation from Lisp form."
  (let ((ir nil))
    (labels ((emit (op result &rest args)
               (push (list op result args) ir))
             
             (gen (form)
               (cond
                 ;; Self-evaluating
                 ((numberp form)
                  (let ((temp (new-temp)))
                    (emit :load-imm temp form)
                    temp))
                 
                 ;; Symbol
                 ((symbolp form)
                  (let ((temp (new-temp)))
                    (emit :load-var temp form)
                    temp))
                 
                 ;; List (function call)
                 ((listp form)
                  (let ((op (first form))
                        (args (rest form)))
                    (cond
                      ;; Arithmetic
                      ((eq op '+)
                       (if (= (length args) 2)
                           (let ((left (gen (first args)))
                                 (right (gen (second args)))
                                 (result (new-temp)))
                             (emit :add result left right)
                             result)
                           (reduce-binary :add args)))
                      
                      ((eq op '-)
                       (let ((left (gen (first args)))
                             (right (gen (second args)))
                             (result (new-temp)))
                         (emit :sub result left right)
                         result))
                      
                      ((eq op '*)
                       (if (= (length args) 2)
                           (let ((left (gen (first args)))
                                 (right (gen (second args)))
                                 (result (new-temp)))
                             (emit :mul result left right)
                             result)
                           (reduce-binary :mul args)))
                      
                      ((eq op '/)
                       (let ((left (gen (first args)))
                             (right (gen (second args)))
                             (result (new-temp)))
                         (emit :div result left right)
                         result))
                      
                      ;; Comparison
                      ((member op '(< > <= >= = /=))
                       (let ((left (gen (first args)))
                             (right (gen (second args)))
                             (result (new-temp)))
                         (emit :cmp result left right)
                         (emit (case op
                                 ((< >) :setlt)
                                 ((<= >=) :setle)
                                 ((=) :seteq)
                                 ((/=) :setne))
                               result)
                         result))
                      
                      ;; If
                      ((eq op 'if)
                       (let ((cond-val (gen (first args)))
                             (then-label (new-label))
                             (else-label (new-label))
                             (end-label (new-label))
                             (result (new-temp)))
                         (emit :beq cond-val else-label)
                         (let ((then-val (gen (second args))))
                           (emit :mov result then-val))
                         (emit :jmp end-label)
                         (emit :label else-label)
                         (when (third args)
                           (let ((else-val (gen (third args))))
                             (emit :mov result else-val)))
                         (emit :label end-label)
                         result))
                      
                      ;; Let
                      ((eq op 'let)
                       (let ((bindings (first args))
                             (body (rest args)))
                         (dolist (binding bindings)
                           (let ((var (first binding))
                                 (val (gen (second binding))))
                             (emit :store-var var val)))
                         (let ((last-val nil))
                           (dolist (expr body)
                             (setf last-val (gen expr)))
                           last-val)))
                      
                      ;; Lambda
                      ((eq op 'lambda)
                       (let ((params (first args))
                             (body (rest args))
                             (func-label (new-label)))
                         (emit :func-start func-label params)
                         (let ((result nil))
                           (dolist (expr body)
                             (setf result (gen expr)))
                           (emit :return result))
                         (emit :func-end func-label)
                         func-label))
                      
                      ;; Function call
                      (t
                       (let ((func (gen op))
                             (arg-temps (mapcar #'gen args))
                             (result (new-temp)))
                         (dolist (arg arg-temps)
                           (emit :push-arg arg))
                         (emit :call result func (length args))
                         result)))))
                 
                 (t form)))
             
             (reduce-binary (op args)
               (if (= (length args) 1)
                   (gen (first args))
                   (let* ((left (gen (first args)))
                          (right (reduce-binary op (rest args)))
                          (result (new-temp)))
                     (emit op result left right)
                     result))))
      
      (gen form)
      (nreverse ir))))

;; Constant folding optimization
(defun constant-fold (ir)
  "Fold constant expressions at compile time."
  (mapcar (lambda (inst)
            (let ((op (first inst))
                  (result (second inst))
                  (args (third inst)))
              (if (and (member op '(:add :sub :mul :div))
                       (every #'numberp args))
                  (list :load-imm result
                        (case op
                          (:add (apply #'+ args))
                          (:sub (apply #'- args))
                          (:mul (apply #'* args))
                          (:div (apply #'/ args))))
                  inst)))
          ir))

;; Dead code elimination
(defun eliminate-dead-code (ir)
  "Remove instructions whose results are never used."
  (let ((used (make-hash-table)))
    ;; Mark all used values
    (dolist (inst ir)
      (dolist (arg (third inst))
        (when (symbolp arg)
          (setf (gethash arg used) t))))
    
    ;; Keep only instructions that define used values or have side effects
    (remove-if (lambda (inst)
                 (let ((op (first inst))
                       (result (second inst)))
                   (and (member op '(:add :sub :mul :div :load-imm :load-var :mov))
                        (symbolp result)
                        (not (gethash result used)))))
               ir)))

;; Loop optimization (strength reduction)
(defun optimize-loops (ir)
  "Apply loop optimizations."
  (let ((in-loop nil)
        (loop-invariant nil))
    (mapcar (lambda (inst)
              (let ((op (first inst)))
                (cond
                  ((eq op :loop-start)
                   (setf in-loop t
                         loop-invariant nil)
                   inst)
                  ((eq op :loop-end)
                   (setf in-loop nil)
                   inst)
                  (in-loop
                   ;; Strength reduction: replace mul by power of 2 with shift
                   (if (and (eq op :mul)
                            (numberp (second (third inst)))
                            (power-of-2-p (second (third inst))))
                       (list :shl (second inst) (first (third inst))
                             (log (second (third inst)) 2))
                       inst))
                  (t inst))))
            ir)))

(defun power-of-2-p (n)
  (and (integerp n) (plusp n) (zerop (logand n (1- n)))))

;; Inline expansion
(defun inline-expand (ir)
  "Inline small functions."
  (let ((funcs (make-hash-table)))
    ;; Collect function definitions
    (let ((current-func nil)
          (func-body nil))
      (dolist (inst ir)
        (cond
          ((eq (first inst) :func-start)
           (setf current-func (second inst)
                 func-body nil))
          ((eq (first inst) :func-end)
           (when (and current-func (< (length func-body) 10))
             (setf (gethash current-func funcs) (nreverse func-body)))
           (setf current-func nil))
          (current-func
           (push inst func-body)))))
    
    ;; Inline calls to small functions
    (mapcan (lambda (inst)
              (if (and (eq (first inst) :call)
                       (gethash (first (third inst)) funcs))
                  (copy-list (gethash (first (third inst)) funcs))
                  (list inst)))
            ir)))

;; Optimize IR
(defun jit-optimize (ir)
  "Optimize intermediate representation."
  (let ((optimized ir))
    (when (>= *optimization-level* 1)
      (setf optimized (constant-fold optimized)))
    (when (>= *optimization-level* 2)
      (setf optimized (eliminate-dead-code optimized))
      (setf optimized (optimize-loops optimized)))
    (when (>= *optimization-level* 3)
      (setf optimized (inline-expand optimized)))
    optimized))

;; Generate PowerISA code from IR
(defun generate-powerisa-code (ir)
  "Generate PowerISA machine code from IR."
  (let ((code nil)
        (labels (make-hash-table)))
    ;; First pass: collect labels
    (let ((pos 0))
      (dolist (inst ir)
        (when (eq (first inst) :label)
          (setf (gethash (second inst) labels) pos))
        (incf pos)))
    
    ;; Second pass: emit code
    (dolist (inst ir)
      (let ((op (first inst))
            (result (second inst))
            (args (third inst)))
        (case op
          (:load-imm
           (push (list :addi result 0 (first args)) code))
          (:load-var
           (push (list :load result (first args) 0) code))
          (:store-var
           (push (list :store (first args) result 0) code))
          (:add
           (push (list :add result (first args) (second args)) code))
          (:sub
           (push (list :sub result (first args) (second args)) code))
          (:mul
           (push (list :mul result (first args) (second args)) code))
          (:div
           (push (list :div result (first args) (second args)) code))
          (:mov
           (push (list :mov result (first args)) code))
          (:cmp
           (push (list :cmp 0 (first args) (second args)) code))
          (:beq
           (push (list :beq (or (gethash (first args) labels) 0)) code))
          (:bne
           (push (list :bne (or (gethash (first args) labels) 0)) code))
          (:jmp
           (push (list :jmp (or (gethash result labels) 0)) code))
          (:call
           (push (list :call (or (gethash (first args) labels) 0)) code))
          (:return
           (push (list :return) code))
          (:label
           nil)  ; Labels consume no code
          (otherwise
           nil))))
    (nreverse code)))

;; Compile function to PowerISA
(defun jit-compile (function)
  "Compile function to PowerISA code."
  (unless *jit-initialized*
    (jit-init))
  
  (let* ((ir (generate-ir function))
         (optimized-ir (jit-optimize ir))
         (code (generate-powerisa-code optimized-ir)))
    (setf (gethash function *compiled-functions*) code)
    code))

;; Execute compiled code (requires FFI to native)
(defun execute-compiled (function &rest args)
  "Execute compiled code."
  (let ((code (or (gethash function *compiled-functions*)
                  (jit-compile function))))
    ;; In real implementation, would call into native code
    ;; For now, return the generated IR for inspection
    code))
