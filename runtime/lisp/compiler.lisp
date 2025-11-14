;; AstraLisp OS Lisp Compiler
;; Compile Lisp to native PowerISA code

(defun compile-function (name params body)
  "Compile function to native code"
  (let ((ir (generate-ir params body))
        (optimized-ir (optimize-ir ir))
        (code (generate-code optimized-ir)))
    (jit-compile code)))

(defun generate-ir (params body)
  "Generate intermediate representation"
  (let ((ir nil))
    (dolist (form body)
      (push (compile-form form) ir))
    (reverse ir)))

(defun compile-form (form)
  "Compile single form to IR"
  (cond
    ((atom form) (compile-atom form))
    ((listp form) (compile-list form))
    (t (error "Unknown form type"))))

(defun compile-atom (atom)
  "Compile atom to IR"
  (cond
    ((integerp atom) (make-ir-constant atom))
    ((symbolp atom) (make-ir-load atom))
    (t (error "Unknown atom type"))))

(defun compile-list (list)
  "Compile list to IR"
  (let ((op (car list))
        (args (cdr list)))
    (cond
      ((eq op '+) (compile-add args))
      ((eq op '-) (compile-sub args))
      ((eq op '*) (compile-mul args))
      ((eq op '/) (compile-div args))
      ((eq op 'if) (compile-if args))
      ((eq op 'let) (compile-let args))
      (t (compile-call op args)))))

(defun optimize-ir (ir)
  "Optimize intermediate representation"
  (let ((optimized (constant-fold ir)))
    (dead-code-elimination optimized)))

(defun constant-fold (ir)
  "Constant folding optimization"
  (mapcar (lambda (inst)
            (if (and (eq (ir-op inst) 'add)
                     (constant-p (ir-arg1 inst))
                     (constant-p (ir-arg2 inst)))
                (make-ir-constant (+ (constant-value (ir-arg1 inst))
                                     (constant-value (ir-arg2 inst))))
                inst))
          ir))

(defun dead-code-elimination (ir)
  "Dead code elimination"
  (let ((used (find-used-vars ir))
        (result nil))
    (dolist (inst ir)
      (if (var-used-p (ir-result inst) used)
          (push inst result)))
    (reverse result)))

(defun generate-code (ir)
  "Generate PowerISA code from IR"
  (let ((code nil))
    (dolist (inst ir)
      (push (emit-instruction inst) code))
    (reverse code)))

(defun emit-instruction (inst)
  "Emit single PowerISA instruction"
  (case (ir-op inst)
    (load (emit-load (ir-reg inst) (ir-arg1 inst)))
    (store (emit-store (ir-reg inst) (ir-arg1 inst)))
    (add (emit-add (ir-reg inst) (ir-arg1 inst) (ir-arg2 inst)))
    (sub (emit-sub (ir-reg inst) (ir-arg1 inst) (ir-arg2 inst)))
    (mul (emit-mul (ir-reg inst) (ir-arg1 inst) (ir-arg2 inst)))
    (div (emit-div (ir-reg inst) (ir-arg1 inst) (ir-arg2 inst)))
    (cmp (emit-cmp (ir-arg1 inst) (ir-arg2 inst)))
    (beq (emit-beq (ir-arg1 inst)))
    (bne (emit-bne (ir-arg1 inst)))
    (b (emit-b (ir-arg1 inst)))
    (ret (emit-blr))))
