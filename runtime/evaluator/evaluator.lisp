;; AstraLisp OS Runtime - Meta-Circular Evaluator
;; Production meta-circular Lisp evaluator

(defpackage :astralisp-evaluator
  (:use :cl)
  (:export :evaluator-init
           :eval-form
           :apply-function))

(in-package :astralisp-evaluator)

;; Evaluator state
(defvar *evaluator-initialized* nil)
(defvar *environment* (make-hash-table))

;; Initialize evaluator
(defun evaluator-init ()
  "Initialize meta-circular evaluator."
  (setf *environment* (make-hash-table))
  (setf *evaluator-initialized* t))

;; Evaluate form
(defun eval-form (form &optional env)
  "Evaluate Lisp form."
  (let ((environment (or env *environment*)))
    (cond
      ;; Self-evaluating
      ((or (numberp form) (stringp form) (characterp form))
       form)
      ;; Symbol
      ((symbolp form)
       (get-variable form environment))
      ;; List
      ((listp form)
       (if (null form)
           nil
           (let ((operator (first form))
                 (arguments (rest form)))
             (cond
               ;; Special forms
               ((eq operator 'quote)
                (first arguments))
               ((eq operator 'if)
                (if (eval-form (first arguments) environment)
                    (eval-form (second arguments) environment)
                    (eval-form (third arguments) environment)))
               ((eq operator 'lambda)
                (make-closure (second arguments) (rest (rest arguments)) environment))
               ((eq operator 'let)
                (eval-let arguments environment))
               ((eq operator 'setq)
                (set-variable (first arguments) (eval-form (second arguments) environment) environment))
               ;; Function call
               (t
                (apply-function (eval-form operator environment)
                                (mapcar (lambda (arg) (eval-form arg environment)) arguments)))))))
      (t
       form))))

;; Apply function
(defun apply-function (function arguments)
  "Apply function to arguments."
  (if (functionp function)
      (funcall function arguments)
      (error "Not a function: ~A" function)))

;; Utility functions
(defun get-variable (symbol env)
  "Get variable value from environment."
  (gethash symbol env))

(defun set-variable (symbol value env)
  "Set variable value in environment."
  (setf (gethash symbol env) value))

(defun make-closure (params body env)
  "Create closure."
  (lambda (args)
    (let ((new-env (copy-hash-table env)))
      (loop for param in params
            for arg in args
            do (setf (gethash param new-env) arg))
      (eval-form (first body) new-env))))

(defun eval-let (bindings body env)
  "Evaluate let form."
  (let ((new-env (copy-hash-table env)))
    (dolist (binding bindings)
      (setf (gethash (first binding) new-env)
            (eval-form (second binding) env)))
    (eval-form (first body) new-env)))

(defun copy-hash-table (ht)
  "Copy hash table."
  (let ((new-ht (make-hash-table)))
    (maphash (lambda (k v) (setf (gethash k new-ht) v)) ht)
    new-ht))

