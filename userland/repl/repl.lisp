;; AstraLisp OS System REPL
;; Production system REPL with introspection

(defpackage :astralisp-repl
  (:use :cl)
  (:export :repl-run
           :repl-eval))

(in-package :astralisp-repl)

(defvar *repl-prompt* "ASTRA> ")

(defun repl-run ()
  "Run system REPL."
  (format t "AstraLisp OS System REPL~%")
  (loop
    (format t "~A" *repl-prompt*)
    (let ((form (read)))
      (when (eq form :quit)
        (return))
      (let ((result (repl-eval form)))
        (format t "~A~%" result)))))

(defun repl-eval (form)
  "Evaluate form in REPL."
  (handler-case
      (eval form)
    (error (e)
      (format nil "Error: ~A" e))))

