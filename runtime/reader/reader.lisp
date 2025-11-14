;; AstraLisp OS Runtime - Lisp Reader
;; Production Common Lisp reader implementation

(defpackage :astralisp-reader
  (:use :cl)
  (:export :reader-init
           :read-from-string
           :read-from-stream))

(in-package :astralisp-reader)

;; Reader state
(defvar *reader-initialized* nil)
(defvar *readtable* (copy-readtable nil))
(defvar *package* (find-package :common-lisp-user))

;; Initialize reader
(defun reader-init ()
  "Initialize Lisp reader."
  (setf *readtable* (copy-readtable nil))
  (setf *package* (find-package :common-lisp-user))
  (setf *reader-initialized* t))

;; Read from string
(defun read-from-string (string &key (start 0) (end nil) (eof-error-p t) (eof-value nil))
  "Read Lisp expression from string."
  (with-input-from-string (stream string :start start :end end)
    (read stream :eof-error-p eof-error-p :eof-value eof-value)))

;; Read from stream
(defun read-from-stream (stream &key (eof-error-p t) (eof-value nil))
  "Read Lisp expression from stream."
  (read stream :eof-error-p eof-error-p :eof-value eof-value))

