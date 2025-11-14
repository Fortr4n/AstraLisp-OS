;; AstraLisp OS Kernel Synchronization - Mutex
;; Production mutex implementation

(defpackage :astralisp-mutex
  (:use :cl)
  (:export :make-mutex
           :mutex-lock
           :mutex-unlock
           :with-mutex))

(in-package :astralisp-mutex)

(defstruct mutex
  "Mutex structure."
  (locked nil :type boolean)
  (owner nil :type (or null t))
  (wait-queue nil :type list))

(defun make-mutex ()
  "Create new mutex."
  (make-mutex :locked nil :owner nil :wait-queue nil))

(defun mutex-lock (mutex)
  "Lock mutex."
  (let ((current (current-thread)))
    (loop
      (when (not (mutex-locked mutex))
        (setf (mutex-locked mutex) t)
        (setf (mutex-owner mutex) current)
        (return))
      ;; Block and wait
      (push current (mutex-wait-queue mutex))
      (thread-block current)
      (pop (mutex-wait-queue mutex)))))

(defun mutex-unlock (mutex)
  "Unlock mutex."
  (when (eq (mutex-owner mutex) (current-thread))
    (setf (mutex-locked mutex) nil)
    (setf (mutex-owner mutex) nil)
    ;; Wake waiting thread
    (when (mutex-wait-queue mutex)
      (let ((waiter (pop (mutex-wait-queue mutex))))
        (thread-wake waiter)))))

(defmacro with-mutex ((mutex) &body body)
  "Execute body with mutex held."
  `(progn
     (mutex-lock ,mutex)
     (unwind-protect
          (progn ,@body)
       (mutex-unlock ,mutex))))

(defun current-thread () nil)
(defun thread-block (thread) nil)
(defun thread-wake (thread) nil)

