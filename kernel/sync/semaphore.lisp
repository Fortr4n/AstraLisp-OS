;; AstraLisp OS Kernel Synchronization - Semaphore
;; Production semaphore implementation

(defpackage :astralisp-semaphore
  (:use :cl)
  (:export :make-semaphore
           :semaphore-wait
           :semaphore-post))

(in-package :astralisp-semaphore)

(defstruct semaphore
  "Semaphore structure with atomic operations."
  (count 0 :type (unsigned-byte 32))          ; Current count (atomic)
  (max-count 0 :type (unsigned-byte 32))      ; Maximum count
  (wait-queue nil :type list)                  ; Priority-sorted wait queue
  (spinlock nil :type t))                      ; Spinlock for queue protection

(defun make-semaphore (initial-count max-count)
  "Create new semaphore."
  (make-semaphore-struct
   :count initial-count
   :max-count max-count
   :wait-queue nil
   :spinlock (make-spinlock)))

(defun make-semaphore-struct (&key count max-count wait-queue spinlock)
  "Internal semaphore constructor."
  (let ((s (allocate-struct)))
    (setf (semaphore-count s) count)
    (setf (semaphore-max-count s) max-count)
    (setf (semaphore-wait-queue s) wait-queue)
    (setf (semaphore-spinlock s) spinlock)
    s))

(defun semaphore-wait (semaphore &key (timeout nil))
  "Wait on semaphore (P operation) with optional timeout."
  (declare (type semaphore semaphore))
  (let ((current (current-thread)))
    (when (null current)
      (return-from semaphore-wait nil))

    ;; Try fast path: atomic decrement if count > 0
    (loop
      (let ((current-count (semaphore-count semaphore)))
        (when (<= current-count 0)
          ;; Count is 0, must block
          (return))
        (when (compare-and-swap (semaphore-count semaphore)
                                current-count
                                (1- current-count))
          ;; Success, acquired semaphore
          (return-from semaphore-wait t))))

    ;; Slow path: must wait
    (spinlock-acquire (semaphore-spinlock semaphore))
    (unwind-protect
        (progn
          ;; Add to wait queue (sorted by priority)
          (setf (semaphore-wait-queue semaphore)
                (merge 'list
                       (list current)
                       (semaphore-wait-queue semaphore)
                       #'>
                       :key #'thread-priority))

          ;; Release spinlock before blocking
          (spinlock-release (semaphore-spinlock semaphore))

          ;; Block until semaphore available
          (let ((deadline (when timeout (+ (get-tick-count) timeout))))
            (loop
              (thread-block current)

              ;; Check if we got the semaphore (count was decremented for us)
              (spinlock-acquire (semaphore-spinlock semaphore))
              (let ((found (not (member current (semaphore-wait-queue semaphore)))))
                (spinlock-release (semaphore-spinlock semaphore))
                (when found
                  (return t)))

              ;; Check timeout
              (when (and deadline (>= (get-tick-count) deadline))
                (spinlock-acquire (semaphore-spinlock semaphore))
                (setf (semaphore-wait-queue semaphore)
                      (remove current (semaphore-wait-queue semaphore)))
                (spinlock-release (semaphore-spinlock semaphore))
                (return nil)))))
      ;; Ensure spinlock is released
      (when (and (semaphore-spinlock semaphore)
                 (spinlock-locked (semaphore-spinlock semaphore)))
        (spinlock-release (semaphore-spinlock semaphore))))))

(defun semaphore-post (semaphore)
  "Post to semaphore (V operation)."
  (declare (type semaphore semaphore))

  (spinlock-acquire (semaphore-spinlock semaphore))
  (unwind-protect
      (progn
        ;; Check if at maximum
        (when (>= (semaphore-count semaphore) (semaphore-max-count semaphore))
          (return-from semaphore-post nil))

        ;; Wake waiting thread if any, otherwise increment count
        (if (semaphore-wait-queue semaphore)
            (let ((waiter (pop (semaphore-wait-queue semaphore))))
              ;; Don't increment count, waiter will consume the post
              (thread-wake waiter))
            ;; No waiters, increment count atomically
            (loop
              (let ((current-count (semaphore-count semaphore)))
                (when (>= current-count (semaphore-max-count semaphore))
                  (return))
                (when (compare-and-swap (semaphore-count semaphore)
                                        current-count
                                        (1+ current-count))
                  (return)))))
        t)
    (spinlock-release (semaphore-spinlock semaphore))))

(defun semaphore-trywait (semaphore)
  "Try to wait on semaphore without blocking."
  (declare (type semaphore semaphore))

  ;; Try atomic decrement
  (loop
    (let ((current-count (semaphore-count semaphore)))
      (when (<= current-count 0)
        ;; Count is 0, cannot acquire
        (return nil))
      (when (compare-and-swap (semaphore-count semaphore)
                              current-count
                              (1- current-count))
        ;; Success
        (return t)))))

(defun semaphore-getvalue (semaphore)
  "Get current semaphore value."
  (declare (type semaphore semaphore))
  (semaphore-count semaphore))

;; Spinlock implementation (minimal - relies on external implementation)
(defstruct spinlock
  "Spinlock structure."
  (locked 0 :type (unsigned-byte 32)))

(defun make-spinlock ()
  "Create spinlock."
  (make-spinlock-struct :locked 0))

(defun make-spinlock-struct (&key locked)
  "Internal spinlock constructor."
  (let ((s (allocate-struct)))
    (setf (spinlock-locked s) locked)
    s))

(defun spinlock-acquire (lock)
  "Acquire spinlock."
  (declare (type spinlock lock))
  (loop
    (when (compare-and-swap (spinlock-locked lock) 0 1)
      (return))
    (cpu-pause)))

(defun spinlock-release (lock)
  "Release spinlock."
  (declare (type spinlock lock))
  (atomic-store (spinlock-locked lock) 0))

;; Atomic operations
(defun compare-and-swap (location old-value new-value)
  "Atomic compare-and-swap."
  (let ((result (alien-funcall (extern-alien "kernel_atomic_compare_and_swap" 
                               (function (* void) (* void) (* void) (* void)))
                               location (int-to-lisp old-value) (int-to-lisp new-value))))
    (not (null result))))

(defun atomic-store (location value)
  "Atomic store."
  (alien-funcall (extern-alien "kernel_atomic_store" 
                 (function (* void) (* void) (* void)))
                 location (int-to-lisp value)))

;; Forward declarations
(defun current-thread ()
  "Get current thread."
  (alien-funcall (extern-alien "kernel_get_current_thread_ptr" (function (* void)))))

(defun thread-block (thread)
  "Block thread."
  (alien-funcall (extern-alien "kernel_thread_block" (function (* void) (* void))) thread))

(defun thread-wake (thread)
  "Wake thread."
  (alien-funcall (extern-alien "kernel_thread_wake" (function (* void) (* void))) thread))

(defun thread-priority (thread)
  "Get thread priority."
  (lisp-to-int (alien-funcall (extern-alien "kernel_thread_get_priority" (function (* void) (* void))) thread)))

(defun get-tick-count ()
  "Get system tick count."
  (lisp-to-int (alien-funcall (extern-alien "get_tick_count" (function (* void))))))

(defun cpu-pause ()
  "Pause CPU."
  (alien-funcall (extern-alien "kernel_cpu_pause" (function (* void)))))

(defun allocate-struct ()
  "Allocate structure."
  (alien-funcall (extern-alien "kmalloc" (function (* void) unsigned-long)) 64))

;; Helper to box int/lisp conversion if needed by raw FFI
(defun lisp-to-int (obj) obj) 
(defun int-to-lisp (val) val)

