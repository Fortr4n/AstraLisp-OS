;; AstraLisp OS Kernel Synchronization - Mutex
;; Production mutex implementation

(defpackage :astralisp-mutex
  (:use :cl)
  (:export :make-mutex
           :mutex-lock
           :mutex-unlock
           :with-mutex))

(in-package :astralisp-mutex)

;; Mutex types
(defconstant +mutex-type-normal+ 0)
(defconstant +mutex-type-recursive+ 1)
(defconstant +mutex-type-errorcheck+ 2)

(defstruct mutex
  "Mutex structure with priority inheritance."
  (lock-word 0 :type (unsigned-byte 32))     ; Atomic lock word
  (owner nil :type (or null t))               ; Owning thread
  (owner-priority 0 :type (unsigned-byte 8)) ; Original owner priority
  (recursion-count 0 :type (unsigned-byte 32)) ; For recursive mutexes
  (type +mutex-type-normal+ :type (unsigned-byte 8))
  (wait-queue nil :type list)                 ; Waiting threads (priority-sorted)
  (spinlock nil :type t))                     ; Spinlock for wait queue protection

(defun make-mutex (&key (type +mutex-type-normal+))
  "Create new mutex."
  (make-mutex-struct
   :lock-word 0
   :owner nil
   :owner-priority 0
   :recursion-count 0
   :type type
   :wait-queue nil
   :spinlock (make-spinlock)))

(defun make-mutex-struct (&key lock-word owner owner-priority recursion-count type wait-queue spinlock)
  "Internal constructor for mutex."
  (let ((m (allocate-struct)))
    (setf (mutex-lock-word m) lock-word)
    (setf (mutex-owner m) owner)
    (setf (mutex-owner-priority m) owner-priority)
    (setf (mutex-recursion-count m) recursion-count)
    (setf (mutex-type m) type)
    (setf (mutex-wait-queue m) wait-queue)
    (setf (mutex-spinlock m) spinlock)
    m))

(defun mutex-lock (mutex &key (timeout nil))
  "Lock mutex with optional timeout."
  (declare (type mutex mutex))
  (let ((current (current-thread)))
    (when (null current)
      (return-from mutex-lock nil))

    ;; Try fast path: atomic compare-and-swap
    (when (compare-and-swap (mutex-lock-word mutex) 0 1)
      (setf (mutex-owner mutex) current)
      (setf (mutex-owner-priority mutex) (thread-priority current))
      (when (= (mutex-type mutex) +mutex-type-recursive+)
        (setf (mutex-recursion-count mutex) 1))
      (return-from mutex-lock t))

    ;; Check for recursive lock
    (when (and (= (mutex-type mutex) +mutex-type-recursive+)
               (eq (mutex-owner mutex) current))
      (incf (mutex-recursion-count mutex))
      (return-from mutex-lock t))

    ;; Check for error detection
    (when (and (= (mutex-type mutex) +mutex-type-errorcheck+)
               (eq (mutex-owner mutex) current))
      (error "Deadlock detected: thread already owns mutex"))

    ;; Slow path: must wait
    (spinlock-acquire (mutex-spinlock mutex))
    (unwind-protect
        (progn
          ;; Implement priority inheritance
          (when (and (mutex-owner mutex)
                     (< (thread-priority (mutex-owner mutex))
                        (thread-priority current)))
            ;; Boost owner priority to prevent priority inversion
            (thread-set-priority (mutex-owner mutex) (thread-priority current)))

          ;; Add to wait queue (sorted by priority)
          (setf (mutex-wait-queue mutex)
                (merge 'list
                       (list current)
                       (mutex-wait-queue mutex)
                       #'>
                       :key #'thread-priority))

          ;; Release spinlock before blocking
          (spinlock-release (mutex-spinlock mutex))

          ;; Block until mutex available
          (let ((deadline (when timeout (+ (get-tick-count) timeout))))
            (loop
              (thread-block current)

              ;; Check if we got the mutex
              (when (eq (mutex-owner mutex) current)
                (return t))

              ;; Check timeout
              (when (and deadline (>= (get-tick-count) deadline))
                (spinlock-acquire (mutex-spinlock mutex))
                (setf (mutex-wait-queue mutex)
                      (remove current (mutex-wait-queue mutex)))
                (spinlock-release (mutex-spinlock mutex))
                (return nil)))))
      ;; Ensure spinlock is released
      (when (spinlock-locked (mutex-spinlock mutex))
        (spinlock-release (mutex-spinlock mutex))))))

(defun mutex-unlock (mutex)
  "Unlock mutex."
  (declare (type mutex mutex))
  (let ((current (current-thread)))
    (when (null current)
      (return-from mutex-unlock nil))

    ;; Verify ownership
    (when (not (eq (mutex-owner mutex) current))
      (when (= (mutex-type mutex) +mutex-type-errorcheck+)
        (error "Mutex not owned by current thread"))
      (return-from mutex-unlock nil))

    ;; Handle recursive mutex
    (when (= (mutex-type mutex) +mutex-type-recursive+)
      (when (> (mutex-recursion-count mutex) 1)
        (decf (mutex-recursion-count mutex))
        (return-from mutex-unlock t)))

    (spinlock-acquire (mutex-spinlock mutex))
    (unwind-protect
        (progn
          ;; Restore original priority if we boosted it
          (when (not (= (thread-priority current) (mutex-owner-priority mutex)))
            (thread-set-priority current (mutex-owner-priority mutex)))

          ;; Wake next waiter if any
          (if (mutex-wait-queue mutex)
              (let ((next-owner (pop (mutex-wait-queue mutex))))
                (setf (mutex-owner mutex) next-owner)
                (setf (mutex-owner-priority mutex) (thread-priority next-owner))
                (when (= (mutex-type mutex) +mutex-type-recursive+)
                  (setf (mutex-recursion-count mutex) 1))
                (thread-wake next-owner))
              ;; No waiters, release mutex
              (progn
                (setf (mutex-owner mutex) nil)
                (setf (mutex-lock-word mutex) 0))))
      (spinlock-release (mutex-spinlock mutex)))
    t))

(defun mutex-trylock (mutex)
  "Try to lock mutex without blocking."
  (declare (type mutex mutex))
  (let ((current (current-thread)))
    (when (null current)
      (return-from mutex-trylock nil))

    ;; Check for recursive lock
    (when (and (= (mutex-type mutex) +mutex-type-recursive+)
               (eq (mutex-owner mutex) current))
      (incf (mutex-recursion-count mutex))
      (return-from mutex-trylock t))

    ;; Try atomic acquisition
    (when (compare-and-swap (mutex-lock-word mutex) 0 1)
      (setf (mutex-owner mutex) current)
      (setf (mutex-owner-priority mutex) (thread-priority current))
      (when (= (mutex-type mutex) +mutex-type-recursive+)
        (setf (mutex-recursion-count mutex) 1))
      t)))

(defmacro with-mutex ((mutex) &body body)
  "Execute body with mutex held."
  `(progn
     (mutex-lock ,mutex)
     (unwind-protect
          (progn ,@body)
       (mutex-unlock ,mutex))))

;; Spinlock implementation
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
  "Acquire spinlock using atomic operations."
  (declare (type spinlock lock))
  (loop
    (when (compare-and-swap (spinlock-locked lock) 0 1)
      (return))
    (cpu-pause)))

(defun spinlock-release (lock)
  "Release spinlock."
  (declare (type spinlock lock))
  (atomic-store (spinlock-locked lock) 0))

;; Atomic operations (FFI to assembly)
(defun compare-and-swap (location old-value new-value)
  "Atomic compare-and-swap operation."
  (declare (ignore location old-value new-value))
  ;; This must be implemented in assembly using PowerISA lwarx/stwcx
  (ffi-compare-and-swap location old-value new-value))

(defun atomic-store (location value)
  "Atomic store operation."
  (declare (ignore location value))
  (ffi-atomic-store location value))

;; Forward declarations
(defun current-thread ()
  "Get current thread."
  nil)

(defun thread-block (thread)
  "Block thread."
  (declare (ignore thread))
  nil)

(defun thread-wake (thread)
  "Wake thread."
  (declare (ignore thread))
  nil)

(defun thread-priority (thread)
  "Get thread priority."
  (declare (ignore thread))
  15)

(defun thread-set-priority (thread priority)
  "Set thread priority."
  (declare (ignore thread priority))
  nil)

(defun get-tick-count ()
  "Get system tick count."
  0)

(defun cpu-pause ()
  "Pause CPU for spinlock."
  nil)

(defun allocate-struct ()
  "Allocate structure."
  (make-hash-table))

;; FFI declarations
(defun ffi-compare-and-swap (location old-val new-val)
  "FFI: Atomic compare-and-swap."
  (declare (ignore location old-val new-val))
  nil)

(defun ffi-atomic-store (location value)
  "FFI: Atomic store."
  (declare (ignore location value))
  nil)

