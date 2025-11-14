;; AstraLisp OS Kernel Scheduler
;; Production multi-level feedback queue scheduler with real-time support

(defpackage :astralisp-scheduler
  (:use :cl)
  (:export :scheduler-init
           :scheduler-add-thread
           :scheduler-remove-thread
           :scheduler-yield
           :scheduler-should-yield
           :scheduler-shutdown
           :current-thread
           :schedule-thread))

(in-package :astralisp-scheduler)

;; Thread states
(defconstant +thread-state-ready+ 0)
(defconstant +thread-state-running+ 1)
(defconstant +thread-state-blocked+ 2)
(defconstant +thread-state-sleeping+ 3)
(defconstant +thread-state-zombie+ 4)

;; Priority levels (0 = highest, 31 = lowest)
(defconstant +max-priority+ 31)
(defconstant +realtime-priority+ 0)
(defconstant +normal-priority+ 15)
(defconstant +idle-priority+ 31)

;; Time slice (in ticks)
(defconstant +default-timeslice+ 10)
(defconstant +realtime-timeslice+ 5)
(defconstant +idle-timeslice+ 1)

;; Thread structure
(defstruct thread
  "Thread control block."
  (id 0 :type (unsigned-byte 64))
  (state +thread-state-ready+ :type (unsigned-byte 8))
  (priority +normal-priority+ :type (unsigned-byte 8))
  (timeslice +default-timeslice+ :type (unsigned-byte 16))
  (remaining-ticks +default-timeslice+ :type (unsigned-byte 16))
  (cpu-affinity 0 :type (unsigned-byte 32))  ; Bitmask of CPUs
  (stack-pointer 0 :type (unsigned-byte 64))
  (program-counter 0 :type (unsigned-byte 64))
  (registers (make-array 32 :element-type '(unsigned-byte 64)) :type (vector (unsigned-byte 64)))
  (next nil :type (or null thread))
  (prev nil :type (or null thread))
  (wait-queue nil :type list)
  (sleep-until 0 :type (unsigned-byte 64))
  (name "" :type string))

;; Scheduler state
(defvar *scheduler-initialized* nil)
(defvar *current-thread* nil)
(defvar *idle-thread* nil)
(defvar *ready-queues* (make-array 32 :initial-element nil))  ; One queue per priority
(defvar *blocked-threads* nil)
(defvar *sleeping-threads* nil)
(defvar *thread-id-counter* 0)
(defvar *scheduler-lock* nil)

;; Initialize scheduler
(defun scheduler-init ()
  "Initialize the scheduler."
  (when *scheduler-initialized*
    (error "Scheduler already initialized"))
  
  ;; Initialize ready queues
  (dotimes (i 32)
    (setf (aref *ready-queues* i) nil))
  
  ;; Create idle thread
  (setf *idle-thread* (make-thread
                       :id (incf *thread-id-counter)
                       :state +thread-state-ready+
                       :priority +idle-priority+
                       :name "idle"
                       :timeslice +idle-timeslice+
                       :remaining-ticks +idle-timeslice+))
  
  ;; Initialize scheduler lock
  (setf *scheduler-lock* (make-spinlock))
  
  ;; Set current thread to idle
  (setf *current-thread* *idle-thread*)
  
  (setf *scheduler-initialized* t))

;; Add thread to ready queue
(defun scheduler-add-thread (thread)
  "Add thread to appropriate ready queue based on priority."
  (declare (type thread thread))
  (with-spinlock (*scheduler-lock*)
    (let ((priority (thread-priority thread)))
      (setf (thread-state thread) +thread-state-ready+)
      (setf (thread-remaining-ticks thread) (thread-timeslice thread))
      (push thread (aref *ready-queues* priority))
      (setf (aref *ready-queues* priority)
            (sort (aref *ready-queues* priority) #'< :key #'thread-remaining-ticks)))))

;; Remove thread from scheduler
(defun scheduler-remove-thread (thread)
  "Remove thread from scheduler."
  (declare (type thread thread))
  (with-spinlock (*scheduler-lock*)
    (let ((priority (thread-priority thread)))
      (setf (aref *ready-queues* priority)
            (remove thread (aref *ready-queues* priority)))
      (setf *blocked-threads* (remove thread *blocked-threads*))
      (setf *sleeping-threads* (remove thread *sleeping-threads*))
      (when (eq thread *current-thread*)
        (setf *current-thread* nil)))))

;; Select next thread to run
(defun select-next-thread ()
  "Select next thread to run using multi-level feedback queue algorithm."
  (with-spinlock (*scheduler-lock*)
    ;; Check real-time threads first (priority 0)
    (dotimes (priority 32)
      (let ((queue (aref *ready-queues* priority)))
        (when queue
          (let ((next-thread (pop queue)))
            (setf (aref *ready-queues* priority) queue)
            (return-from select-next-thread next-thread)))))
    
    ;; No ready threads, return idle
    *idle-thread*))

;; Update thread priorities (aging)
(defun age-threads ()
  "Age threads to prevent starvation."
  (with-spinlock (*scheduler-lock*)
    (dotimes (priority 31)  ; Don't age priority 31 (idle)
      (let ((queue (aref *ready-queues* priority)))
        (when queue
          ;; Move one thread to lower priority (if not real-time)
          (when (and (> priority +realtime-priority+)
                     queue)
            (let ((thread (pop queue)))
              (setf (aref *ready-queues* priority) queue)
              (setf (thread-priority thread) (min +max-priority+ (1+ priority)))
              (push thread (aref *ready-queues* (thread-priority thread)))))))))

;; Check sleeping threads
(defun check-sleeping-threads ()
  "Wake up threads that have finished sleeping."
  (let ((current-time (get-tick-count)))
    (with-spinlock (*scheduler-lock*)
      (setf *sleeping-threads*
            (remove-if (lambda (thread)
                         (when (>= current-time (thread-sleep-until thread))
                           (setf (thread-state thread) +thread-state-ready+)
                           (scheduler-add-thread thread)
                           t))
                       *sleeping-threads*)))))

;; Scheduler yield
(defun scheduler-yield ()
  "Yield current thread and switch to next."
  (when (not *scheduler-initialized*)
    (return-from scheduler-yield))
  
  (let ((current *current-thread*))
    ;; Decrement remaining ticks
    (when (> (thread-remaining-ticks current) 0)
      (decf (thread-remaining-ticks current)))
    
    ;; If timeslice expired or thread blocked, switch
    (when (or (zerop (thread-remaining-ticks current))
              (not (eq (thread-state current) +thread-state-running+)))
      ;; Save current thread state
      (save-thread-context current)
      
      ;; Select next thread
      (let ((next (select-next-thread)))
        (when (not (eq next current))
          ;; Switch context
          (switch-thread-context current next)
          (setf *current-thread* next)
          (setf (thread-state next) +thread-state-running+)
          (setf (thread-remaining-ticks next) (thread-timeslice next)))))
    
    ;; Age threads periodically
    (when (zerop (mod (get-tick-count) 100))
      (age-threads))
    
    ;; Check sleeping threads
    (check-sleeping-threads)))

(defun scheduler-should-yield ()
  "Check if scheduler should yield."
  (and *scheduler-initialized*
       (or (zerop (thread-remaining-ticks *current-thread*))
           (not (eq (thread-state *current-thread*) +thread-state-running+)))))

(defun scheduler-shutdown ()
  "Shutdown scheduler."
  (setf *scheduler-initialized* nil)
  (setf *current-thread* nil))

;; Thread context switching (implemented in assembly)
(defun save-thread-context (thread)
  "Save thread context - implemented in assembly."
  (declare (type thread thread))
  (error "save-thread-context must be implemented in assembly"))

(defun switch-thread-context (from to)
  "Switch thread context - implemented in assembly."
  (declare (type thread from) (type thread to))
  (error "switch-thread-context must be implemented in assembly"))

;; Utility functions
(defun current-thread ()
  "Get current running thread."
  *current-thread*)

(defun schedule-thread (thread)
  "Schedule a thread to run."
  (scheduler-add-thread thread))

;; Spinlock implementation
(defstruct spinlock
  "Spinlock structure."
  (locked nil :type boolean))

(defmacro with-spinlock ((lock) &body body)
  "Execute body with spinlock held."
  `(progn
     (spinlock-acquire ,lock)
     (unwind-protect
          (progn ,@body)
       (spinlock-release ,lock))))

(defun spinlock-acquire (lock)
  "Acquire spinlock."
  (declare (type spinlock lock))
  (loop while (spinlock-locked lock)
        do (cpu-pause))
  (setf (spinlock-locked lock) t))

(defun spinlock-release (lock)
  "Release spinlock."
  (declare (type spinlock lock))
  (setf (spinlock-locked lock) nil))

;; Forward declarations
(defun get-tick-count () 0)
(defun cpu-pause ())

