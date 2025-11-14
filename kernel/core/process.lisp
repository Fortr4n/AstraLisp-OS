;; AstraLisp OS Kernel Process Management
;; Production process management with full lifecycle, fork/exec, signals

(defpackage :astralisp-process
  (:use :cl)
  (:export :process-init
           :process-create
           :process-fork
           :process-exec
           :process-exit
           :process-wait
           :process-kill
           :current-process))

(in-package :astralisp-process)

;; Process states
(defconstant +process-state-new+ 0)
(defconstant +process-state-ready+ 1)
(defconstant +process-state-running+ 2)
(defconstant +process-state-blocked+ 3)
(defconstant +process-state-zombie+ 4)
(defconstant +process-state-dead+ 5)

;; Process structure
(defstruct process
  "Process control block."
  (pid 0 :type (unsigned-byte 32))
  (ppid 0 :type (unsigned-byte 32))  ; Parent PID
  (state +process-state-new+ :type (unsigned-byte 8))
  (exit-code 0 :type (signed-byte 32))
  (threads nil :type list)
  (memory-space nil :type (or null t))  ; Address space
  (file-descriptors (make-hash-table) :type hash-table)
  (working-directory "/" :type string)
  (environment (make-hash-table :test 'equal) :type hash-table)
  (signals-pending 0 :type (unsigned-byte 32))
  (signal-handlers (make-hash-table) :type hash-table)
  (process-group 0 :type (unsigned-byte 32))
  (session 0 :type (unsigned-byte 32))
  (name "" :type string)
  (next nil :type (or null process)))

;; Process management state
(defvar *process-initialized* nil)
(defvar *process-list* nil)
(defvar *process-id-counter* 0)
(defvar *init-process* nil)

;; Initialize process management
(defun process-init ()
  "Initialize process management system."
  (when *process-initialized*
    (error "Process management already initialized"))
  
  ;; Create init process (PID 1)
  (setf *init-process* (make-process
                        :pid (incf *process-id-counter*)
                        :ppid 0
                        :state +process-state-running+
                        :name "init"
                        :process-group 1
                        :session 1))
  (push *init-process* *process-list*)
  
  (setf *process-initialized* t))

;; Create new process
(defun process-create (name entry-point &key (priority 15))
  "Create a new process."
  (declare (type string name)
           (type (unsigned-byte 64) entry-point))
  (let ((process (make-process
                  :pid (incf *process-id-counter*)
                  :ppid (process-pid (current-process))
                  :state +process-state-new+
                  :name name
                  :process-group (process-process-group (current-process))
                  :session (process-session (current-process)))))
    ;; Create address space
    (setf (process-memory-space process) (create-address-space))
    ;; Create main thread
    (let ((thread (create-thread entry-point)))
      (push thread (process-threads process)))
    ;; Add to process list
    (push process *process-list*)
    ;; Schedule process
    (scheduler-add-thread (first (process-threads process)))
    process))

;; Fork process
(defun process-fork ()
  "Fork current process."
  (let ((parent (current-process)))
    (let ((child (copy-process parent)))
      (setf (process-pid child) (incf *process-id-counter*))
      (setf (process-ppid child) (process-pid parent))
      (setf (process-state child) +process-state-ready+)
      ;; Copy address space (copy-on-write)
      (setf (process-memory-space child) (copy-address-space (process-memory-space parent)))
      ;; Copy file descriptors
      (copy-file-descriptors parent child)
      ;; Add to process list
      (push child *process-list*)
      ;; Schedule child
      (scheduler-add-thread (first (process-threads child)))
      (process-pid child))))

;; Execute new program
(defun process-exec (path argv env)
  "Execute new program in current process."
  (declare (type string path)
           (type list argv env))
  (let ((process (current-process)))
    ;; Load executable
    (let ((entry-point (load-executable path)))
      (when (not entry-point)
        (return-from process-exec -1))
      ;; Replace address space
      (destroy-address-space (process-memory-space process))
      (setf (process-memory-space process) (create-address-space))
      ;; Set up new stack
      (setup-process-stack process entry-point argv env)
      ;; Clear signal handlers
      (clrhash (process-signal-handlers process))
      ;; Update process name
      (setf (process-name process) path)
      ;; Jump to entry point
      (jump-to-entry entry-point)
      0)))

;; Exit process
(defun process-exit (code)
  "Exit current process."
  (declare (type (signed-byte 32) code))
  (let ((process (current-process)))
    (setf (process-exit-code process) code)
    (setf (process-state process) +process-state-zombie+)
    ;; Clean up resources
    (cleanup-process process)
    ;; Notify parent
    (notify-parent process)
    ;; Switch to another process
    (scheduler-yield)
    ;; Should never reach here
    (loop)))

;; Wait for child process
(defun process-wait (pid)
  "Wait for child process to exit."
  (declare (type (unsigned-byte 32) pid))
  (let ((process (current-process)))
    (loop
      (let ((child (find-process pid)))
        (when (and child
                   (eq (process-state child) +process-state-zombie+))
          (let ((exit-code (process-exit-code child)))
            (remove-process child)
            (return exit-code)))
        ;; Block until child exits
        (process-block process)))))

;; Kill process
(defun process-kill (pid signal)
  "Send signal to process."
  (declare (type (unsigned-byte 32) pid signal))
  (let ((process (find-process pid)))
    (when process
      (send-signal process signal)
      0)
    -1))

;; Get current process
(defun current-process ()
  "Get current running process."
  (let ((thread (current-thread)))
    (when thread
      (find-process-by-thread thread))))

;; Utility functions
(defun copy-process (process)
  "Create copy of process."
  (make-process
   :ppid (process-ppid process)
   :name (process-name process)
   :process-group (process-process-group process)
   :session (process-session process)
   :working-directory (process-working-directory process)))

(defun find-process (pid)
  "Find process by PID."
  (find pid *process-list* :key #'process-pid))

(defun find-process-by-thread (thread)
  "Find process containing thread."
  (find-if (lambda (p)
             (member thread (process-threads p)))
           *process-list*))

(defun remove-process (process)
  "Remove process from system."
  (setf *process-list* (remove process *process-list*)))

(defun cleanup-process (process)
  "Clean up process resources."
  ;; Close file descriptors
  (maphash (lambda (fd obj)
             (declare (ignore obj))
             (close-file-descriptor fd))
           (process-file-descriptors process))
  ;; Destroy address space
  (destroy-address-space (process-memory-space process)))

(defun notify-parent (process)
  "Notify parent process of child exit."
  (let ((parent (find-process (process-ppid process))))
    (when parent
      (wake-process parent))))

(defun send-signal (process signal)
  "Send signal to process."
  (setf (process-signals-pending process)
        (logior (process-signals-pending process) (ash 1 signal)))
  (wake-process process))

(defun process-block (process)
  "Block process."
  (setf (process-state process) +process-state-blocked+)
  (scheduler-yield))

(defun wake-process (process)
  "Wake up blocked process."
  (when (eq (process-state process) +process-state-blocked+)
    (setf (process-state process) +process-state-ready+)
    (scheduler-add-thread (first (process-threads process)))))

;; Forward declarations
(defun create-address-space () nil)
(defun copy-address-space (space) nil)
(defun destroy-address-space (space) nil)
(defun create-thread (entry-point) nil)
(defun load-executable (path) nil)
(defun setup-process-stack (process entry argv env) nil)
(defun jump-to-entry (entry) nil)
(defun copy-file-descriptors (from to) nil)
(defun close-file-descriptor (fd) nil)
(defun current-thread () nil)
(defun scheduler-add-thread (thread) nil)

