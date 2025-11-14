;; AstraLisp OS System Utilities
;; High-level system interface in Lisp

;; Process management
(defun ps ()
  "List all processes"
  (get-process-list))

(defun kill (pid)
  "Kill process by PID"
  (kernel-kill-process pid))

(defun sleep (seconds)
  "Sleep for seconds"
  (kernel-sleep (* seconds 1000)))

;; Memory management
(defun memory-info ()
  "Get memory information"
  (get-memory-info))

(defun gc ()
  "Run garbage collection"
  (kernel-gc))

(defun gc-stats ()
  "Get GC statistics"
  (kernel-gc-stats))

;; File system
(defun ls (path)
  "List directory"
  (list-directory path))

(defun cat (path)
  "Print file contents"
  (print (read-file path)))

(defun write-file (path data)
  "Write data to file"
  (kernel-write-file path data))

(defun rm (path)
  "Remove file"
  (kernel-remove-file path))

(defun mkdir (path)
  "Create directory"
  (kernel-mkdir path))

;; Network
(defun ping (host)
  "Ping host"
  (kernel-ping host))

(defun http-get (url)
  "HTTP GET request"
  (kernel-http-get url))

;; System information
(defun uname ()
  "Get system information"
  (list "AstraLisp OS" "PowerISA" (get-cpu-info)))

(defun uptime ()
  "Get system uptime"
  (kernel-get-uptime))

(defun load-average ()
  "Get load average"
  (kernel-get-load-average))

;; Kernel introspection
(defun inspect (object)
  "Inspect kernel object"
  (cond
    ((integerp object) (inspect-process object))
    ((symbolp object) (inspect-symbol object))
    (t (inspect-memory object))))

(defun kernel-stats ()
  "Get kernel statistics"
  (get-kernel-stats))

;; Device access
(defun read-register (device offset)
  "Read device register"
  (read-device device offset 4))

(defun write-register (device offset value)
  "Write device register"
  (write-device device offset value))

;; Interrupt handling
(defun on-interrupt (irq handler)
  "Register interrupt handler"
  (register-interrupt-handler irq handler))

(defun off-interrupt (irq)
  "Unregister interrupt handler"
  (unregister-interrupt-handler irq))

;; Live coding
(defun reload (module)
  "Reload module"
  (unload-module module)
  (load-module module))

(defun patch (function new-body)
  "Patch function at runtime"
  (hot-patch-function function new-body))

;; Concurrency
(defun parallel-map (func list)
  "Apply function to list in parallel"
  (let ((threads nil))
    (dolist (item list)
      (push (spawn-thread (lambda () (funcall func item))) threads))
    (mapcar #'thread-join (reverse threads))))

(defun with-lock (mutex body)
  "Execute body with mutex lock"
  (mutex-lock mutex)
  (unwind-protect
      (funcall body)
    (mutex-unlock mutex)))

;; Message passing
(defun send (process message)
  "Send message to process"
  (send-message process message))

(defun receive (&optional timeout)
  "Receive message"
  (receive-message (or timeout -1)))

;; Profiling
(defun profile (function)
  "Profile function execution"
  (profile-function function)
  (let ((result (funcall function)))
    (get-profile-results function)
    result))

;; Error handling
(defun error-handler (condition)
  "Default error handler"
  (print (list "Error:" condition))
  nil)

(defun with-error-handler (handler body)
  "Execute body with error handler"
  (try-catch body handler))

;; System configuration
(defun set-config (key value)
  "Set system configuration"
  (kernel-set-config key value))

(defun get-config (key)
  "Get system configuration"
  (kernel-get-config key))

;; Module system
(defun require (module)
  "Require module"
  (if (not (module-loaded-p module))
      (load-module module)))

(defun provide (module)
  "Provide module"
  (kernel-provide-module module))

(defun module-loaded-p (module)
  "Check if module is loaded"
  (kernel-module-loaded-p module))
