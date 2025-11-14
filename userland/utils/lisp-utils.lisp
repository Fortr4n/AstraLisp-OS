;; AstraLisp OS Lisp Utilities
;; Core system utilities written in Lisp

;; List utilities
(defun mapcar (func list)
  "Apply function to each element of list"
  (if (null list)
      nil
      (cons (funcall func (car list))
            (mapcar func (cdr list)))))

(defun filter (pred list)
  "Filter list by predicate"
  (if (null list)
      nil
      (if (funcall pred (car list))
          (cons (car list) (filter pred (cdr list)))
          (filter pred (cdr list)))))

(defun reduce (func init list)
  "Reduce list with function"
  (if (null list)
      init
      (reduce func (funcall func init (car list)) (cdr list))))

(defun reverse (list)
  "Reverse list"
  (let ((result nil))
    (dolist (item list)
      (setq result (cons item result)))
    result))

(defun append (list1 list2)
  "Append two lists"
  (if (null list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(defun length (list)
  "Get length of list"
  (if (null list)
      0
      (+ 1 (length (cdr list)))))

(defun nth (n list)
  "Get nth element of list"
  (if (= n 0)
      (car list)
      (nth (- n 1) (cdr list))))

;; String utilities
(defun string-append (&rest strings)
  "Concatenate strings"
  (reduce #'concat strings))

(defun string-split (string delimiter)
  "Split string by delimiter"
  (let ((result nil)
        (current nil))
    (dolist (char (string-to-list string))
      (if (eq char delimiter)
          (progn
            (setq result (cons (list-to-string (reverse current)) result))
            (setq current nil))
          (setq current (cons char current))))
    (if current
        (setq result (cons (list-to-string (reverse current)) result)))
    (reverse result)))

;; Math utilities
(defun factorial (n)
  "Calculate factorial"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun fibonacci (n)
  "Calculate fibonacci number"
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(defun gcd (a b)
  "Calculate greatest common divisor"
  (if (= b 0)
      a
      (gcd b (% a b))))

;; System utilities
(defun get-process-list ()
  "Get list of all processes"
  (kernel-get-processes))

(defun get-memory-info ()
  "Get memory information"
  (kernel-get-memory-info))

(defun get-cpu-info ()
  "Get CPU information"
  (kernel-get-cpu-info))

;; File system utilities
(defun file-exists-p (path)
  "Check if file exists"
  (kernel-file-exists path))

(defun read-file (path)
  "Read file contents"
  (kernel-read-file path))

(defun write-file (path data)
  "Write data to file"
  (kernel-write-file path data))

(defun list-directory (path)
  "List directory contents"
  (kernel-list-directory path))

;; Network utilities
(defun tcp-connect (host port)
  "Connect to TCP host"
  (kernel-tcp-connect host port))

(defun tcp-send (socket data)
  "Send data over TCP socket"
  (kernel-tcp-send socket data))

(defun tcp-receive (socket)
  "Receive data from TCP socket"
  (kernel-tcp-receive socket))

;; Kernel introspection
(defun inspect-process (pid)
  "Inspect process by PID"
  (kernel-inspect-process pid))

(defun inspect-thread (tid)
  "Inspect thread by TID"
  (kernel-inspect-thread tid))

(defun inspect-memory (addr)
  "Inspect memory at address"
  (kernel-inspect-memory addr))

(defun get-kernel-stats ()
  "Get kernel statistics"
  (kernel-get-stats))

;; Live code injection
(defun hot-patch-function (name new-body)
  "Hot-patch a function"
  (kernel-hot-patch name new-body))

(defun load-module (path)
  "Load Lisp module"
  (kernel-load-module path))

(defun unload-module (name)
  "Unload Lisp module"
  (kernel-unload-module name))

;; Profiling
(defun profile-function (name)
  "Start profiling function"
  (kernel-profile-start name))

(defun get-profile-results (name)
  "Get profiling results"
  (kernel-profile-get-results name))

;; Error handling
(defun try-catch (try-expr catch-expr)
  "Try-catch error handling"
  (let ((result (kernel-try try-expr)))
    (if (error-p result)
        (funcall catch-expr result)
        result)))

;; Concurrency
(defun spawn-thread (func)
  "Spawn new thread"
  (kernel-spawn-thread func))

(defun thread-join (thread)
  "Join thread"
  (kernel-thread-join thread))

(defun mutex-create ()
  "Create mutex"
  (kernel-mutex-create))

(defun mutex-lock (mutex)
  "Lock mutex"
  (kernel-mutex-lock mutex))

(defun mutex-unlock (mutex)
  "Unlock mutex"
  (kernel-mutex-unlock mutex))

;; Message passing
(defun send-message (process message)
  "Send message to process"
  (kernel-send-message process message))

(defun receive-message (timeout)
  "Receive message"
  (kernel-receive-message timeout))

;; Device access
(defun read-device (device offset size)
  "Read from device"
  (kernel-read-device device offset size))

(defun write-device (device offset data)
  "Write to device"
  (kernel-write-device device offset data))

;; Interrupt handling
(defun register-interrupt-handler (irq handler)
  "Register interrupt handler"
  (kernel-register-interrupt irq handler))

(defun unregister-interrupt-handler (irq)
  "Unregister interrupt handler"
  (kernel-unregister-interrupt irq))
