;; AstraLisp OS Kernel Signal Handling
;; Production POSIX signal implementation with real-time signals

(defpackage :astralisp-signal
  (:use :cl)
  (:export :signal-init
           :signal-send
           :signal-send-info
           :signal-kill
           :signal-set-handler
           :signal-get-handler
           :signal-mask
           :signal-pending
           :signal-suspend
           :signal-deliver-pending
           :signal-setup-frame
           :signal-return))

(in-package :astralisp-signal)

;;; Signal Number Constants (POSIX)

(defconstant +sighup+ 1 "Hangup")
(defconstant +sigint+ 2 "Interrupt")
(defconstant +sigquit+ 3 "Quit")
(defconstant +sigill+ 4 "Illegal instruction")
(defconstant +sigtrap+ 5 "Trace/breakpoint trap")
(defconstant +sigabrt+ 6 "Aborted")
(defconstant +sigbus+ 7 "Bus error")
(defconstant +sigfpe+ 8 "Floating point exception")
(defconstant +sigkill+ 9 "Killed")
(defconstant +sigusr1+ 10 "User defined signal 1")
(defconstant +sigsegv+ 11 "Segmentation fault")
(defconstant +sigusr2+ 12 "User defined signal 2")
(defconstant +sigpipe+ 13 "Broken pipe")
(defconstant +sigalrm+ 14 "Alarm clock")
(defconstant +sigterm+ 15 "Terminated")
(defconstant +sigstkflt+ 16 "Stack fault")
(defconstant +sigchld+ 17 "Child exited")
(defconstant +sigcont+ 18 "Continued")
(defconstant +sigstop+ 19 "Stopped (signal)")
(defconstant +sigtstp+ 20 "Stopped")
(defconstant +sigttin+ 21 "Stopped (tty input)")
(defconstant +sigttou+ 22 "Stopped (tty output)")
(defconstant +sigurg+ 23 "Urgent I/O condition")
(defconstant +sigxcpu+ 24 "CPU time limit exceeded")
(defconstant +sigxfsz+ 25 "File size limit exceeded")
(defconstant +sigvtalrm+ 26 "Virtual timer expired")
(defconstant +sigprof+ 27 "Profiling timer expired")
(defconstant +sigwinch+ 28 "Window changed")
(defconstant +sigio+ 29 "I/O possible")
(defconstant +sigpwr+ 30 "Power failure")
(defconstant +sigsys+ 31 "Bad system call")

;; Real-time signals
(defconstant +sigrtmin+ 32 "First real-time signal")
(defconstant +sigrtmax+ 64 "Last real-time signal")

(defconstant +nsig+ 65 "Number of signals")

;;; Signal Handler Constants

(defconstant +sig-dfl+ 0 "Default signal handler")
(defconstant +sig-ign+ 1 "Ignore signal")

;;; Signal Action Flags

(defconstant +sa-nocldstop+ #x00000001 "Don't notify on child stop")
(defconstant +sa-nocldwait+ #x00000002 "Don't create zombie on child death")
(defconstant +sa-siginfo+ #x00000004 "Use sa_sigaction handler")
(defconstant +sa-onstack+ #x08000000 "Use signal stack")
(defconstant +sa-restart+ #x10000000 "Restart syscalls if possible")
(defconstant +sa-nodefer+ #x40000000 "Don't automatically block signal")
(defconstant +sa-resethand+ #x80000000 "Reset to SIG_DFL on entry")

;;; Signal Masking Operations

(defconstant +sig-block+ 0 "Block signals")
(defconstant +sig-unblock+ 1 "Unblock signals")
(defconstant +sig-setmask+ 2 "Set signal mask")

;;; Signal Info Codes

(defconstant +si-user+ 0 "Sent by kill()")
(defconstant +si-kernel+ 128 "Sent by kernel")
(defconstant +si-queue+ -1 "Sent by sigqueue()")
(defconstant +si-timer+ -2 "Timer expired")
(defconstant +si-mesgq+ -3 "Message queue state changed")
(defconstant +si-asyncio+ -4 "AIO completed")
(defconstant +si-sigio+ -5 "Queued SIGIO")
(defconstant +si-tkill+ -6 "Sent by tkill()")

;;; Data Structures

(defstruct siginfo
  "Signal information structure."
  (signo 0 :type (unsigned-byte 8))
  (errno 0 :type (signed-byte 32))
  (code 0 :type (signed-byte 32))
  (pid 0 :type (unsigned-byte 32))
  (uid 0 :type (unsigned-byte 32))
  (status 0 :type (signed-byte 32))
  (addr 0 :type (unsigned-byte 64))
  (value 0 :type (unsigned-byte 64))
  (fd 0 :type (signed-byte 32))
  (band 0 :type (signed-byte 64)))

(defstruct sigaction
  "Signal action structure."
  (handler 0 :type (unsigned-byte 64))      ; Handler address or SIG_DFL/SIG_IGN
  (flags 0 :type (unsigned-byte 32))
  (restorer 0 :type (unsigned-byte 64))     ; Signal return trampoline
  (mask 0 :type (unsigned-byte 64)))        ; Signals to block during handler

(defstruct signal-queue-entry
  "Queued signal entry."
  (signo 0 :type (unsigned-byte 8))
  (info nil :type (or null siginfo))
  (next nil :type (or null signal-queue-entry)))

(defstruct signal-frame
  "Signal stack frame for user space handler."
  (pretcode 0 :type (unsigned-byte 64))     ; Return trampoline address
  (signo 0 :type (unsigned-byte 32))
  (handler 0 :type (unsigned-byte 64))
  (context-addr 0 :type (unsigned-byte 64)) ; Saved context
  (info-addr 0 :type (unsigned-byte 64)))   ; siginfo address

(defstruct process-signal-state
  "Per-process signal state."
  (handlers (make-array +nsig+ :initial-element nil) :type (vector t))
  (pending-mask 0 :type (unsigned-byte 64))
  (blocked-mask 0 :type (unsigned-byte 64))
  (real-time-queue nil :type list)          ; Queue for real-time signals
  (standard-pending (make-array +nsig+ :initial-element nil) :type (vector t))
  (shared-pending nil :type list))           ; Process-wide pending signals

(defstruct thread-signal-state
  "Per-thread signal state."
  (blocked-mask 0 :type (unsigned-byte 64))
  (pending-mask 0 :type (unsigned-byte 64))
  (real-time-queue nil :type list)
  (in-signal-handler nil :type boolean)
  (sigaltstack-addr 0 :type (unsigned-byte 64))
  (sigaltstack-size 0 :type (unsigned-byte 64))
  (sigaltstack-flags 0 :type (unsigned-byte 32)))

;;; Global State

(defvar *signal-initialized* nil)
(defvar *default-actions* (make-hash-table) "Default signal actions")

;;; Signal Properties

(defun signal-is-realtime (signo)
  "Check if signal is a real-time signal."
  (declare (type (unsigned-byte 8) signo))
  (and (>= signo +sigrtmin+) (<= signo +sigrtmax+)))

(defun signal-is-fatal (signo)
  "Check if signal causes process termination by default."
  (declare (type (unsigned-byte 8) signo))
  (member signo (list +sigkill+ +sigterm+ +sigabrt+ +sigquit+
                      +sigill+ +sigbus+ +sigfpe+ +sigsegv+
                      +sigpipe+ +sigalrm+ +sigusr1+ +sigusr2+
                      +sigstkflt+ +sigxcpu+ +sigxfsz+ +sigsys+)))

(defun signal-cannot-be-caught (signo)
  "Check if signal cannot be caught or ignored."
  (declare (type (unsigned-byte 8) signo))
  (or (= signo +sigkill+) (= signo +sigstop+)))

(defun signal-stops-process (signo)
  "Check if signal stops process by default."
  (declare (type (unsigned-byte 8) signo))
  (member signo (list +sigstop+ +sigtstp+ +sigttin+ +sigttou+)))

(defun signal-continues-process (signo)
  "Check if signal continues stopped process."
  (declare (type (unsigned-byte 8) signo))
  (= signo +sigcont+))

;;; Initialization

(defun signal-init ()
  "Initialize signal handling system."
  (when *signal-initialized*
    (return-from signal-init t))

  ;; Initialize default actions
  (setup-default-actions)

  (setf *signal-initialized* t)
  t)

(defun setup-default-actions ()
  "Setup default signal actions."
  ;; Terminal signals
  (dolist (sig (list +sighup+ +sigint+ +sigquit+ +sigterm+ +sigusr1+ +sigusr2+))
    (setf (gethash sig *default-actions*) :terminate))

  ;; Fatal signals with core dump
  (dolist (sig (list +sigill+ +sigabrt+ +sigfpe+ +sigsegv+ +sigbus+
                     +sigtrap+ +sigsys+ +sigxcpu+ +sigxfsz+))
    (setf (gethash sig *default-actions*) :core))

  ;; Stop signals
  (dolist (sig (list +sigstop+ +sigtstp+ +sigttin+ +sigttou+))
    (setf (gethash sig *default-actions*) :stop))

  ;; Continue signal
  (setf (gethash +sigcont+ *default-actions*) :continue)

  ;; Ignored signals
  (dolist (sig (list +sigchld+ +sigwinch+ +sigurg+))
    (setf (gethash sig *default-actions*) :ignore)))

;;; Signal Sending

(defun signal-send (pid signo)
  "Send signal to process."
  (declare (type (unsigned-byte 32) pid)
           (type (unsigned-byte 8) signo))

  (let ((info (make-siginfo
               :signo signo
               :errno 0
               :code +si-user+
               :pid (get-current-pid)
               :uid (get-current-uid))))
    (signal-send-info pid info)))

(defun signal-send-info (pid info)
  "Send signal with full siginfo to process."
  (declare (type (unsigned-byte 32) pid)
           (type siginfo info))

  ;; Validate signal number
  (let ((signo (siginfo-signo info)))
    (when (or (< signo 1) (>= signo +nsig+))
      (return-from signal-send-info nil))

    ;; Get target process
    (let ((process (get-process pid)))
      (unless process
        (return-from signal-send-info nil))

      ;; Get process signal state
      (let ((sig-state (get-process-signal-state process)))

        ;; If signal is blocked by all threads, add to shared pending
        (if (signal-is-blocked-by-all-threads process signo)
            (add-to-shared-pending sig-state info)
            ;; Otherwise, pick a thread that can receive it
            (let ((target-thread (find-thread-for-signal process signo)))
              (if target-thread
                  (send-signal-to-thread target-thread info)
                  (add-to-shared-pending sig-state info))))

        t))))

(defun signal-kill (pid signo)
  "Send signal to process (wrapper for signal-send)."
  (declare (type (signed-byte 32) pid)
           (type (unsigned-byte 8) signo))

  (cond
    ((> pid 0)
     ;; Send to specific process
     (signal-send pid signo))

    ((= pid 0)
     ;; Send to process group
     (signal-send-to-group (get-current-pgid) signo))

    ((= pid -1)
     ;; Send to all processes except init
     (signal-send-to-all signo))

    ((< pid -1)
     ;; Send to process group |pid|
     (signal-send-to-group (- pid) signo))

    (t nil)))

(defun signal-send-to-thread (thread info)
  "Send signal to specific thread."
  (declare (type t thread)
           (type siginfo info))

  (let ((thread-sig-state (get-thread-signal-state thread))
        (signo (siginfo-signo info)))

    ;; Add to thread's pending signals
    (if (signal-is-realtime signo)
        ;; Real-time signals are queued
        (push (make-signal-queue-entry :signo signo :info info)
              (thread-signal-state-real-time-queue thread-sig-state))
        ;; Standard signals are set in pending mask
        (setf (thread-signal-state-pending-mask thread-sig-state)
              (logior (thread-signal-state-pending-mask thread-sig-state)
                      (ash 1 signo))))

    ;; Wake thread if blocked
    (when (thread-is-blocked thread)
      (thread-wake (thread-id thread)))

    t))

(defun add-to-shared-pending (sig-state info)
  "Add signal to process shared pending queue."
  (declare (type process-signal-state sig-state)
           (type siginfo info))

  (let ((signo (siginfo-signo info)))
    (if (signal-is-realtime signo)
        (push (make-signal-queue-entry :signo signo :info info)
              (process-signal-state-real-time-queue sig-state))
        (setf (process-signal-state-pending-mask sig-state)
              (logior (process-signal-state-pending-mask sig-state)
                      (ash 1 signo))))))

;;; Signal Masking

(defun signal-mask (how set &optional oldset)
  "Change signal mask for current thread."
  (declare (type (unsigned-byte 8) how)
           (type (unsigned-byte 64) set))

  (let* ((thread (get-current-thread))
         (thread-sig-state (get-thread-signal-state thread))
         (old-mask (thread-signal-state-blocked-mask thread-sig-state)))

    ;; Return old mask if requested
    (when oldset
      (setf oldset old-mask))

    ;; Cannot block SIGKILL or SIGSTOP
    (let ((filtered-set (logand set (lognot (logior (ash 1 +sigkill+)
                                                     (ash 1 +sigstop+))))))
      (case how
        (#.+sig-block+
         (setf (thread-signal-state-blocked-mask thread-sig-state)
               (logior old-mask filtered-set)))

        (#.+sig-unblock+
         (setf (thread-signal-state-blocked-mask thread-sig-state)
               (logand old-mask (lognot filtered-set))))

        (#.+sig-setmask+
         (setf (thread-signal-state-blocked-mask thread-sig-state)
               filtered-set))

        (t
         (return-from signal-mask nil))))

    old-mask))

(defun signal-pending (set)
  "Get pending signals for current thread."
  (declare (type (unsigned-byte 64) set))

  (let* ((thread (get-current-thread))
         (thread-sig-state (get-thread-signal-state thread))
         (process (get-current-process))
         (process-sig-state (get-process-signal-state process)))

    ;; Combine thread and process pending signals
    (setf set (logior (thread-signal-state-pending-mask thread-sig-state)
                     (process-signal-state-pending-mask process-sig-state)))

    set))

(defun signal-suspend (mask)
  "Replace signal mask and wait for signal."
  (declare (type (unsigned-byte 64) mask))

  (let* ((thread (get-current-thread))
         (thread-sig-state (get-thread-signal-state thread))
         (old-mask (thread-signal-state-blocked-mask thread-sig-state)))

    ;; Set new mask
    (setf (thread-signal-state-blocked-mask thread-sig-state) mask)

    ;; Block until signal arrives
    (thread-block (thread-id thread))

    ;; Restore old mask
    (setf (thread-signal-state-blocked-mask thread-sig-state) old-mask)

    t))

;;; Signal Handler Management

(defun signal-set-handler (signo action)
  "Set signal handler."
  (declare (type (unsigned-byte 8) signo)
           (type sigaction action))

  ;; Cannot catch SIGKILL or SIGSTOP
  (when (signal-cannot-be-caught signo)
    (return-from signal-set-handler nil))

  (let* ((process (get-current-process))
         (sig-state (get-process-signal-state process)))

    (setf (aref (process-signal-state-handlers sig-state) signo) action)
    t))

(defun signal-get-handler (signo)
  "Get signal handler."
  (declare (type (unsigned-byte 8) signo))

  (let* ((process (get-current-process))
         (sig-state (get-process-signal-state process)))

    (aref (process-signal-state-handlers sig-state) signo)))

;;; Signal Delivery

(defun signal-deliver-pending ()
  "Deliver pending signals to current thread."
  (let* ((thread (get-current-thread))
         (thread-sig-state (get-thread-signal-state thread))
         (process (get-current-process))
         (process-sig-state (get-process-signal-state process)))

    ;; Don't deliver signals if already in handler
    (when (thread-signal-state-in-signal-handler thread-sig-state)
      (return-from signal-deliver-pending nil))

    ;; Find highest priority pending unblocked signal
    (let ((signo (find-next-signal thread-sig-state process-sig-state)))
      (when signo
        (deliver-signal thread signo)
        t))))

(defun find-next-signal (thread-sig-state process-sig-state)
  "Find next pending unblocked signal."
  (declare (type thread-signal-state thread-sig-state)
           (type process-signal-state process-sig-state))

  (let ((blocked (thread-signal-state-blocked-mask thread-sig-state))
        (thread-pending (thread-signal-state-pending-mask thread-sig-state))
        (process-pending (process-signal-state-pending-mask process-sig-state)))

    ;; Combine pending masks
    (let ((all-pending (logior thread-pending process-pending)))
      ;; Mask out blocked signals
      (let ((deliverable (logand all-pending (lognot blocked))))

        ;; Find highest priority signal (lowest number)
        ;; SIGKILL and SIGSTOP have highest priority
        (cond
          ((logbitp +sigkill+ deliverable) +sigkill+)
          ((logbitp +sigstop+ deliverable) +sigstop+)
          (t
           ;; Find first set bit
           (dotimes (i +nsig+)
             (when (logbitp i deliverable)
               (return i)))
           nil))))))

(defun deliver-signal (thread signo)
  "Deliver signal to thread."
  (declare (type t thread)
           (type (unsigned-byte 8) signo))

  (let* ((process (get-thread-process thread))
         (process-sig-state (get-process-signal-state process))
         (thread-sig-state (get-thread-signal-state thread))
         (action (aref (process-signal-state-handlers process-sig-state) signo)))

    ;; Clear from pending mask
    (setf (thread-signal-state-pending-mask thread-sig-state)
          (logand (thread-signal-state-pending-mask thread-sig-state)
                  (lognot (ash 1 signo))))

    (setf (process-signal-state-pending-mask process-sig-state)
          (logand (process-signal-state-pending-mask process-sig-state)
                  (lognot (ash 1 signo))))

    (cond
      ;; No handler or SIG_DFL - perform default action
      ((or (null action) (= (sigaction-handler action) +sig-dfl+))
       (perform-default-action thread signo))

      ;; SIG_IGN - ignore signal
      ((= (sigaction-handler action) +sig-ign+)
       nil)

      ;; User handler - setup signal frame
      (t
       (setup-signal-frame thread signo action)))))

(defun perform-default-action (thread signo)
  "Perform default action for signal."
  (declare (type t thread)
           (type (unsigned-byte 8) signo))

  (let ((default-action (gethash signo *default-actions* :terminate)))
    (case default-action
      (:terminate
       (process-exit (logior 128 signo)))

      (:core
       (generate-core-dump thread)
       (process-exit (logior 128 signo)))

      (:stop
       (process-stop (get-thread-process thread)))

      (:continue
       (process-continue (get-thread-process thread)))

      (:ignore
       nil)

      (t
       (process-exit (logior 128 signo))))))

(defun setup-signal-frame (thread signo action)
  "Setup signal stack frame for user handler."
  (declare (type t thread)
           (type (unsigned-byte 8) signo)
           (type sigaction action))

  (let* ((thread-sig-state (get-thread-signal-state thread))
         (use-altstack (and (logtest (sigaction-flags action) +sa-onstack+)
                           (not (zerop (thread-signal-state-sigaltstack-addr thread-sig-state)))))
         (stack-addr (if use-altstack
                        (thread-signal-state-sigaltstack-addr thread-sig-state)
                        (get-thread-stack-pointer thread))))

    ;; Save current context
    (let ((context-addr (save-signal-context thread stack-addr)))

      ;; Create signal frame on stack
      (let ((frame-addr (- stack-addr 256)))  ; Reserve 256 bytes for frame

        ;; Write signal frame
        (write-signal-frame frame-addr signo action context-addr)

        ;; Update thread state
        (setf (thread-signal-state-in-signal-handler thread-sig-state) t)

        ;; Set PC to handler and SP to frame
        (set-thread-pc thread (sigaction-handler action))
        (set-thread-sp thread frame-addr)

        ;; Set up arguments (r3 = signo, r4 = siginfo, r5 = context)
        (set-thread-register thread 3 signo)
        (set-thread-register thread 4 0)  ; siginfo (if SA_SIGINFO)
        (set-thread-register thread 5 context-addr)

        ;; Block signals during handler if requested
        (unless (logtest (sigaction-flags action) +sa-nodefer+)
          (signal-mask +sig-block+ (ash 1 signo)))

        ;; Block additional signals from action mask
        (signal-mask +sig-block+ (sigaction-mask action))))))

(defun signal-return (context-addr)
  "Return from signal handler."
  (declare (type (unsigned-byte 64) context-addr))

  (let* ((thread (get-current-thread))
         (thread-sig-state (get-thread-signal-state thread)))

    ;; Restore context
    (restore-signal-context thread context-addr)

    ;; Mark no longer in signal handler
    (setf (thread-signal-state-in-signal-handler thread-sig-state) nil)

    t))

;;; Helper Functions

(defun signal-is-blocked-by-all-threads (process signo)
  "Check if signal is blocked by all threads in process."
  (declare (type t process)
           (type (unsigned-byte 8) signo))
  ;; Check all threads
  (dolist (thread (get-process-threads process))
    (let ((thread-sig-state (get-thread-signal-state thread)))
      (unless (logbitp signo (thread-signal-state-blocked-mask thread-sig-state))
        (return-from signal-is-blocked-by-all-threads nil))))
  t)

(defun find-thread-for-signal (process signo)
  "Find thread that can receive signal."
  (declare (type t process)
           (type (unsigned-byte 8) signo))
  ;; Return first thread that doesn't block the signal
  (dolist (thread (get-process-threads process))
    (let ((thread-sig-state (get-thread-signal-state thread)))
      (unless (logbitp signo (thread-signal-state-blocked-mask thread-sig-state))
        (return thread))))
  nil)

(defun signal-send-to-group (pgid signo)
  "Send signal to process group."
  (declare (type (unsigned-byte 32) pgid signo))
  (dolist (process (get-process-group-members pgid))
    (signal-send (get-process-pid process) signo))
  t)

(defun signal-send-to-all (signo)
  "Send signal to all processes."
  (declare (type (unsigned-byte 8) signo))
  (dolist (process (get-all-processes))
    (unless (= (get-process-pid process) 1)  ; Don't signal init
      (signal-send (get-process-pid process) signo)))
  t)

;;; Forward Declarations

(defun get-current-process () nil)
(defun get-current-thread () nil)
(defun get-current-pid () 1)
(defun get-current-uid () 0)
(defun get-current-pgid () 1)
(defun get-process (pid) (declare (ignore pid)) nil)
(defun get-process-pid (process) (declare (ignore process)) 1)
(defun get-process-threads (process) (declare (ignore process)) nil)
(defun get-thread-process (thread) (declare (ignore thread)) nil)
(defun get-thread-id (thread) (declare (ignore thread)) 1)
(defun get-all-processes () nil)
(defun get-process-group-members (pgid) (declare (ignore pgid)) nil)

(defun get-process-signal-state (process)
  (declare (ignore process))
  (make-process-signal-state))

(defun get-thread-signal-state (thread)
  (declare (ignore thread))
  (make-thread-signal-state))

(defun thread-is-blocked (thread) (declare (ignore thread)) nil)
(defun thread-wake (thread-id) (declare (ignore thread-id)) nil)
(defun thread-block (thread-id) (declare (ignore thread-id)) nil)
(defun thread-id (thread) (declare (ignore thread)) 1)

(defun process-exit (status) (declare (ignore status)) nil)
(defun process-stop (process) (declare (ignore process)) nil)
(defun process-continue (process) (declare (ignore process)) nil)
(defun generate-core-dump (thread) (declare (ignore thread)) nil)

(defun get-thread-stack-pointer (thread) (declare (ignore thread)) 0)
(defun set-thread-pc (thread pc) (declare (ignore thread pc)) nil)
(defun set-thread-sp (thread sp) (declare (ignore thread sp)) nil)
(defun set-thread-register (thread reg val) (declare (ignore thread reg val)) nil)

(defun save-signal-context (thread stack-addr)
  (declare (ignore thread stack-addr))
  0)

(defun restore-signal-context (thread context-addr)
  (declare (ignore thread context-addr))
  nil)

(defun write-signal-frame (frame-addr signo action context-addr)
  (declare (ignore frame-addr signo action context-addr))
  nil)

