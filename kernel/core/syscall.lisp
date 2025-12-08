;; AstraLisp OS Kernel System Call Dispatcher
;; Production system call handler with comprehensive POSIX syscall support

(defpackage :astralisp-syscall
  (:use :cl)
  (:export :syscall-init
           :syscall-dispatch
           :syscall-register
           :syscall-trace-enable
           :syscall-trace-disable))

(in-package :astralisp-syscall)

;;; System Call Numbers (Linux-compatible for PowerPC64)

;; Process Management
(defconstant +sys-exit+ 1)
(defconstant +sys-fork+ 2)
(defconstant +sys-read+ 3)
(defconstant +sys-write+ 4)
(defconstant +sys-open+ 5)
(defconstant +sys-close+ 6)
(defconstant +sys-waitpid+ 7)
(defconstant +sys-creat+ 8)
(defconstant +sys-link+ 9)
(defconstant +sys-unlink+ 10)
(defconstant +sys-execve+ 11)
(defconstant +sys-chdir+ 12)
(defconstant +sys-time+ 13)
(defconstant +sys-mknod+ 14)
(defconstant +sys-chmod+ 15)
(defconstant +sys-lchown+ 16)
(defconstant +sys-stat+ 18)
(defconstant +sys-lseek+ 19)
(defconstant +sys-getpid+ 20)
(defconstant +sys-mount+ 21)
(defconstant +sys-umount+ 22)
(defconstant +sys-setuid+ 23)
(defconstant +sys-getuid+ 24)
(defconstant +sys-stime+ 25)
(defconstant +sys-ptrace+ 26)
(defconstant +sys-alarm+ 27)
(defconstant +sys-pause+ 29)
(defconstant +sys-utime+ 30)
(defconstant +sys-access+ 33)
(defconstant +sys-sync+ 36)
(defconstant +sys-kill+ 37)
(defconstant +sys-rename+ 38)
(defconstant +sys-mkdir+ 39)
(defconstant +sys-rmdir+ 40)
(defconstant +sys-dup+ 41)
(defconstant +sys-pipe+ 42)
(defconstant +sys-times+ 43)
(defconstant +sys-brk+ 45)
(defconstant +sys-setgid+ 46)
(defconstant +sys-getgid+ 47)
(defconstant +sys-signal+ 48)
(defconstant +sys-geteuid+ 49)
(defconstant +sys-getegid+ 50)
(defconstant +sys-acct+ 51)
(defconstant +sys-umount2+ 52)
(defconstant +sys-ioctl+ 54)
(defconstant +sys-fcntl+ 55)
(defconstant +sys-setpgid+ 57)
(defconstant +sys-umask+ 60)
(defconstant +sys-chroot+ 61)
(defconstant +sys-ustat+ 62)
(defconstant +sys-dup2+ 63)
(defconstant +sys-getppid+ 64)
(defconstant +sys-getpgrp+ 65)
(defconstant +sys-setsid+ 66)
(defconstant +sys-sigaction+ 67)
(defconstant +sys-setreuid+ 70)
(defconstant +sys-setregid+ 71)
(defconstant +sys-sigsuspend+ 72)
(defconstant +sys-sigpending+ 73)
(defconstant +sys-sethostname+ 74)
(defconstant +sys-setrlimit+ 75)
(defconstant +sys-getrlimit+ 76)
(defconstant +sys-getrusage+ 77)
(defconstant +sys-gettimeofday+ 78)
(defconstant +sys-settimeofday+ 79)
(defconstant +sys-getgroups+ 80)
(defconstant +sys-setgroups+ 81)
(defconstant +sys-symlink+ 83)
(defconstant +sys-readlink+ 85)
(defconstant +sys-uselib+ 86)
(defconstant +sys-swapon+ 87)
(defconstant +sys-reboot+ 88)
(defconstant +sys-readdir+ 89)
(defconstant +sys-mmap+ 90)
(defconstant +sys-munmap+ 91)
(defconstant +sys-truncate+ 92)
(defconstant +sys-ftruncate+ 93)
(defconstant +sys-fchmod+ 94)
(defconstant +sys-fchown+ 95)
(defconstant +sys-getpriority+ 96)
(defconstant +sys-setpriority+ 97)
(defconstant +sys-statfs+ 99)
(defconstant +sys-fstatfs+ 100)
(defconstant +sys-socketcall+ 102)
(defconstant +sys-syslog+ 103)
(defconstant +sys-setitimer+ 104)
(defconstant +sys-getitimer+ 105)
(defconstant +sys-fstat+ 108)
(defconstant +sys-vhangup+ 111)
(defconstant +sys-wait4+ 114)
(defconstant +sys-swapoff+ 115)
(defconstant +sys-sysinfo+ 116)
(defconstant +sys-ipc+ 117)
(defconstant +sys-fsync+ 118)
(defconstant +sys-sigreturn+ 119)
(defconstant +sys-clone+ 120)
(defconstant +sys-setdomainname+ 121)
(defconstant +sys-uname+ 122)
(defconstant +sys-adjtimex+ 124)
(defconstant +sys-mprotect+ 125)
(defconstant +sys-sigprocmask+ 126)
(defconstant +sys-init-module+ 128)
(defconstant +sys-delete-module+ 129)
(defconstant +sys-quotactl+ 131)
(defconstant +sys-getpgid+ 132)
(defconstant +sys-fchdir+ 133)
(defconstant +sys-bdflush+ 134)
(defconstant +sys-sysfs+ 135)
(defconstant +sys-personality+ 136)
(defconstant +sys-setfsuid+ 138)
(defconstant +sys-setfsgid+ 139)
(defconstant +sys-llseek+ 140)
(defconstant +sys-getdents+ 141)
(defconstant +sys-select+ 142)
(defconstant +sys-flock+ 143)
(defconstant +sys-msync+ 144)
(defconstant +sys-readv+ 145)
(defconstant +sys-writev+ 146)
(defconstant +sys-getsid+ 147)
(defconstant +sys-fdatasync+ 148)
(defconstant +sys-sysctl+ 149)
(defconstant +sys-mlock+ 150)
(defconstant +sys-munlock+ 151)
(defconstant +sys-mlockall+ 152)
(defconstant +sys-munlockall+ 153)
(defconstant +sys-sched-setparam+ 154)
(defconstant +sys-sched-getparam+ 155)
(defconstant +sys-sched-setscheduler+ 156)
(defconstant +sys-sched-getscheduler+ 157)
(defconstant +sys-sched-yield+ 158)
(defconstant +sys-sched-get-priority-max+ 159)
(defconstant +sys-sched-get-priority-min+ 160)
(defconstant +sys-sched-rr-get-interval+ 161)
(defconstant +sys-nanosleep+ 162)
(defconstant +sys-mremap+ 163)
(defconstant +sys-setresuid+ 164)
(defconstant +sys-getresuid+ 165)
(defconstant +sys-poll+ 167)
(defconstant +sys-nfsservctl+ 168)
(defconstant +sys-setresgid+ 169)
(defconstant +sys-getresgid+ 170)
(defconstant +sys-prctl+ 171)
(defconstant +sys-rt-sigreturn+ 172)
(defconstant +sys-rt-sigaction+ 173)
(defconstant +sys-rt-sigprocmask+ 174)
(defconstant +sys-rt-sigpending+ 175)
(defconstant +sys-rt-sigtimedwait+ 176)
(defconstant +sys-rt-sigqueueinfo+ 177)
(defconstant +sys-rt-sigsuspend+ 178)
(defconstant +sys-pread64+ 179)
(defconstant +sys-pwrite64+ 180)
(defconstant +sys-chown+ 181)
(defconstant +sys-getcwd+ 182)
(defconstant +sys-capget+ 183)
(defconstant +sys-capset+ 184)
(defconstant +sys-sigaltstack+ 185)
(defconstant +sys-sendfile+ 186)
(defconstant +sys-vfork+ 189)
(defconstant +sys-getrlimit+ 190)
(defconstant +sys-readahead+ 191)

;; Modern syscalls
(defconstant +sys-socket+ 198)
(defconstant +sys-bind+ 200)
(defconstant +sys-connect+ 201)
(defconstant +sys-listen+ 202)
(defconstant +sys-accept+ 203)
(defconstant +sys-getsockname+ 204)
(defconstant +sys-getpeername+ 205)
(defconstant +sys-socketpair+ 206)
(defconstant +sys-send+ 207)
(defconstant +sys-recv+ 208)
(defconstant +sys-sendto+ 209)
(defconstant +sys-recvfrom+ 210)
(defconstant +sys-shutdown+ 211)
(defconstant +sys-setsockopt+ 212)
(defconstant +sys-getsockopt+ 213)
(defconstant +sys-sendmsg+ 214)
(defconstant +sys-recvmsg+ 215)
(defconstant +sys-futex+ 221)
(defconstant +sys-sched-setaffinity+ 222)
(defconstant +sys-sched-getaffinity+ 223)
(defconstant +sys-set-thread-area+ 229)
(defconstant +sys-get-thread-area+ 230)
(defconstant +sys-exit-group+ 234)
(defconstant +sys-epoll-create+ 236)
(defconstant +sys-epoll-ctl+ 237)
(defconstant +sys-epoll-wait+ 238)
(defconstant +sys-set-tid-address+ 252)
(defconstant +sys-clock-gettime+ 246)
(defconstant +sys-clock-settime+ 247)
(defconstant +sys-clock-getres+ 248)
(defconstant +sys-clock-nanosleep+ 249)

;;; Error Codes (errno)

(defconstant +eperm+ 1 "Operation not permitted")
(defconstant +enoent+ 2 "No such file or directory")
(defconstant +esrch+ 3 "No such process")
(defconstant +eintr+ 4 "Interrupted system call")
(defconstant +eio+ 5 "I/O error")
(defconstant +enxio+ 6 "No such device or address")
(defconstant +e2big+ 7 "Argument list too long")
(defconstant +enoexec+ 8 "Exec format error")
(defconstant +ebadf+ 9 "Bad file number")
(defconstant +echild+ 10 "No child processes")
(defconstant +eagain+ 11 "Try again")
(defconstant +enomem+ 12 "Out of memory")
(defconstant +eacces+ 13 "Permission denied")
(defconstant +efault+ 14 "Bad address")
(defconstant +enotblk+ 15 "Block device required")
(defconstant +ebusy+ 16 "Device or resource busy")
(defconstant +eexist+ 17 "File exists")
(defconstant +exdev+ 18 "Cross-device link")
(defconstant +enodev+ 19 "No such device")
(defconstant +enotdir+ 20 "Not a directory")
(defconstant +eisdir+ 21 "Is a directory")
(defconstant +einval+ 22 "Invalid argument")
(defconstant +enfile+ 23 "File table overflow")
(defconstant +emfile+ 24 "Too many open files")
(defconstant +enotty+ 25 "Not a typewriter")
(defconstant +etxtbsy+ 26 "Text file busy")
(defconstant +efbig+ 27 "File too large")
(defconstant +enospc+ 28 "No space left on device")
(defconstant +espipe+ 29 "Illegal seek")
(defconstant +erofs+ 30 "Read-only file system")
(defconstant +emlink+ 31 "Too many links")
(defconstant +epipe+ 32 "Broken pipe")
(defconstant +edom+ 33 "Math argument out of domain")
(defconstant +erange+ 34 "Math result not representable")

;;; System Call Context

(defstruct syscall-context
  "System call context from exception handler."
  (number 0 :type (unsigned-byte 64))      ; r0 on PowerISA
  (arg1 0 :type (unsigned-byte 64))         ; r3
  (arg2 0 :type (unsigned-byte 64))         ; r4
  (arg3 0 :type (unsigned-byte 64))         ; r5
  (arg4 0 :type (unsigned-byte 64))         ; r6
  (arg5 0 :type (unsigned-byte 64))         ; r7
  (arg6 0 :type (unsigned-byte 64))         ; r8
  (return-value 0 :type (signed-byte 64))   ; Return in r3
  (error-code 0 :type (unsigned-byte 32))   ; errno
  (srr0 0 :type (unsigned-byte 64))         ; Return address
  (srr1 0 :type (unsigned-byte 64))         ; Saved MSR
  (pid 0 :type (unsigned-byte 32)))         ; Calling process

(defstruct syscall-handler
  "System call handler descriptor."
  (number 0 :type (unsigned-byte 16))
  (name "" :type string)
  (handler nil :type (or null function))
  (arg-count 0 :type (unsigned-byte 8))
  (trace-enabled t :type boolean))

(defstruct syscall-trace-entry
  "System call trace entry for auditing."
  (timestamp 0 :type (unsigned-byte 64))
  (pid 0 :type (unsigned-byte 32))
  (syscall 0 :type (unsigned-byte 16))
  (args nil :type list)
  (result 0 :type (signed-byte 64))
  (errno 0 :type (unsigned-byte 32))
  (duration 0 :type (unsigned-byte 64)))

;;; Global State

(defvar *syscall-initialized* nil)
(defvar *syscall-table* (make-hash-table) "System call dispatch table")
(defvar *syscall-trace-enabled* nil "Global syscall tracing")
(defvar *syscall-trace-log* nil "Syscall trace log")
(defvar *max-trace-entries* 10000 "Maximum trace entries")
(defvar *syscall-stats* (make-hash-table) "Syscall statistics")

;;; Initialization

(defun syscall-init ()
  "Initialize system call dispatcher."
  (when *syscall-initialized*
    (return-from syscall-init t))

  ;; Register all syscall handlers
  (register-all-syscalls)

  (setf *syscall-initialized* t)
  t)

(defun register-all-syscalls ()
  "Register all system call handlers."
  ;; Process management
  (syscall-register +sys-exit+ "exit" #'sys-exit 1)
  (syscall-register +sys-fork+ "fork" #'sys-fork 0)
  (syscall-register +sys-execve+ "execve" #'sys-execve 3)
  (syscall-register +sys-getpid+ "getpid" #'sys-getpid 0)
  (syscall-register +sys-getppid+ "getppid" #'sys-getppid 0)
  (syscall-register +sys-getuid+ "getuid" #'sys-getuid 0)
  (syscall-register +sys-getgid+ "getgid" #'sys-getgid 0)
  (syscall-register +sys-geteuid+ "geteuid" #'sys-geteuid 0)
  (syscall-register +sys-getegid+ "getegid" #'sys-getegid 0)
  (syscall-register +sys-setuid+ "setuid" #'sys-setuid 1)
  (syscall-register +sys-setgid+ "setgid" #'sys-setgid 1)
  (syscall-register +sys-clone+ "clone" #'sys-clone 5)
  (syscall-register +sys-vfork+ "vfork" #'sys-vfork 0)
  (syscall-register +sys-waitpid+ "waitpid" #'sys-waitpid 3)
  (syscall-register +sys-wait4+ "wait4" #'sys-wait4 4)
  (syscall-register +sys-exit-group+ "exit_group" #'sys-exit-group 1)

  ;; File operations
  (syscall-register +sys-open+ "open" #'sys-open 3)
  (syscall-register +sys-close+ "close" #'sys-close 1)
  (syscall-register +sys-read+ "read" #'sys-read 3)
  (syscall-register +sys-write+ "write" #'sys-write 3)
  (syscall-register +sys-lseek+ "lseek" #'sys-lseek 3)
  (syscall-register +sys-creat+ "creat" #'sys-creat 2)
  (syscall-register +sys-link+ "link" #'sys-link 2)
  (syscall-register +sys-unlink+ "unlink" #'sys-unlink 1)
  (syscall-register +sys-rename+ "rename" #'sys-rename 2)
  (syscall-register +sys-mkdir+ "mkdir" #'sys-mkdir 2)
  (syscall-register +sys-rmdir+ "rmdir" #'sys-rmdir 1)
  (syscall-register +sys-chdir+ "chdir" #'sys-chdir 1)
  (syscall-register +sys-fchdir+ "fchdir" #'sys-fchdir 1)
  (syscall-register +sys-getcwd+ "getcwd" #'sys-getcwd 2)
  (syscall-register +sys-dup+ "dup" #'sys-dup 1)
  (syscall-register +sys-dup2+ "dup2" #'sys-dup2 2)
  (syscall-register +sys-pipe+ "pipe" #'sys-pipe 1)
  (syscall-register +sys-readv+ "readv" #'sys-readv 3)
  (syscall-register +sys-writev+ "writev" #'sys-writev 3)
  (syscall-register +sys-pread64+ "pread64" #'sys-pread64 4)
  (syscall-register +sys-pwrite64+ "pwrite64" #'sys-pwrite64 4)

  ;; File metadata
  (syscall-register +sys-stat+ "stat" #'sys-stat 2)
  (syscall-register +sys-fstat+ "fstat" #'sys-fstat 2)
  (syscall-register +sys-access+ "access" #'sys-access 2)
  (syscall-register +sys-chmod+ "chmod" #'sys-chmod 2)
  (syscall-register +sys-fchmod+ "fchmod" #'sys-fchmod 2)
  (syscall-register +sys-chown+ "chown" #'sys-chown 3)
  (syscall-register +sys-fchown+ "fchown" #'sys-fchown 3)
  (syscall-register +sys-lchown+ "lchown" #'sys-lchown 3)
  (syscall-register +sys-umask+ "umask" #'sys-umask 1)
  (syscall-register +sys-truncate+ "truncate" #'sys-truncate 2)
  (syscall-register +sys-ftruncate+ "ftruncate" #'sys-ftruncate 2)

  ;; Memory management
  (syscall-register +sys-brk+ "brk" #'sys-brk 1)
  (syscall-register +sys-mmap+ "mmap" #'sys-mmap 6)
  (syscall-register +sys-munmap+ "munmap" #'sys-munmap 2)
  (syscall-register +sys-mprotect+ "mprotect" #'sys-mprotect 3)
  (syscall-register +sys-msync+ "msync" #'sys-msync 3)
  (syscall-register +sys-mlock+ "mlock" #'sys-mlock 2)
  (syscall-register +sys-munlock+ "munlock" #'sys-munlock 2)
  (syscall-register +sys-mlockall+ "mlockall" #'sys-mlockall 1)
  (syscall-register +sys-munlockall+ "munlockall" #'sys-munlockall 0)
  (syscall-register +sys-mremap+ "mremap" #'sys-mremap 5)

  ;; Signals
  (syscall-register +sys-kill+ "kill" #'sys-kill 2)
  (syscall-register +sys-signal+ "signal" #'sys-signal 2)
  (syscall-register +sys-sigaction+ "sigaction" #'sys-sigaction 3)
  (syscall-register +sys-rt-sigaction+ "rt_sigaction" #'sys-rt-sigaction 4)
  (syscall-register +sys-sigprocmask+ "sigprocmask" #'sys-sigprocmask 3)
  (syscall-register +sys-rt-sigprocmask+ "rt_sigprocmask" #'sys-rt-sigprocmask 4)
  (syscall-register +sys-sigpending+ "sigpending" #'sys-sigpending 1)
  (syscall-register +sys-rt-sigpending+ "rt_sigpending" #'sys-rt-sigpending 2)
  (syscall-register +sys-sigsuspend+ "sigsuspend" #'sys-sigsuspend 1)
  (syscall-register +sys-rt-sigsuspend+ "rt_sigsuspend" #'sys-rt-sigsuspend 2)
  (syscall-register +sys-sigaltstack+ "sigaltstack" #'sys-sigaltstack 2)
  (syscall-register +sys-pause+ "pause" #'sys-pause 0)

  ;; Time
  (syscall-register +sys-time+ "time" #'sys-time 1)
  (syscall-register +sys-gettimeofday+ "gettimeofday" #'sys-gettimeofday 2)
  (syscall-register +sys-settimeofday+ "settimeofday" #'sys-settimeofday 2)
  (syscall-register +sys-clock-gettime+ "clock_gettime" #'sys-clock-gettime 2)
  (syscall-register +sys-clock-settime+ "clock_settime" #'sys-clock-settime 2)
  (syscall-register +sys-clock-getres+ "clock_getres" #'sys-clock-getres 2)
  (syscall-register +sys-nanosleep+ "nanosleep" #'sys-nanosleep 2)
  (syscall-register +sys-clock-nanosleep+ "clock_nanosleep" #'sys-clock-nanosleep 4)
  (syscall-register +sys-alarm+ "alarm" #'sys-alarm 1)
  (syscall-register +sys-setitimer+ "setitimer" #'sys-setitimer 3)
  (syscall-register +sys-getitimer+ "getitimer" #'sys-getitimer 2)

  ;; Networking
  (syscall-register +sys-socket+ "socket" #'sys-socket 3)
  (syscall-register +sys-bind+ "bind" #'sys-bind 3)
  (syscall-register +sys-connect+ "connect" #'sys-connect 3)
  (syscall-register +sys-listen+ "listen" #'sys-listen 2)
  (syscall-register +sys-accept+ "accept" #'sys-accept 3)
  (syscall-register +sys-send+ "send" #'sys-send 4)
  (syscall-register +sys-recv+ "recv" #'sys-recv 4)
  (syscall-register +sys-sendto+ "sendto" #'sys-sendto 6)
  (syscall-register +sys-recvfrom+ "recvfrom" #'sys-recvfrom 6)
  (syscall-register +sys-sendmsg+ "sendmsg" #'sys-sendmsg 3)
  (syscall-register +sys-recvmsg+ "recvmsg" #'sys-recvmsg 3)
  (syscall-register +sys-shutdown+ "shutdown" #'sys-shutdown 2)
  (syscall-register +sys-getsockopt+ "getsockopt" #'sys-getsockopt 5)
  (syscall-register +sys-setsockopt+ "setsockopt" #'sys-setsockopt 5)
  (syscall-register +sys-getsockname+ "getsockname" #'sys-getsockname 3)
  (syscall-register +sys-getpeername+ "getpeername" #'sys-getpeername 3)
  (syscall-register +sys-socketpair+ "socketpair" #'sys-socketpair 4)

  ;; Process groups and sessions
  (syscall-register +sys-setpgid+ "setpgid" #'sys-setpgid 2)
  (syscall-register +sys-getpgid+ "getpgid" #'sys-getpgid 1)
  (syscall-register +sys-getpgrp+ "getpgrp" #'sys-getpgrp 0)
  (syscall-register +sys-setsid+ "setsid" #'sys-setsid 0)
  (syscall-register +sys-getsid+ "getsid" #'sys-getsid 1)

  ;; Scheduling
  (syscall-register +sys-sched-yield+ "sched_yield" #'sys-sched-yield 0)
  (syscall-register +sys-sched-setparam+ "sched_setparam" #'sys-sched-setparam 2)
  (syscall-register +sys-sched-getparam+ "sched_getparam" #'sys-sched-getparam 2)
  (syscall-register +sys-sched-setscheduler+ "sched_setscheduler" #'sys-sched-setscheduler 3)
  (syscall-register +sys-sched-getscheduler+ "sched_getscheduler" #'sys-sched-getscheduler 1)
  (syscall-register +sys-sched-setaffinity+ "sched_setaffinity" #'sys-sched-setaffinity 3)
  (syscall-register +sys-sched-getaffinity+ "sched_getaffinity" #'sys-sched-getaffinity 3)
  (syscall-register +sys-getpriority+ "getpriority" #'sys-getpriority 2)
  (syscall-register +sys-setpriority+ "setpriority" #'sys-setpriority 3)

  ;; Misc
  (syscall-register +sys-sync+ "sync" #'sys-sync 0)
  (syscall-register +sys-fsync+ "fsync" #'sys-fsync 1)
  (syscall-register +sys-fdatasync+ "fdatasync" #'sys-fdatasync 1)
  (syscall-register +sys-ioctl+ "ioctl" #'sys-ioctl 3)
  (syscall-register +sys-fcntl+ "fcntl" #'sys-fcntl 3)
  (syscall-register +sys-select+ "select" #'sys-select 5)
  (syscall-register +sys-poll+ "poll" #'sys-poll 3)
  (syscall-register +sys-getdents+ "getdents" #'sys-getdents 3)
  (syscall-register +sys-uname+ "uname" #'sys-uname 1)
  (syscall-register +sys-sysinfo+ "sysinfo" #'sys-sysinfo 1)

  t)

(defun syscall-register (number name handler arg-count)
  "Register system call handler."
  (declare (type (unsigned-byte 16) number)
           (type string name)
           (type function handler)
           (type (unsigned-byte 8) arg-count))

  (setf (gethash number *syscall-table*)
        (make-syscall-handler
         :number number
         :name name
         :handler handler
         :arg-count arg-count
         :trace-enabled t)))

;;; Main Dispatch Function

(defun syscall-dispatch (exception-context)
  "Dispatch system call from exception context."
  (declare (type t exception-context))

  (let* ((syscall-num (get-register exception-context 0))  ; r0 = syscall number
         (arg1 (get-register exception-context 3))         ; r3 = arg1
         (arg2 (get-register exception-context 4))         ; r4 = arg2
         (arg3 (get-register exception-context 5))         ; r5 = arg3
         (arg4 (get-register exception-context 6))         ; r6 = arg4
         (arg5 (get-register exception-context 7))         ; r7 = arg5
         (arg6 (get-register exception-context 8))         ; r8 = arg6
         (srr0 (get-srr0 exception-context))
         (srr1 (get-srr1 exception-context))
         (pid (get-current-pid))
         (start-time (when *syscall-trace-enabled* (get-tick-count))))

    (let ((ctx (make-syscall-context
                :number syscall-num
                :arg1 arg1
                :arg2 arg2
                :arg3 arg3
                :arg4 arg4
                :arg5 arg5
                :arg6 arg6
                :srr0 srr0
                :srr1 srr1
                :pid pid)))

      ;; Validate syscall number
      (let ((handler (gethash syscall-num *syscall-table*)))
        (if handler
            (progn
              ;; Trace entry
              (when (and *syscall-trace-enabled* (syscall-handler-trace-enabled handler))
                (trace-syscall-enter ctx handler))

              ;; Execute handler
              (handler-case
                  (let ((result (execute-syscall-handler handler ctx)))
                    (setf (syscall-context-return-value ctx) result)
                    (setf (syscall-context-error-code ctx) 0))
                (syscall-error (e)
                  (setf (syscall-context-return-value ctx) -1)
                  (setf (syscall-context-error-code ctx) (syscall-error-errno e)))
                (error (e)
                  (format t "Syscall ~A error: ~A~%" (syscall-handler-name handler) e)
                  (setf (syscall-context-return-value ctx) -1)
                  (setf (syscall-context-error-code ctx) +einval+)))

              ;; Trace exit
              (when (and *syscall-trace-enabled* (syscall-handler-trace-enabled handler))
                (trace-syscall-exit ctx handler start-time))

              ;; Update statistics
              (update-syscall-stats syscall-num))

            ;; Unknown syscall
            (progn
              (format t "Unknown syscall: ~A~%" syscall-num)
              (setf (syscall-context-return-value ctx) -1)
              (setf (syscall-context-error-code ctx) +einval+))))

      ;; Set return values in exception context
      (set-register exception-context 3 (syscall-context-return-value ctx))
      (when (< (syscall-context-return-value ctx) 0)
        ;; Set errno on error
        (set-errno (syscall-context-error-code ctx))))))

(defun execute-syscall-handler (handler ctx)
  "Execute syscall handler with appropriate number of arguments."
  (declare (type syscall-handler handler)
           (type syscall-context ctx))

  (let ((func (syscall-handler-handler handler))
        (arg-count (syscall-handler-arg-count handler)))
    (case arg-count
      (0 (funcall func))
      (1 (funcall func (syscall-context-arg1 ctx)))
      (2 (funcall func (syscall-context-arg1 ctx) (syscall-context-arg2 ctx)))
      (3 (funcall func (syscall-context-arg1 ctx) (syscall-context-arg2 ctx)
                  (syscall-context-arg3 ctx)))
      (4 (funcall func (syscall-context-arg1 ctx) (syscall-context-arg2 ctx)
                  (syscall-context-arg3 ctx) (syscall-context-arg4 ctx)))
      (5 (funcall func (syscall-context-arg1 ctx) (syscall-context-arg2 ctx)
                  (syscall-context-arg3 ctx) (syscall-context-arg4 ctx)
                  (syscall-context-arg5 ctx)))
      (6 (funcall func (syscall-context-arg1 ctx) (syscall-context-arg2 ctx)
                  (syscall-context-arg3 ctx) (syscall-context-arg4 ctx)
                  (syscall-context-arg5 ctx) (syscall-context-arg6 ctx)))
      (t (error "Invalid argument count: ~A" arg-count)))))

;;; Syscall Error Handling

(define-condition syscall-error (error)
  ((errno :initarg :errno :reader syscall-error-errno)
   (message :initarg :message :reader syscall-error-message))
  (:report (lambda (condition stream)
             (format stream "Syscall error ~A: ~A"
                     (syscall-error-errno condition)
                     (syscall-error-message condition)))))

(defun syscall-error (errno message)
  "Signal syscall error with errno."
  (error 'syscall-error :errno errno :message message))

;;; Tracing

(defun syscall-trace-enable ()
  "Enable syscall tracing."
  (setf *syscall-trace-enabled* t))

(defun syscall-trace-disable ()
  "Disable syscall tracing."
  (setf *syscall-trace-enabled* nil))

(defun trace-syscall-enter (ctx handler)
  "Trace syscall entry."
  (declare (type syscall-context ctx)
           (type syscall-handler handler))
  (format t "[~A] ~A(~A, ~A, ~A, ~A, ~A, ~A)~%"
          (syscall-context-pid ctx)
          (syscall-handler-name handler)
          (syscall-context-arg1 ctx)
          (syscall-context-arg2 ctx)
          (syscall-context-arg3 ctx)
          (syscall-context-arg4 ctx)
          (syscall-context-arg5 ctx)
          (syscall-context-arg6 ctx)))

(defun trace-syscall-exit (ctx handler start-time)
  "Trace syscall exit."
  (declare (type syscall-context ctx)
           (type syscall-handler handler)
           (type (unsigned-byte 64) start-time))
  (let ((duration (- (get-tick-count) start-time)))
    (format t "[~A] ~A = ~A (errno=~A, ~A ticks)~%"
            (syscall-context-pid ctx)
            (syscall-handler-name handler)
            (syscall-context-return-value ctx)
            (syscall-context-error-code ctx)
            duration)

    ;; Add to trace log
    (when (< (length *syscall-trace-log*) *max-trace-entries*)
      (push (make-syscall-trace-entry
             :timestamp start-time
             :pid (syscall-context-pid ctx)
             :syscall (syscall-context-number ctx)
             :args (list (syscall-context-arg1 ctx) (syscall-context-arg2 ctx)
                        (syscall-context-arg3 ctx) (syscall-context-arg4 ctx)
                        (syscall-context-arg5 ctx) (syscall-context-arg6 ctx))
             :result (syscall-context-return-value ctx)
             :errno (syscall-context-error-code ctx)
             :duration duration)
            *syscall-trace-log*))))

(defun update-syscall-stats (syscall-num)
  "Update syscall statistics."
  (let ((count (gethash syscall-num *syscall-stats* 0)))
    (setf (gethash syscall-num *syscall-stats*) (1+ count))))

;;; System Call Implementations (Stubs for now - will be comprehensive)

;; Process Management
(defun sys-exit (status)
  (declare (type (signed-byte 32) status))
  (process-exit status)
  0)

(defun sys-fork ()
  (let ((child-pid (process-fork)))
    (if child-pid child-pid 0)))

(defun sys-execve (filename argv envp)
  (declare (type (unsigned-byte 64) filename argv envp))
  (process-exec (read-user-string filename)
                (read-user-string-array argv)
                (read-user-string-array envp)))

(defun sys-getpid ()
  (get-current-pid))

(defun sys-getppid ()
  (get-parent-pid))

(defun sys-getuid () 0)
(defun sys-getgid () 0)
(defun sys-geteuid () 0)
(defun sys-getegid () 0)

(defun sys-setuid (uid)
  (declare (ignore uid))
  (syscall-error +eperm+ "Operation not permitted"))

(defun sys-setgid (gid)
  (declare (ignore gid))
  (syscall-error +eperm+ "Operation not permitted"))

(defun sys-clone (flags stack parent-tid child-tid tls)
  (declare (ignore flags stack parent-tid child-tid tls))
  (sys-fork))

(defun sys-vfork ()
  (sys-fork))

(defun sys-waitpid (pid status options)
  (declare (type (signed-byte 32) pid)
           (type (unsigned-byte 64) status options))
  (process-wait pid status options))

(defun sys-wait4 (pid status options rusage)
  (declare (ignore rusage))
  (sys-waitpid pid status options))

(defun sys-exit-group (status)
  (sys-exit status))

;; File Operations
(defun sys-open (pathname flags mode)
  (declare (type (unsigned-byte 64) pathname)
           (type (unsigned-byte 32) flags mode))
  (vfs-open (read-user-string pathname) flags mode))

(defun sys-close (fd)
  (declare (type (unsigned-byte 32) fd))
  (vfs-close fd))

(defun sys-read (fd buf count)
  (declare (type (unsigned-byte 32) fd)
           (type (unsigned-byte 64) buf count))
  (vfs-read fd buf count))

(defun sys-write (fd buf count)
  (declare (type (unsigned-byte 32) fd)
           (type (unsigned-byte 64) buf count))
  (vfs-write fd buf count))

(defun sys-lseek (fd offset whence)
  (declare (type (unsigned-byte 32) fd)
           (type (signed-byte 64) offset)
           (type (unsigned-byte 32) whence))
  (vfs-lseek fd offset whence))

(defun sys-creat (pathname mode)
  (declare (type (unsigned-byte 64) pathname)
           (type (unsigned-byte 32) mode))
  (sys-open pathname #x0241 mode))  ; O_CREAT | O_WRONLY | O_TRUNC

;; Memory Management
(defun sys-brk (addr)
  (declare (type (unsigned-byte 64) addr))
  (process-brk addr))

(defun sys-mmap (addr length prot flags fd offset)
  (declare (type (unsigned-byte 64) addr length offset)
           (type (unsigned-byte 32) prot flags fd))
  (process-mmap addr length prot flags fd offset))

(defun sys-munmap (addr length)
  (declare (type (unsigned-byte 64) addr length))
  (process-munmap addr length))

(defun sys-mprotect (addr length prot)
  (declare (type (unsigned-byte 64) addr length)
           (type (unsigned-byte 32) prot))
  (process-mprotect addr length prot))

;; Signals
(defun sys-kill (pid sig)
  (declare (type (signed-byte 32) pid)
           (type (unsigned-byte 32) sig))
  (signal-send pid sig))

(defun sys-signal (signum handler)
  (declare (ignore signum handler))
  0)

(defun sys-sigaction (signum act oldact)
  (declare (ignore signum act oldact))
  0)

(defun sys-rt-sigaction (signum act oldact sigsetsize)
  (declare (ignore signum act oldact sigsetsize))
  0)

(defun sys-sigprocmask (how set oldset)
  (declare (ignore how set oldset))
  0)

(defun sys-rt-sigprocmask (how set oldset sigsetsize)
  (declare (ignore how set oldset sigsetsize))
  0)

;; Time
(defun sys-time (tloc)
  (declare (type (unsigned-byte 64) tloc))
  (let ((time (get-system-time)))
    (when (not (zerop tloc))
      (write-user-u64 tloc time))
    time))

(defun sys-gettimeofday (tv tz)
  (declare (type (unsigned-byte 64) tv tz))
  (when (not (zerop tv))
    (let ((time (get-system-time-usec)))
      (write-user-u64 tv (car time))       ; tv_sec
      (write-user-u64 (+ tv 8) (cdr time)))) ; tv_usec
  0)

(defun sys-clock-gettime (clk-id tp)
  (declare (type (unsigned-byte 32) clk-id)
           (type (unsigned-byte 64) tp))
  (when (not (zerop tp))
    (let ((time (get-clock-time clk-id)))
      (write-user-u64 tp (car time))       ; tv_sec
      (write-user-u64 (+ tp 8) (cdr time)))) ; tv_nsec
  0)

(defun sys-nanosleep (req rem)
  (declare (type (unsigned-byte 64) req rem))
  (let ((sec (read-user-u64 req))
        (nsec (read-user-u64 (+ req 8))))
    (process-sleep (+ (* sec 1000000000) nsec))
    0))

;; Scheduling
(defun sys-sched-yield ()
  (scheduler-yield)
  0)

;; Placeholder implementations for remaining syscalls

(defun sys-link (oldpath newpath) (declare (ignore oldpath newpath)) -1) ; ENOSYS
(defun sys-unlink (pathname) 
  (kernel-vfs-unlink pathname))

(defun sys-rename (oldpath newpath) (declare (ignore oldpath newpath)) -1) ; ENOSYS

(defun sys-mkdir (pathname mode) 
  (declare (ignore mode))
  0)

(defun set-register (context reg-num value)
  "Set register in exception context."
  (declare (ignore context reg-num value))
  nil)

(defun get-srr0 (context)
  "Get SRR0 from context."
  (declare (ignore context))
  0)

(defun get-srr1 (context)
  "Get SRR1 from context."
  (declare (ignore context))
  0)

(defun set-errno (errno)
  "Set errno for current thread."
  (declare (ignore errno))
  nil)

(defun read-user-string (addr)
  "Read null-terminated string from user space."
  (declare (type (unsigned-byte 64) addr))
  (declare (ignore addr))
  "")

(defun read-user-string-array (addr)
  "Read array of strings from user space."
  (declare (type (unsigned-byte 64) addr))
  (declare (ignore addr))
  nil)

(defun read-user-u64 (addr)
  "Read 64-bit value from user space."
  (declare (type (unsigned-byte 64) addr))
  (declare (ignore addr))
  0)

(defun write-user-u64 (addr value)
  "Write 64-bit value to user space."
  (declare (type (unsigned-byte 64) addr value))
  (declare (ignore addr value))
  nil)

;;; Forward Declarations

;;; Forward Declarations - Implementation via Alien FFI

(defun get-current-pid () 
  (alien-funcall (extern-alien "get_current_pid" (function unsigned-int))))

(defun get-parent-pid () 
  (alien-funcall (extern-alien "get_parent_pid" (function unsigned-int))))

(defun get-tick-count () 
   (alien-funcall (extern-alien "timer_get_tick" (function unsigned-long))))

(defun get-system-time () 
   (alien-funcall (extern-alien "timer_get_unix_time" (function unsigned-long))))

(defun get-system-time-usec () 
   (let ((sec (alien-funcall (extern-alien "timer_get_unix_time" (function unsigned-long))))
         (usec (alien-funcall (extern-alien "timer_get_usec" (function unsigned-long)))))
     (cons sec usec)))

(defun get-clock-time (clk-id) 
  (declare (ignore clk-id)) 
  (get-system-time-usec))

(defun process-exit (status)
  (alien-funcall (extern-alien "process_exit" (function void int)) status))

(defun process-fork ()
  (alien-funcall (extern-alien "process_fork" (function int))))

(defun process-exec (filename argv envp) 
   (alien-funcall (extern-alien "process_exec" (function int c-string (* c-string) (* c-string)))
                  filename argv envp))

(defun process-wait (pid status options) 
   (alien-funcall (extern-alien "process_wait" (function int int (* int) int))
                  pid status options))

(defun process-brk (addr) 
   (alien-funcall (extern-alien "process_brk" (function int unsigned-long)) addr))

(defun process-mmap (addr length prot flags fd offset)
   (alien-funcall (extern-alien "process_mmap" (function unsigned-long unsigned-long unsigned-long int int int unsigned-long))
                  addr length prot flags fd offset))

(defun process-munmap (addr length)
   (alien-funcall (extern-alien "process_munmap" (function int unsigned-long unsigned-long))
                  addr length))

(defun process-mprotect (addr length prot)
   (alien-funcall (extern-alien "process_mprotect" (function int unsigned-long unsigned-long int))
                  addr length prot))

(defun process-sleep (nsec)
   ;; Convert nsec to ticks (assuming 1ms tick = 1000000ns)
   (let ((ticks (floor nsec 1000000)))
     (alien-funcall (extern-alien "scheduler_sleep" (function void unsigned-int)) ticks)))

(defun vfs-open (pathname flags mode) 
  (alien-funcall (extern-alien "lfsx_vfs_open" (function int c-string int int)) pathname flags mode))

(defun vfs-close (fd) 
  (alien-funcall (extern-alien "lfsx_vfs_close" (function int int)) fd))

(defun vfs-read (fd buf count) 
  (alien-funcall (extern-alien "lfsx_vfs_read" (function int int (* void) unsigned-long)) fd buf count))

(defun vfs-write (fd buf count) 
  (alien-funcall (extern-alien "lfsx_vfs_write" (function int int (* void) unsigned-long)) fd buf count))

(defun vfs-lseek (fd offset whence) 
  (alien-funcall (extern-alien "lfsx_vfs_lseek" (function int int long int)) fd offset whence))

(defun signal-send (pid sig) 
  (alien-funcall (extern-alien "signal_send" (function int int int)) pid sig))

(defun scheduler-yield () 
  (alien-funcall (extern-alien "scheduler_yield" (function void))))

