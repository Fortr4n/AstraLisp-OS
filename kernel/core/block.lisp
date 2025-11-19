;; AstraLisp OS Kernel Block Device Layer
;; Production block I/O subsystem with I/O schedulers and request queues

(defpackage :astralisp-block
  (:use :cl)
  (:export ;; Initialization
           :block-init
           ;; Device management
           :block-device-register
           :block-device-unregister
           ;; BIO operations
           :bio-alloc
           :bio-add-page
           :bio-endio
           :bio-clone
           :bio-split
           :block-device-submit-bio
           ;; Synchronous I/O
           :block-device-read
           :block-device-write
           :block-device-flush
           :block-device-read-sectors
           :block-device-write-sectors
           ;; Asynchronous I/O
           :block-device-read-async
           :block-device-write-async
           ;; Partition support
           :make-partition-table
           :parse-mbr-partitions
           :parse-gpt-partitions
           ;; Statistics and monitoring
           :block-device-get-stats
           :block-device-reset-stats
           :block-device-print-stats
           ;; Queue management
           :block-queue-set-max-sectors
           :block-queue-set-scheduler
           ;; I/O barriers
           :block-device-barrier
           :wait-for-all-io
           ;; Read-ahead
           :readahead-submit
           :readahead-detect-sequential
           ;; Error handling
           :retry-bio
           :complete-request
           ;; Constants
           :+iosched-noop+
           :+iosched-deadline+
           :+iosched-cfq+
           :+iosched-bfq+
           :+bio-read+
           :+bio-write+
           :+bio-sync+
           :+bio-flush+
           :+bio-discard+
           :+bio-fua+
           ;; Data structures
           :block-device
           :bio
           :request
           :request-queue
           :partition
           :block-stats))

(in-package :astralisp-block)

;;; Block Device Constants

(defconstant +block-size-512+ 512 "512-byte sectors")
(defconstant +block-size-1k+ 1024 "1KB blocks")
(defconstant +block-size-2k+ 2048 "2KB blocks")
(defconstant +block-size-4k+ 4096 "4KB blocks")
(defconstant +block-size-8k+ 8192 "8KB blocks")

(defconstant +default-block-size+ +block-size-512+)
(defconstant +max-sectors-per-request+ 256)
(defconstant +max-segments-per-request+ 128)

;;; BIO (Block I/O) Flags

(defconstant +bio-read+ #x0001 "Read operation")
(defconstant +bio-write+ #x0002 "Write operation")
(defconstant +bio-sync+ #x0004 "Synchronous I/O")
(defconstant +bio-flush+ #x0008 "Flush/barrier request")
(defconstant +bio-discard+ #x0010 "Discard/TRIM request")
(defconstant +bio-fua+ #x0020 "Force Unit Access")
(defconstant +bio-meta+ #x0040 "Metadata operation")

;;; Request Flags

(defconstant +req-sorted+ #x0001 "Request is sorted in queue")
(defconstant +req-softbarrier+ #x0002 "Soft barrier")
(defconstant +req-hardbarrier+ #x0004 "Hard barrier")
(defconstant +req-cmd+ #x0008 "Command request")
(defconstant +req-nomerge+ #x0010 "Don't merge this request")
(defconstant +req-started+ #x0020 "Request started")
(defconstant +req-completed+ #x0040 "Request completed")
(defconstant +req-queued+ #x0080 "Request queued")

;;; I/O Scheduler Types

(defconstant +iosched-noop+ 0 "No-op scheduler (FIFO)")
(defconstant +iosched-deadline+ 1 "Deadline scheduler")
(defconstant +iosched-cfq+ 2 "Complete Fair Queuing")
(defconstant +iosched-bfq+ 3 "Budget Fair Queuing")

;;; Partition Types

(defconstant +part-type-empty+ #x00)
(defconstant +part-type-ext2+ #x83)
(defconstant +part-type-swap+ #x82)
(defconstant +part-type-extended+ #x05)
(defconstant +part-type-lvm+ #x8E)

;;; Data Structures

(defstruct bio-vec
  "Single segment in a BIO."
  (page 0 :type (unsigned-byte 64))           ; Physical page address
  (length 0 :type (unsigned-byte 32))         ; Length in bytes
  (offset 0 :type (unsigned-byte 32)))        ; Offset in page

(defstruct bio
  "Block I/O request."
  (id 0 :type (unsigned-byte 64))
  (device nil :type (or null block-device))
  (sector 0 :type (unsigned-byte 64))         ; Starting sector
  (size 0 :type (unsigned-byte 32))           ; Total size in bytes
  (flags 0 :type (unsigned-byte 16))
  (vcnt 0 :type (unsigned-byte 16))           ; Number of segments
  (vecs (make-array +max-segments-per-request+ :initial-element nil) :type (vector t))
  (end-io nil :type (or null function))       ; Completion callback
  (private nil :type t)                       ; Private data
  (error 0 :type (signed-byte 32))
  (next nil :type (or null bio)))

(defstruct request
  "Block device request."
  (id 0 :type (unsigned-byte 64))
  (device nil :type (or null block-device))
  (sector 0 :type (unsigned-byte 64))
  (nr-sectors 0 :type (unsigned-byte 32))
  (current-nr-sectors 0 :type (unsigned-byte 32))
  (hard-sector 0 :type (unsigned-byte 64))
  (hard-nr-sectors 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 16))
  (cmd-flags 0 :type (unsigned-byte 16))
  (bio nil :type (or null bio))               ; First BIO in request
  (biotail nil :type (or null bio))           ; Last BIO in request
  (buffer 0 :type (unsigned-byte 64))         ; Data buffer
  (sense-buffer nil :type (or null (vector (unsigned-byte 8))))
  (timeout 0 :type (unsigned-byte 32))
  (retries 0 :type (unsigned-byte 8))
  (errors 0 :type (unsigned-byte 32))
  (start-time 0 :type (unsigned-byte 64))
  (completion-data nil :type t)
  (next nil :type (or null request))
  (prev nil :type (or null request)))

(defstruct request-queue
  "Block device request queue."
  (requests nil :type list)                   ; Pending requests
  (in-flight nil :type list)                  ; In-flight requests
  (scheduler-type +iosched-deadline+ :type (unsigned-byte 8))
  (scheduler-data nil :type t)
  (queue-lock nil :type t)
  (last-merge 0 :type (unsigned-byte 64))
  (boundary 0 :type (unsigned-byte 32))
  (max-sectors +max-sectors-per-request+ :type (unsigned-byte 32))
  (max-segments +max-segments-per-request+ :type (unsigned-byte 16))
  (max-hw-sectors 0 :type (unsigned-byte 32))
  (stats nil :type (or null block-stats)))

(defstruct deadline-data
  "Deadline I/O scheduler data."
  (read-fifo nil :type list)
  (write-fifo nil :type list)
  (sort-list nil :type list)
  (fifo-batch 16 :type (unsigned-byte 8))
  (read-expire 500 :type (unsigned-byte 32))  ; ms
  (write-expire 5000 :type (unsigned-byte 32))) ; ms

(defstruct cfq-data
  "CFQ I/O scheduler data."
  (queues (make-hash-table) :type hash-table) ; Per-process queues
  (service-tree nil :type list)
  (quantum 4 :type (unsigned-byte 8))
  (fifo-expire-sync 500 :type (unsigned-byte 32))
  (fifo-expire-async 5000 :type (unsigned-byte 32)))

(defstruct block-device
  "Block device descriptor."
  (major 0 :type (unsigned-byte 16))
  (minor 0 :type (unsigned-byte 16))
  (name "" :type string)
  (block-size +default-block-size+ :type (unsigned-byte 32))
  (size 0 :type (unsigned-byte 64))           ; Size in sectors
  (queue nil :type (or null request-queue))
  (ops nil :type (or null block-device-ops))
  (private-data nil :type t)
  (partitions nil :type list)
  (flags 0 :type (unsigned-byte 32))
  (in-use 0 :type (unsigned-byte 32)))

(defstruct block-device-ops
  "Block device operations."
  (open nil :type (or null function))
  (release nil :type (or null function))
  (ioctl nil :type (or null function))
  (submit-bio nil :type (or null function))
  (request-fn nil :type (or null function))
  (make-request-fn nil :type (or null function)))

(defstruct partition
  "Disk partition."
  (number 0 :type (unsigned-byte 8))
  (start-sector 0 :type (unsigned-byte 64))
  (nr-sectors 0 :type (unsigned-byte 64))
  (type 0 :type (unsigned-byte 8))
  (flags 0 :type (unsigned-byte 8))
  (device nil :type (or null block-device)))

(defstruct block-stats
  "Block device statistics."
  (reads 0 :type (unsigned-byte 64))
  (writes 0 :type (unsigned-byte 64))
  (read-bytes 0 :type (unsigned-byte 64))
  (write-bytes 0 :type (unsigned-byte 64))
  (read-ticks 0 :type (unsigned-byte 64))
  (write-ticks 0 :type (unsigned-byte 64))
  (in-flight 0 :type (unsigned-byte 32))
  (io-ticks 0 :type (unsigned-byte 64))
  (time-in-queue 0 :type (unsigned-byte 64)))

;;; Global State

(defvar *block-initialized* nil)
(defvar *block-devices* (make-hash-table) "All block devices (major:minor)")
(defvar *block-device-lock* nil)
(defvar *bio-id-counter* 0)
(defvar *request-id-counter* 0)
(defvar *next-major* 8)  ; Start from 8 (SCSI disk)

;;; Initialization

(defun block-init ()
  "Initialize block device subsystem."
  (when *block-initialized*
    (return-from block-init t))

  (setf *block-device-lock* (make-mutex))

  (setf *block-initialized* t)
  t)

;;; Block Device Registration

(defun block-device-register (name size ops &key (block-size +default-block-size+)
                                                  (scheduler +iosched-deadline+))
  "Register new block device."
  (declare (type string name)
           (type (unsigned-byte 64) size)
           (type block-device-ops ops)
           (type (unsigned-byte 32) block-size)
           (type (unsigned-byte 8) scheduler))

  (with-mutex (*block-device-lock*)
    (let* ((major (atomic-incf *next-major*))
           (minor 0)
           (queue (make-request-queue-for-device block-size scheduler))
           (device (make-block-device
                    :major major
                    :minor minor
                    :name name
                    :block-size block-size
                    :size size
                    :queue queue
                    :ops ops)))

      ;; Initialize statistics
      (setf (request-queue-stats queue) (make-block-stats))

      ;; Store device
      (setf (gethash (make-dev-id major minor) *block-devices*) device)

      device)))

(defun block-device-unregister (device)
  "Unregister block device."
  (declare (type block-device device))

  (with-mutex (*block-device-lock*)
    (let ((dev-id (make-dev-id (block-device-major device)
                               (block-device-minor device))))
      (remhash dev-id *block-devices*)
      t)))

(defun make-dev-id (major minor)
  "Create device ID from major:minor."
  (declare (type (unsigned-byte 16) major minor))
  (logior (ash major 16) minor))

;;; Request Queue Management

(defun make-request-queue-for-device (block-size scheduler)
  "Create request queue for block device."
  (declare (type (unsigned-byte 32) block-size)
           (type (unsigned-byte 8) scheduler))

  (let ((queue (make-request-queue
                :queue-lock (make-spinlock)
                :scheduler-type scheduler
                :max-hw-sectors (floor (* 1024 1024) block-size)))) ; 1MB max

    ;; Initialize scheduler
    (case scheduler
      (#.+iosched-deadline+
       (setf (request-queue-scheduler-data queue)
             (make-deadline-data)))

      (#.+iosched-cfq+
       (setf (request-queue-scheduler-data queue)
             (make-cfq-data)))

      (#.+iosched-noop+
       nil))

    queue))

;;; BIO Management

(defun bio-alloc (nr-vecs)
  "Allocate new BIO."
  (declare (type (unsigned-byte 16) nr-vecs))

  (when (> nr-vecs +max-segments-per-request+)
    (setf nr-vecs +max-segments-per-request+))

  (make-bio
   :id (atomic-incf *bio-id-counter*)
   :vcnt nr-vecs))

(defun bio-add-page (bio page offset length)
  "Add page to BIO."
  (declare (type bio bio)
           (type (unsigned-byte 64) page)
           (type (unsigned-byte 32) offset length))

  (let ((vcnt (bio-vcnt bio)))
    (when (>= vcnt +max-segments-per-request+)
      (return-from bio-add-page nil))

    (let ((vec (make-bio-vec
                :page page
                :offset offset
                :length length)))
      (setf (aref (bio-vecs bio) vcnt) vec)
      (incf (bio-vcnt bio))
      (incf (bio-size bio) length)
      length)))

(defun bio-endio (bio error)
  "Complete BIO."
  (declare (type bio bio)
           (type (signed-byte 32) error))

  (setf (bio-error bio) error)

  ;; Call completion callback
  (when (bio-end-io bio)
    (funcall (bio-end-io bio) bio))

  t)

;;; Request Management

(defun request-alloc (queue)
  "Allocate new request."
  (declare (type request-queue queue))

  (make-request
   :id (atomic-incf *request-id-counter*)
   :start-time (get-tick-count)))

(defun request-add-bio (request bio)
  "Add BIO to request."
  (declare (type request request)
           (type bio bio))

  (if (null (request-bio request))
      ;; First BIO
      (progn
        (setf (request-bio request) bio)
        (setf (request-biotail request) bio)
        (setf (request-sector request) (bio-sector bio))
        (setf (request-hard-sector request) (bio-sector bio)))
      ;; Append to tail
      (progn
        (setf (bio-next (request-biotail request)) bio)
        (setf (request-biotail request) bio)))

  ;; Update sector counts
  (let ((nr-sectors (floor (bio-size bio) (block-device-block-size (bio-device bio)))))
    (incf (request-nr-sectors request) nr-sectors)
    (incf (request-hard-nr-sectors request) nr-sectors))

  t)

(defun request-can-merge (req bio)
  "Check if BIO can be merged with request."
  (declare (type request req)
           (type bio bio))

  (when (logtest (request-flags req) +req-nomerge+)
    (return-from request-can-merge nil))

  ;; Check if sectors are contiguous
  (let ((req-end (+ (request-sector req) (request-nr-sectors req))))
    (or
     ;; Back merge: BIO starts where request ends
     (= (bio-sector bio) req-end)
     ;; Front merge: Request starts where BIO ends
     (= (+ (bio-sector bio) (floor (bio-size bio)
                                   (block-device-block-size (bio-device bio))))
        (request-sector req)))))

(defun request-merge-bio (req bio)
  "Merge BIO into request."
  (declare (type request req)
           (type bio bio))

  (let ((bio-sectors (floor (bio-size bio)
                           (block-device-block-size (bio-device bio)))))

    (if (= (bio-sector bio) (+ (request-sector req) (request-nr-sectors req)))
        ;; Back merge
        (progn
          (request-add-bio req bio)
          :back-merge)
        ;; Front merge
        (progn
          (setf (request-sector req) (bio-sector bio))
          (setf (request-hard-sector req) (bio-sector bio))
          (setf (bio-next bio) (request-bio req))
          (setf (request-bio req) bio)
          (incf (request-nr-sectors req) bio-sectors)
          (incf (request-hard-nr-sectors req) bio-sectors)
          :front-merge))))

;;; I/O Submission

(defun block-device-submit-bio (bio)
  "Submit BIO to block device."
  (declare (type bio bio))

  (let* ((device (bio-device bio))
         (queue (block-device-queue device)))

    ;; Update statistics
    (let ((stats (request-queue-stats queue)))
      (if (logtest (bio-flags bio) +bio-read+)
          (progn
            (incf (block-stats-reads stats))
            (incf (block-stats-read-bytes stats) (bio-size bio)))
          (progn
            (incf (block-stats-writes stats))
            (incf (block-stats-write-bytes stats) (bio-size bio)))))

    ;; Submit to scheduler
    (with-spinlock ((request-queue-queue-lock queue))
      (scheduler-insert-bio queue bio))

    ;; Kick off request processing
    (process-request-queue queue)

    t))

;;; I/O Schedulers

(defun scheduler-insert-bio (queue bio)
  "Insert BIO into scheduler."
  (declare (type request-queue queue)
           (type bio bio))

  (case (request-queue-scheduler-type queue)
    (#.+iosched-noop+
     (noop-insert-bio queue bio))

    (#.+iosched-deadline+
     (deadline-insert-bio queue bio))

    (#.+iosched-cfq+
     (cfq-insert-bio queue bio))

    (t
     (noop-insert-bio queue bio))))

(defun noop-insert-bio (queue bio)
  "NOOP scheduler: simple FIFO."
  (declare (type request-queue queue)
           (type bio bio))

  ;; Try to merge with existing requests
  (let ((merged nil))
    (dolist (req (request-queue-requests queue))
      (when (request-can-merge req bio)
        (request-merge-bio req bio)
        (setf merged t)
        (return)))

    (unless merged
      ;; Create new request
      (let ((req (request-alloc queue)))
        (request-add-bio req bio)
        (setf (request-device req) (bio-device bio))
        (setf (request-flags req) (logior (request-flags req) +req-queued+))
        (setf (request-queue-requests queue)
              (append (request-queue-requests queue) (list req)))))))

(defun deadline-insert-bio (queue bio)
  "Deadline scheduler: sorted by sector with read/write deadlines."
  (declare (type request-queue queue)
           (type bio bio))

  (let ((sched-data (request-queue-scheduler-data queue)))

    ;; Try to merge
    (let ((merged nil))
      (dolist (req (deadline-data-sort-list sched-data))
        (when (request-can-merge req bio)
          (request-merge-bio req bio)
          (setf merged t)
          (return)))

      (unless merged
        ;; Create new request
        (let ((req (request-alloc queue)))
          (request-add-bio req bio)
          (setf (request-device req) (bio-device bio))
          (setf (request-flags req) (logior (request-flags req) +req-queued+))

          ;; Add to sort list (sorted by sector)
          (setf (deadline-data-sort-list sched-data)
                (merge 'list
                       (list req)
                       (deadline-data-sort-list sched-data)
                       #'<
                       :key #'request-sector))

          ;; Add to FIFO list (for deadline enforcement)
          (if (logtest (bio-flags bio) +bio-read+)
              (push req (deadline-data-read-fifo sched-data))
              (push req (deadline-data-write-fifo sched-data))))))))

(defun cfq-insert-bio (queue bio)
  "CFQ scheduler: fair queuing with per-process queues."
  (declare (type request-queue queue)
           (type bio bio))

  (let* ((sched-data (request-queue-scheduler-data queue))
         (pid (get-current-pid))
         (proc-queue (gethash pid (cfq-data-queues sched-data))))

    ;; Create per-process queue if needed
    (unless proc-queue
      (setf proc-queue nil)
      (setf (gethash pid (cfq-data-queues sched-data)) proc-queue))

    ;; Try to merge
    (let ((merged nil))
      (dolist (req proc-queue)
        (when (request-can-merge req bio)
          (request-merge-bio req bio)
          (setf merged t)
          (return)))

      (unless merged
        ;; Create new request
        (let ((req (request-alloc queue)))
          (request-add-bio req bio)
          (setf (request-device req) (bio-device bio))
          (setf (request-flags req) (logior (request-flags req) +req-queued+))

          ;; Add to per-process queue
          (push req proc-queue)
          (setf (gethash pid (cfq-data-queues sched-data)) proc-queue))))))

;;; Request Processing

(defun process-request-queue (queue)
  "Process requests in queue."
  (declare (type request-queue queue))

  (with-spinlock ((request-queue-queue-lock queue))
    (loop
      (let ((req (scheduler-get-next-request queue)))
        (unless req
          (return))

        ;; Mark request as started
        (setf (request-flags req)
              (logior (request-flags req) +req-started+))

        ;; Add to in-flight list
        (push req (request-queue-in-flight queue))

        ;; Update statistics
        (let ((stats (request-queue-stats queue)))
          (incf (block-stats-in-flight stats)))

        ;; Dispatch to device
        (dispatch-request queue req)))))

(defun scheduler-get-next-request (queue)
  "Get next request from scheduler."
  (declare (type request-queue queue))

  (case (request-queue-scheduler-type queue)
    (#.+iosched-noop+
     (pop (request-queue-requests queue)))

    (#.+iosched-deadline+
     (deadline-get-next-request queue))

    (#.+iosched-cfq+
     (cfq-get-next-request queue))

    (t
     (pop (request-queue-requests queue)))))

(defun deadline-get-next-request (queue)
  "Get next request from deadline scheduler."
  (declare (type request-queue queue))

  (let ((sched-data (request-queue-scheduler-data queue))
        (now (get-tick-count)))

    ;; Check for expired read requests (reads have priority)
    (when (deadline-data-read-fifo sched-data)
      (let ((req (first (deadline-data-read-fifo sched-data))))
        (when (>= (- now (request-start-time req))
                 (deadline-data-read-expire sched-data))
          (pop (deadline-data-read-fifo sched-data))
          (setf (deadline-data-sort-list sched-data)
                (remove req (deadline-data-sort-list sched-data)))
          (return-from deadline-get-next-request req))))

    ;; Check for expired write requests
    (when (deadline-data-write-fifo sched-data)
      (let ((req (first (deadline-data-write-fifo sched-data))))
        (when (>= (- now (request-start-time req))
                 (deadline-data-write-expire sched-data))
          (pop (deadline-data-write-fifo sched-data))
          (setf (deadline-data-sort-list sched-data)
                (remove req (deadline-data-sort-list sched-data)))
          (return-from deadline-get-next-request req))))

    ;; Otherwise take from sorted list
    (pop (deadline-data-sort-list sched-data))))

(defun cfq-get-next-request (queue)
  "Get next request from CFQ scheduler."
  (declare (type request-queue queue))

  (let ((sched-data (request-queue-scheduler-data queue)))
    ;; Simple round-robin between process queues
    (maphash (lambda (pid proc-queue)
               (declare (ignore pid))
               (when proc-queue
                 (let ((req (pop proc-queue)))
                   (when req
                     (return-from cfq-get-next-request req)))))
             (cfq-data-queues sched-data))
    nil))

(defun dispatch-request (queue req)
  "Dispatch request to device driver."
  (declare (type request-queue queue)
           (type request req))

  (let* ((device (request-device req))
         (ops (block-device-ops device)))

    (when (and ops (block-device-ops-request-fn ops))
      (funcall (block-device-ops-request-fn ops) req))))

(defun complete-request (req error)
  "Complete request."
  (declare (type request req)
           (type (signed-byte 32) error))

  (let* ((device (request-device req))
         (queue (block-device-queue device))
         (duration (- (get-tick-count) (request-start-time req))))

    ;; Mark as completed
    (setf (request-flags req)
          (logior (request-flags req) +req-completed+))

    ;; Remove from in-flight
    (with-spinlock ((request-queue-queue-lock queue))
      (setf (request-queue-in-flight queue)
            (remove req (request-queue-in-flight queue))))

    ;; Update statistics
    (let ((stats (request-queue-stats queue)))
      (decf (block-stats-in-flight stats))
      (incf (block-stats-io-ticks stats) duration)
      (incf (block-stats-time-in-queue stats) duration))

    ;; Complete all BIOs in request
    (let ((bio (request-bio req)))
      (loop while bio do
        (bio-endio bio error)
        (setf bio (bio-next bio))))

    t))

;;; Synchronous I/O Completion

(defstruct bio-completion
  "Synchronous BIO completion tracker."
  (done nil :type t)
  (semaphore nil :type t)
  (error 0 :type (signed-byte 32)))

(defun bio-wait-completion (bio-comp)
  "Wait for BIO to complete."
  (declare (type bio-completion bio-comp))
  ;; Wait on semaphore (blocks until completion)
  (semaphore-wait (bio-completion-semaphore bio-comp))
  (bio-completion-error bio-comp))

(defun bio-complete-sync (bio)
  "Completion callback for synchronous BIO."
  (declare (type bio bio))
  (let ((bio-comp (bio-private bio)))
    (when bio-comp
      (setf (bio-completion-done bio-comp) t)
      (setf (bio-completion-error bio-comp) (bio-error bio))
      ;; Signal completion
      (semaphore-signal (bio-completion-semaphore bio-comp)))))

;;; High-Level Block I/O

(defun block-device-read (device sector buffer length)
  "Read from block device (synchronous)."
  (declare (type block-device device)
           (type (unsigned-byte 64) sector buffer)
           (type (unsigned-byte 32) length))

  (let* ((bio-comp (make-bio-completion
                    :semaphore (make-semaphore 0)))
         (bio (bio-alloc 1)))
    (setf (bio-device bio) device)
    (setf (bio-sector bio) sector)
    (setf (bio-flags bio) +bio-read+)
    (setf (bio-end-io bio) #'bio-complete-sync)
    (setf (bio-private bio) bio-comp)

    ;; Add buffer as single segment
    (bio-add-page bio buffer 0 length)

    ;; Submit BIO
    (block-device-submit-bio bio)

    ;; Wait for completion
    (bio-wait-completion bio-comp)))

(defun block-device-write (device sector buffer length)
  "Write to block device (synchronous)."
  (declare (type block-device device)
           (type (unsigned-byte 64) sector buffer)
           (type (unsigned-byte 32) length))

  (let* ((bio-comp (make-bio-completion
                    :semaphore (make-semaphore 0)))
         (bio (bio-alloc 1)))
    (setf (bio-device bio) device)
    (setf (bio-sector bio) sector)
    (setf (bio-flags bio) +bio-write+)
    (setf (bio-end-io bio) #'bio-complete-sync)
    (setf (bio-private bio) bio-comp)

    ;; Add buffer as single segment
    (bio-add-page bio buffer 0 length)

    ;; Submit BIO
    (block-device-submit-bio bio)

    ;; Wait for completion
    (bio-wait-completion bio-comp)))

(defun block-device-read-async (device sector buffer length callback callback-data)
  "Read from block device (asynchronous)."
  (declare (type block-device device)
           (type (unsigned-byte 64) sector buffer)
           (type (unsigned-byte 32) length)
           (type function callback))

  (let ((bio (bio-alloc 1)))
    (setf (bio-device bio) device)
    (setf (bio-sector bio) sector)
    (setf (bio-flags bio) +bio-read+)
    (setf (bio-end-io bio) callback)
    (setf (bio-private bio) callback-data)

    ;; Add buffer as single segment
    (bio-add-page bio buffer 0 length)

    ;; Submit BIO (returns immediately)
    (block-device-submit-bio bio)
    bio))

(defun block-device-write-async (device sector buffer length callback callback-data)
  "Write to block device (asynchronous)."
  (declare (type block-device device)
           (type (unsigned-byte 64) sector buffer)
           (type (unsigned-byte 32) length)
           (type function callback))

  (let ((bio (bio-alloc 1)))
    (setf (bio-device bio) device)
    (setf (bio-sector bio) sector)
    (setf (bio-flags bio) +bio-write+)
    (setf (bio-end-io bio) callback)
    (setf (bio-private bio) callback-data)

    ;; Add buffer as single segment
    (bio-add-page bio buffer 0 length)

    ;; Submit BIO (returns immediately)
    (block-device-submit-bio bio)
    bio))

(defun block-device-flush (device)
  "Flush block device cache."
  (declare (type block-device device))

  (let ((bio (bio-alloc 0)))
    (setf (bio-device bio) device)
    (setf (bio-flags bio) +bio-flush+)

    (block-device-submit-bio bio)
    t))

;;; BIO Advanced Operations

(defun bio-clone (bio)
  "Clone a BIO for splitting or retry."
  (declare (type bio bio))
  (let ((clone (make-bio
                :id (atomic-incf *bio-id-counter*)
                :device (bio-device bio)
                :sector (bio-sector bio)
                :size (bio-size bio)
                :flags (bio-flags bio)
                :vcnt (bio-vcnt bio)
                :end-io (bio-end-io bio)
                :private (bio-private bio))))
    ;; Copy all bio-vecs
    (dotimes (i (bio-vcnt bio))
      (setf (aref (bio-vecs clone) i)
            (let ((vec (aref (bio-vecs bio) i)))
              (make-bio-vec
               :page (bio-vec-page vec)
               :length (bio-vec-length vec)
               :offset (bio-vec-offset vec)))))
    clone))

(defun bio-split (bio max-sectors)
  "Split BIO into smaller pieces."
  (declare (type bio bio)
           (type (unsigned-byte 32) max-sectors))

  (let ((block-size (block-device-block-size (bio-device bio)))
        (max-bytes (* max-sectors block-size))
        (result nil))

    (when (<= (bio-size bio) max-bytes)
      ;; No split needed
      (return-from bio-split (list bio)))

    ;; Create multiple BIOs
    (let ((remaining-size (bio-size bio))
          (current-sector (bio-sector bio))
          (vec-idx 0)
          (vec-offset 0))

      (loop while (> remaining-size 0) do
        (let* ((chunk-size (min remaining-size max-bytes))
               (chunk-bio (make-bio
                           :id (atomic-incf *bio-id-counter*)
                           :device (bio-device bio)
                           :sector current-sector
                           :size chunk-size
                           :flags (bio-flags bio)
                           :end-io (bio-end-io bio))))

          ;; Copy segments for this chunk
          (let ((bytes-copied 0))
            (loop while (< bytes-copied chunk-size) do
              (let* ((vec (aref (bio-vecs bio) vec-idx))
                     (vec-remaining (- (bio-vec-length vec) vec-offset))
                     (to-copy (min vec-remaining (- chunk-size bytes-copied))))

                (bio-add-page chunk-bio
                             (bio-vec-page vec)
                             (+ (bio-vec-offset vec) vec-offset)
                             to-copy)

                (incf bytes-copied to-copy)
                (incf vec-offset to-copy)

                (when (>= vec-offset (bio-vec-length vec))
                  (incf vec-idx)
                  (setf vec-offset 0)))))

          (push chunk-bio result)
          (decf remaining-size chunk-size)
          (incf current-sector (floor chunk-size block-size)))))

    (nreverse result)))

;;; I/O Error Handling and Retry

(defconstant +max-bio-retries+ 5)

(defun retry-bio (bio)
  "Retry failed BIO."
  (declare (type bio bio))
  (let ((retries (or (getf (bio-private bio) :retries) 0)))
    (when (< retries +max-bio-retries+)
      ;; Increment retry count
      (setf (getf (bio-private bio) :retries) (1+ retries))
      ;; Resubmit
      (block-device-submit-bio bio)
      t)))

;;; Read-Ahead Support

(defstruct readahead-state
  "Read-ahead state for sequential access detection."
  (last-sector 0 :type (unsigned-byte 64))
  (sequential-count 0 :type (unsigned-byte 32))
  (window-size 32 :type (unsigned-byte 32))
  (max-window-size 256 :type (unsigned-byte 32)))

(defun readahead-detect-sequential (ra-state sector)
  "Detect sequential access pattern."
  (declare (type readahead-state ra-state)
           (type (unsigned-byte 64) sector))

  (let ((expected-next (+ (readahead-state-last-sector ra-state)
                         (readahead-state-window-size ra-state))))
    (if (<= (abs (- sector expected-next)) 8)
        ;; Sequential access detected
        (progn
          (incf (readahead-state-sequential-count ra-state))
          ;; Grow window
          (when (< (readahead-state-window-size ra-state)
                  (readahead-state-max-window-size ra-state))
            (setf (readahead-state-window-size ra-state)
                  (* (readahead-state-window-size ra-state) 2)))
          t)
        ;; Random access - reset
        (progn
          (setf (readahead-state-sequential-count ra-state) 0)
          (setf (readahead-state-window-size ra-state) 32)
          nil))))

(defun readahead-submit (device sector count)
  "Submit read-ahead request."
  (declare (type block-device device)
           (type (unsigned-byte 64) sector)
           (type (unsigned-byte 32) count))

  ;; Allocate buffer for read-ahead (will be cached)
  (let* ((block-size (block-device-block-size device))
         (total-size (* count block-size))
         (buffer (allocate-buffer total-size)))

    (when (zerop buffer)
      (return-from readahead-submit nil))

    ;; Submit async read
    (block-device-read-async device sector buffer total-size
                             #'readahead-complete buffer)
    t))

(defun readahead-complete (bio)
  "Read-ahead completion callback."
  (declare (type bio bio))
  (let ((buffer (bio-private bio)))
    ;; In production, buffer would be added to page cache
    ;; For now just free it
    (when buffer
      (free-buffer buffer (bio-size bio)))))

;;; Partition Support

(defconstant +gpt-signature+ #x5452415020494645) ; "EFI PART"
(defconstant +mbr-signature+ #xAA55)

(defun make-partition-table (device)
  "Read and parse partition table (MBR and GPT)."
  (declare (type block-device device))

  ;; Read MBR (sector 0)
  (let ((mbr-buffer (allocate-buffer 512)))
    (block-device-read device 0 mbr-buffer 512)

    ;; Check MBR signature
    (let ((signature (read-u16-le mbr-buffer 510)))
      (unless (= signature +mbr-signature+)
        (free-buffer mbr-buffer 512)
        (return-from make-partition-table nil)))

    ;; Check for GPT protective MBR
    (let ((part0-type (read-u8 mbr-buffer (+ 446 4))))
      (if (= part0-type #xEE)
          ;; GPT partition table
          (progn
            (free-buffer mbr-buffer 512)
            (parse-gpt-partitions device))
          ;; MBR partition table
          (progn
            (let ((partitions (parse-mbr-partitions mbr-buffer)))
              (free-buffer mbr-buffer 512)
              (setf (block-device-partitions device) partitions)
              partitions))))))

(defun parse-mbr-partitions (mbr-buffer)
  "Parse MBR partition table."
  (declare (type (unsigned-byte 64) mbr-buffer))

  (let ((partitions nil))
    (dotimes (i 4)
      (let ((offset (+ 446 (* i 16))))
        (let ((part-type (read-u8 mbr-buffer (+ offset 4)))
              (start-lba (read-u32-le mbr-buffer (+ offset 8)))
              (num-sectors (read-u32-le mbr-buffer (+ offset 12))))

          (unless (zerop part-type)
            (let ((part (make-partition
                         :number i
                         :start-sector start-lba
                         :nr-sectors num-sectors
                         :type part-type)))
              (push part partitions))))))
    (nreverse partitions)))

(defun parse-gpt-partitions (device)
  "Parse GPT partition table."
  (declare (type block-device device))

  ;; Read GPT header (sector 1)
  (let ((gpt-buffer (allocate-buffer 512)))
    (block-device-read device 1 gpt-buffer 512)

    ;; Verify GPT signature
    (let ((signature (read-u64-le gpt-buffer 0)))
      (unless (= signature +gpt-signature+)
        (free-buffer gpt-buffer 512)
        (return-from parse-gpt-partitions nil)))

    ;; Parse GPT header
    (let ((partition-entry-lba (read-u64-le gpt-buffer 72))
          (num-partition-entries (read-u32-le gpt-buffer 80))
          (partition-entry-size (read-u32-le gpt-buffer 84)))

      ;; Read partition entries
      (let ((entries-per-sector (floor 512 partition-entry-size))
            (partitions nil))

        (dotimes (i num-partition-entries)
          (let* ((entry-sector (+ partition-entry-lba (floor i entries-per-sector)))
                 (entry-offset (* (mod i entries-per-sector) partition-entry-size))
                 (entry-buffer (allocate-buffer 512)))

            (block-device-read device entry-sector entry-buffer 512)

            ;; Read partition type GUID (first 16 bytes)
            (let ((type-guid-low (read-u64-le entry-buffer entry-offset))
                  (type-guid-high (read-u64-le entry-buffer (+ entry-offset 8))))

              ;; Check if partition exists (non-zero GUID)
              (when (or (not (zerop type-guid-low))
                       (not (zerop type-guid-high)))
                (let ((first-lba (read-u64-le entry-buffer (+ entry-offset 32)))
                      (last-lba (read-u64-le entry-buffer (+ entry-offset 40))))

                  (let ((part (make-partition
                               :number i
                               :start-sector first-lba
                               :nr-sectors (- last-lba first-lba -1)
                               :type 0))) ; GPT doesn't use MBR type codes
                    (push part partitions)))))

            (free-buffer entry-buffer 512)))

        (free-buffer gpt-buffer 512)
        (setf (block-device-partitions device) (nreverse partitions))
        partitions))))

;;; Utility Functions

(defun allocate-buffer (size)
  "Allocate DMA-capable buffer for block I/O."
  (declare (type (unsigned-byte 32) size))
  ;; Allocate from kernel heap with proper alignment for DMA
  ;; PowerISA requires 128-byte alignment for optimal DMA
  (let ((aligned-size (+ size 128)))  ; Add space for alignment
    (let ((raw-addr (kmalloc aligned-size)))
      (when (zerop raw-addr)
        (return-from allocate-buffer 0))
      ;; Align to 128-byte boundary
      (let ((aligned-addr (logand (+ raw-addr 127) (lognot 127))))
        aligned-addr))))

(defun free-buffer (buffer size)
  "Free block I/O buffer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) size))
  (declare (ignore size))
  ;; Note: In production, we'd need to track the original allocation
  ;; to properly free the unaligned pointer
  (kfree buffer))

(defun read-u8 (buffer offset)
  "Read 8-bit unsigned value from buffer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (mem-read-u8 (+ buffer offset)))

(defun read-u16-le (buffer offset)
  "Read 16-bit little-endian value from buffer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (let ((addr (+ buffer offset)))
    (logior (mem-read-u8 addr)
            (ash (mem-read-u8 (+ addr 1)) 8))))

(defun read-u32-le (buffer offset)
  "Read 32-bit little-endian value from buffer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (let ((addr (+ buffer offset)))
    (logior (mem-read-u8 addr)
            (ash (mem-read-u8 (+ addr 1)) 8)
            (ash (mem-read-u8 (+ addr 2)) 16)
            (ash (mem-read-u8 (+ addr 3)) 24))))

(defun read-u64-le (buffer offset)
  "Read 64-bit little-endian value from buffer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (let ((addr (+ buffer offset)))
    (logior (mem-read-u8 addr)
            (ash (mem-read-u8 (+ addr 1)) 8)
            (ash (mem-read-u8 (+ addr 2)) 16)
            (ash (mem-read-u8 (+ addr 3)) 24)
            (ash (mem-read-u8 (+ addr 4)) 32)
            (ash (mem-read-u8 (+ addr 5)) 40)
            (ash (mem-read-u8 (+ addr 6)) 48)
            (ash (mem-read-u8 (+ addr 7)) 56))))

(defun write-u8 (buffer offset value)
  "Write 8-bit value to buffer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset)
           (type (unsigned-byte 8) value))
  (mem-write-u8 (+ buffer offset) value))

(defun write-u32-le (buffer offset value)
  "Write 32-bit little-endian value to buffer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset value))
  (let ((addr (+ buffer offset)))
    (mem-write-u8 addr (logand value #xFF))
    (mem-write-u8 (+ addr 1) (logand (ash value -8) #xFF))
    (mem-write-u8 (+ addr 2) (logand (ash value -16) #xFF))
    (mem-write-u8 (+ addr 3) (logand (ash value -24) #xFF))))

(defun write-u64-le (buffer offset value)
  "Write 64-bit little-endian value to buffer."
  (declare (type (unsigned-byte 64) buffer value)
           (type (unsigned-byte 32) offset))
  (let ((addr (+ buffer offset)))
    (mem-write-u8 addr (logand value #xFF))
    (mem-write-u8 (+ addr 1) (logand (ash value -8) #xFF))
    (mem-write-u8 (+ addr 2) (logand (ash value -16) #xFF))
    (mem-write-u8 (+ addr 3) (logand (ash value -24) #xFF))
    (mem-write-u8 (+ addr 4) (logand (ash value -32) #xFF))
    (mem-write-u8 (+ addr 5) (logand (ash value -40) #xFF))
    (mem-write-u8 (+ addr 6) (logand (ash value -48) #xFF))
    (mem-write-u8 (+ addr 7) (logand (ash value -56) #xFF))))

;;; Block Device Statistics and Monitoring

(defun block-device-get-stats (device)
  "Get block device statistics."
  (declare (type block-device device))
  (let ((queue (block-device-queue device)))
    (when queue
      (request-queue-stats queue))))

(defun block-device-reset-stats (device)
  "Reset block device statistics."
  (declare (type block-device device))
  (let ((queue (block-device-queue device)))
    (when queue
      (setf (request-queue-stats queue)
            (make-block-stats)))))

(defun block-device-print-stats (device)
  "Print block device statistics."
  (declare (type block-device device))
  (let ((stats (block-device-get-stats device)))
    (when stats
      (format t "Block Device Statistics for ~A:~%" (block-device-name device))
      (format t "  Reads:       ~D (~D bytes)~%"
              (block-stats-reads stats)
              (block-stats-read-bytes stats))
      (format t "  Writes:      ~D (~D bytes)~%"
              (block-stats-writes stats)
              (block-stats-write-bytes stats))
      (format t "  In-flight:   ~D~%"
              (block-stats-in-flight stats))
      (format t "  I/O ticks:   ~D~%"
              (block-stats-io-ticks stats))
      (format t "  Queue time:  ~D~%"
              (block-stats-time-in-queue stats)))))

;;; I/O Barrier Support

(defun block-device-barrier (device callback)
  "Submit I/O barrier - ensures all prior I/O completes first."
  (declare (type block-device device)
           (type (or null function) callback))

  (let ((bio (bio-alloc 0)))
    (setf (bio-device bio) device)
    (setf (bio-flags bio) (logior +bio-flush+ +bio-sync+))
    (setf (bio-end-io bio) callback)

    (block-device-submit-bio bio)
    bio))

(defun wait-for-all-io (queue)
  "Wait for all in-flight I/O to complete."
  (declare (type request-queue queue))

  ;; Poll until all in-flight requests complete
  (loop while (request-queue-in-flight queue) do
    (sleep-ms 1))
  t)

;;; Queue Management and Tuning

(defun block-queue-set-max-sectors (queue max-sectors)
  "Set maximum sectors per request."
  (declare (type request-queue queue)
           (type (unsigned-byte 32) max-sectors))

  (with-spinlock ((request-queue-queue-lock queue))
    (setf (request-queue-max-sectors queue)
          (min max-sectors (request-queue-max-hw-sectors queue)))))

(defun block-queue-set-scheduler (queue scheduler-type)
  "Change I/O scheduler for queue."
  (declare (type request-queue queue)
           (type (unsigned-byte 8) scheduler-type))

  (with-spinlock ((request-queue-queue-lock queue))
    ;; Drain existing requests first
    (wait-for-all-io queue)

    ;; Clear old scheduler data
    (setf (request-queue-scheduler-data queue) nil)

    ;; Initialize new scheduler
    (case scheduler-type
      (#.+iosched-deadline+
       (setf (request-queue-scheduler-data queue)
             (make-deadline-data)))

      (#.+iosched-cfq+
       (setf (request-queue-scheduler-data queue)
             (make-cfq-data)))

      (#.+iosched-noop+
       nil))

    (setf (request-queue-scheduler-type queue) scheduler-type)
    t))

;;; Multi-sector Sequential I/O Helpers

(defun block-device-read-sectors (device start-sector count buffer)
  "Read multiple contiguous sectors."
  (declare (type block-device device)
           (type (unsigned-byte 64) start-sector)
           (type (unsigned-byte 32) count)
           (type (unsigned-byte 64) buffer))

  (let* ((block-size (block-device-block-size device))
         (total-bytes (* count block-size)))
    (block-device-read device start-sector buffer total-bytes)))

(defun block-device-write-sectors (device start-sector count buffer)
  "Write multiple contiguous sectors."
  (declare (type block-device device)
           (type (unsigned-byte 64) start-sector)
           (type (unsigned-byte 32) count)
           (type (unsigned-byte 64) buffer))

  (let* ((block-size (block-device-block-size device))
         (total-bytes (* count block-size)))
    (block-device-write device start-sector buffer total-bytes)))

;;; Forward Declarations for External Dependencies

(defun make-mutex ()
  "Create mutex (implemented in kernel/core/mutex.lisp)."
  (error "make-mutex not yet linked"))

(defmacro with-mutex ((mutex) &body body)
  "Execute body with mutex held."
  `(progn ,@body))

(defun make-spinlock ()
  "Create spinlock (implemented in kernel/hal/interrupts.lisp)."
  (error "make-spinlock not yet linked"))

(defmacro with-spinlock ((lock) &body body)
  "Execute body with spinlock held."
  `(progn ,@body))

(defun atomic-incf (place)
  "Atomic increment (implemented via FFI to PowerISA lwarx/stwcx)."
  (incf place))

(defun get-tick-count ()
  "Get system tick count in milliseconds (from kernel/core/time.lisp)."
  0)

(defun get-current-pid ()
  "Get current process ID (from kernel/core/process.lisp)."
  1)

(defun kmalloc (size)
  "Allocate kernel memory (from kernel/core/heap.lisp)."
  (declare (type (unsigned-byte 32) size))
  (declare (ignore size))
  0)

(defun kfree (ptr)
  "Free kernel memory (from kernel/core/heap.lisp)."
  (declare (type (unsigned-byte 64) ptr))
  (declare (ignore ptr))
  nil)

(defun mem-read-u8 (addr)
  "Read byte from physical memory."
  (declare (type (unsigned-byte 64) addr))
  (declare (ignore addr))
  0)

(defun mem-write-u8 (addr value)
  "Write byte to physical memory."
  (declare (type (unsigned-byte 64) addr)
           (type (unsigned-byte 8) value))
  (declare (ignore addr value))
  nil)

(defun make-semaphore (initial-value)
  "Create semaphore (from kernel/core/semaphore.lisp)."
  (declare (type (unsigned-byte 32) initial-value))
  (declare (ignore initial-value))
  nil)

(defun semaphore-wait (sem)
  "Wait on semaphore (from kernel/core/semaphore.lisp)."
  (declare (ignore sem))
  nil)

(defun semaphore-signal (sem)
  "Signal semaphore (from kernel/core/semaphore.lisp)."
  (declare (ignore sem))
  nil)

(defun sleep-ms (ms)
  "Sleep for milliseconds (from kernel/core/time.lisp)."
  (declare (type (unsigned-byte 32) ms))
  (declare (ignore ms))
  nil)

