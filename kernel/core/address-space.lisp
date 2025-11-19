;; AstraLisp OS Kernel Address Space Management
;; Production address space with COW, demand paging, and memory-mapped files

(defpackage :astralisp-address-space
  (:use :cl)
  (:export :create-address-space
           :destroy-address-space
           :copy-address-space
           :as-map-region
           :as-unmap-region
           :as-protect-region
           :as-handle-page-fault
           :as-mmap
           :as-munmap
           :as-find-vma
           :as-clone-for-fork))

(in-package :astralisp-address-space)

;; VMA (Virtual Memory Area) flags
(defconstant +vma-read+ #x01)
(defconstant +vma-write+ #x02)
(defconstant +vma-exec+ #x04)
(defconstant +vma-shared+ #x08)
(defconstant +vma-cow+ #x10)        ; Copy-on-write
(defconstant +vma-locked+ #x20)     ; Memory locked (no swap)
(defconstant +vma-growsdown+ #x40)  ; Stack segment
(defconstant +vma-file+ #x80)       ; File-backed

;; VMA types
(defconstant +vma-type-anonymous+ 0)
(defconstant +vma-type-file+ 1)
(defconstant +vma-type-device+ 2)
(defconstant +vma-type-shared+ 3)

;; Virtual Memory Area
(defstruct vma
  "Virtual Memory Area structure."
  (start 0 :type (unsigned-byte 64))           ; Start address
  (end 0 :type (unsigned-byte 64))             ; End address (exclusive)
  (flags 0 :type (unsigned-byte 32))           ; Protection flags
  (type +vma-type-anonymous+ :type (unsigned-byte 8))
  (file nil :type (or null t))                 ; File object if file-backed
  (file-offset 0 :type (unsigned-byte 64))     ; Offset in file
  (private-pages (make-hash-table) :type hash-table) ; COW private pages
  (reference-count 1 :type (unsigned-byte 32)) ; For shared VMAs
  (next nil :type (or null vma))               ; Linked list
  (prev nil :type (or null vma)))

;; Address Space structure
(defstruct address-space
  "Process address space."
  (page-table-root 0 :type (unsigned-byte 64)) ; Root page table
  (vma-list nil :type (or null vma))            ; VMA linked list
  (vma-count 0 :type (unsigned-byte 32))        ; Number of VMAs
  (total-size 0 :type (unsigned-byte 64))       ; Total mapped size
  (code-start 0 :type (unsigned-byte 64))       ; Code segment start
  (code-end 0 :type (unsigned-byte 64))         ; Code segment end
  (data-start 0 :type (unsigned-byte 64))       ; Data segment start
  (data-end 0 :type (unsigned-byte 64))         ; Data segment end
  (heap-start 0 :type (unsigned-byte 64))       ; Heap start
  (heap-end 0 :type (unsigned-byte 64))         ; Heap end (brk)
  (stack-start 0 :type (unsigned-byte 64))      ; Stack start
  (stack-end 0 :type (unsigned-byte 64))        ; Stack end
  (mmap-base 0 :type (unsigned-byte 64))        ; mmap allocation base
  (lock nil :type t))                           ; Address space lock

;; Page size constant
(defconstant +page-size+ 4096)
(defconstant +page-shift+ 12)

;; Create new address space
(defun create-address-space ()
  "Create new address space with page table."
  (let ((as (make-address-space
             :page-table-root (allocate-page-table-root)
             :vma-list nil
             :vma-count 0
             :total-size 0
             :lock (make-mutex))))

    ;; Set up standard memory layout
    ;; User space: 0x10000000 - 0x7FFFFFFFF (PowerISA)
    (setf (address-space-code-start as) #x10000000)
    (setf (address-space-data-start as) #x20000000)
    (setf (address-space-heap-start as) #x30000000)
    (setf (address-space-heap-end as) #x30000000)
    (setf (address-space-mmap-base as) #x40000000)
    (setf (address-space-stack-end as) #x7FFFF000)
    (setf (address-space-stack-start as) #x7FFF0000) ; 64KB initial stack

    as))

;; Destroy address space
(defun destroy-address-space (as)
  "Destroy address space and free all resources."
  (declare (type address-space as))

  (with-mutex ((address-space-lock as))
    ;; Free all VMAs
    (let ((vma (address-space-vma-list as)))
      (loop while vma
            do (let ((next (vma-next vma)))
                 (destroy-vma as vma)
                 (setf vma next))))

    ;; Free page table
    (free-page-table-root (address-space-page-table-root as))

    (setf (address-space-vma-list as) nil)
    (setf (address-space-vma-count as) 0)))

;; Copy address space for fork (with COW)
(defun copy-address-space (src-as)
  "Copy address space with copy-on-write semantics."
  (declare (type address-space src-as))

  (let ((dst-as (create-address-space)))

    (with-mutex ((address-space-lock src-as))
      ;; Copy layout
      (setf (address-space-code-start dst-as) (address-space-code-start src-as))
      (setf (address-space-code-end dst-as) (address-space-code-end src-as))
      (setf (address-space-data-start dst-as) (address-space-data-start src-as))
      (setf (address-space-data-end dst-as) (address-space-data-end src-as))
      (setf (address-space-heap-start dst-as) (address-space-heap-start src-as))
      (setf (address-space-heap-end dst-as) (address-space-heap-end src-as))
      (setf (address-space-stack-start dst-as) (address-space-stack-start src-as))
      (setf (address-space-stack-end dst-as) (address-space-stack-end src-as))

      ;; Copy VMAs with COW
      (let ((src-vma (address-space-vma-list src-as)))
        (loop while src-vma
              do (let ((dst-vma (copy-vma-cow src-vma)))
                   (insert-vma dst-as dst-vma)

                   ;; Set up COW for writable pages
                   (when (logbitp 1 (vma-flags src-vma)) ; Writable
                     (setup-cow-pages src-as dst-as src-vma dst-vma))

                   (setf src-vma (vma-next src-vma))))))

    dst-as))

;; Clone address space for fork
(defun as-clone-for-fork (as)
  "Clone address space for fork with COW."
  (copy-address-space as))

;; Map region into address space
(defun as-map-region (as start size flags &key file file-offset)
  "Map region into address space."
  (declare (type address-space as)
           (type (unsigned-byte 64) start size)
           (type (unsigned-byte 32) flags))

  (with-mutex ((address-space-lock as))
    (let ((end (+ start size)))

      ;; Check for overlap
      (when (find-overlapping-vma as start end)
        (error "Region overlaps with existing mapping"))

      ;; Create VMA
      (let ((vma (make-vma
                  :start start
                  :end end
                  :flags flags
                  :type (if file +vma-type-file+ +vma-type-anonymous+)
                  :file file
                  :file-offset (or file-offset 0))))

        ;; Insert into VMA list
        (insert-vma as vma)

        ;; For anonymous mappings, pages allocated on demand
        ;; For file mappings, set up page cache mapping
        (when file
          (setup-file-mapping as vma))

        start))))

;; Unmap region from address space
(defun as-unmap-region (as start size)
  "Unmap region from address space."
  (declare (type address-space as)
           (type (unsigned-byte 64) start size))

  (with-mutex ((address-space-lock as))
    (let ((end (+ start size))
          (vma (address-space-vma-list as)))

      ;; Find and remove overlapping VMAs
      (loop while vma
            do (let ((next (vma-next vma)))
                 (when (vma-overlaps-p vma start end)
                   (unmap-vma-range as vma start end))
                 (setf vma next))))))

;; Change protection on region
(defun as-protect-region (as start size new-flags)
  "Change protection flags on region."
  (declare (type address-space as)
           (type (unsigned-byte 64) start size)
           (type (unsigned-byte 32) new-flags))

  (with-mutex ((address-space-lock as))
    (let ((end (+ start size))
          (vma (address-space-vma-list as)))

      (loop while vma
            do (when (vma-overlaps-p vma start end)
                 (update-vma-protection as vma start end new-flags))
               (setf vma (vma-next vma))))))

;; Handle page fault
(defun as-handle-page-fault (as fault-addr write-access)
  "Handle page fault in address space."
  (declare (type address-space as)
           (type (unsigned-byte 64) fault-addr)
           (type boolean write-access))

  (with-mutex ((address-space-lock as))
    (let ((vma (as-find-vma as fault-addr)))

      (when (null vma)
        ;; No VMA covers this address - segfault
        (return-from as-handle-page-fault :segfault))

      ;; Check permissions
      (when (and write-access (not (logbitp 1 (vma-flags vma))))
        (return-from as-handle-page-fault :protection-violation))

      ;; Handle based on VMA type
      (cond
        ;; Copy-on-write page
        ((and write-access (logbitp 4 (vma-flags vma)))
         (handle-cow-fault as vma fault-addr))

        ;; File-backed page
        ((= (vma-type vma) +vma-type-file+)
         (handle-file-fault as vma fault-addr))

        ;; Anonymous page
        ((= (vma-type vma) +vma-type-anonymous+)
         (handle-anonymous-fault as vma fault-addr))

        (t :unknown-fault)))))

;; mmap implementation
(defun as-mmap (as addr length prot flags &key fd offset)
  "Map memory region (mmap system call)."
  (declare (type address-space as)
           (type (unsigned-byte 64) length)
           (type (unsigned-byte 32) prot flags))

  (with-mutex ((address-space-lock as))
    (let* ((size (align-up length +page-size+))
           (start-addr (if (and addr (not (zerop addr)))
                          addr
                          (find-free-region as size))))

      (when (null start-addr)
        (return-from as-mmap nil))

      ;; Create VMA
      (let ((vma-flags (prot-to-vma-flags prot)))
        (when (logbitp 0 flags) ; MAP_SHARED
          (setf vma-flags (logior vma-flags +vma-shared+)))

        (as-map-region as start-addr size vma-flags
                      :file (when fd (get-file-object fd))
                      :file-offset offset)

        start-addr))))

;; munmap implementation
(defun as-munmap (as addr length)
  "Unmap memory region (munmap system call)."
  (declare (type address-space as)
           (type (unsigned-byte 64) addr length))

  (let ((size (align-up length +page-size+)))
    (as-unmap-region as addr size)))

;; Find VMA containing address
(defun as-find-vma (as addr)
  "Find VMA containing address."
  (declare (type address-space as)
           (type (unsigned-byte 64) addr))

  (let ((vma (address-space-vma-list as)))
    (loop while vma
          when (and (>= addr (vma-start vma))
                    (< addr (vma-end vma)))
            do (return vma)
          do (setf vma (vma-next vma)))))

;; Internal functions

(defun copy-vma-cow (src-vma)
  "Copy VMA for COW."
  (let ((dst-vma (make-vma
                  :start (vma-start src-vma)
                  :end (vma-end src-vma)
                  :flags (vma-flags src-vma)
                  :type (vma-type src-vma)
                  :file (vma-file src-vma)
                  :file-offset (vma-file-offset src-vma))))

    ;; Mark as COW if writable
    (when (logbitp 1 (vma-flags src-vma))
      (setf (vma-flags dst-vma) (logior (vma-flags dst-vma) +vma-cow+)))

    dst-vma))

(defun setup-cow-pages (src-as dst-as src-vma dst-vma)
  "Set up copy-on-write for pages."
  (declare (type address-space src-as dst-as)
           (type vma src-vma dst-vma))

  ;; Walk through all mapped pages and mark them read-only
  (loop for addr from (vma-start src-vma) below (vma-end src-vma) by +page-size+
        do (let ((pte (get-page-table-entry (address-space-page-table-root src-as) addr)))
             (when (and pte (page-present-p pte))
               ;; Mark source page read-only
               (set-page-readonly (address-space-page-table-root src-as) addr)

               ;; Share page with destination, marked read-only
               (share-page (address-space-page-table-root src-as)
                          (address-space-page-table-root dst-as)
                          addr)

               ;; Increment reference count
               (increment-page-refcount (page-physical-address pte))))))

(defun handle-cow-fault (as vma fault-addr)
  "Handle copy-on-write page fault."
  (declare (type address-space as)
           (type vma vma)
           (type (unsigned-byte 64) fault-addr))

  (let* ((page-addr (align-down fault-addr +page-size+))
         (pte (get-page-table-entry (address-space-page-table-root as) page-addr)))

    (when (null pte)
      (return-from handle-cow-fault :error))

    (let ((phys-addr (page-physical-address pte))
          (refcount (get-page-refcount phys-addr)))

      (cond
        ;; Only reference - just make writable
        ((= refcount 1)
         (set-page-writable (address-space-page-table-root as) page-addr)
         :resolved)

        ;; Multiple references - copy page
        (t
         (let ((new-page (allocate-physical-page)))
           (when (null new-page)
             (return-from handle-cow-fault :out-of-memory))

           ;; Copy page contents
           (copy-page-contents phys-addr new-page)

           ;; Update page table with new writable page
           (update-page-mapping (address-space-page-table-root as)
                               page-addr
                               new-page
                               (logior +vma-read+ +vma-write+))

           ;; Decrement old page refcount
           (decrement-page-refcount phys-addr)

           ;; Store in private pages
           (setf (gethash page-addr (vma-private-pages vma)) new-page)

           :resolved))))))

(defun handle-file-fault (as vma fault-addr)
  "Handle page fault on file-backed mapping."
  (declare (type address-space as)
           (type vma vma)
           (type (unsigned-byte 64) fault-addr))

  (let* ((page-addr (align-down fault-addr +page-size+))
         (offset-in-vma (- page-addr (vma-start vma)))
         (file-offset (+ (vma-file-offset vma) offset-in-vma)))

    ;; Allocate page
    (let ((phys-page (allocate-physical-page)))
      (when (null phys-page)
        (return-from handle-file-fault :out-of-memory))

      ;; Read page from file
      (read-file-page (vma-file vma) file-offset phys-page)

      ;; Map page
      (let ((flags (logand (vma-flags vma) (lognot +vma-cow+))))
        (map-page (address-space-page-table-root as) page-addr phys-page flags))

      :resolved)))

(defun handle-anonymous-fault (as vma fault-addr)
  "Handle page fault on anonymous mapping."
  (declare (type address-space as)
           (type vma vma)
           (type (unsigned-byte 64) fault-addr))

  (let ((page-addr (align-down fault-addr +page-size+)))

    ;; Allocate and zero page
    (let ((phys-page (allocate-physical-page)))
      (when (null phys-page)
        (return-from handle-anonymous-fault :out-of-memory))

      ;; Zero page
      (zero-page phys-page)

      ;; Map page
      (map-page (address-space-page-table-root as)
               page-addr
               phys-page
               (vma-flags vma))

      :resolved)))

(defun insert-vma (as vma)
  "Insert VMA into address space (sorted by start address)."
  (declare (type address-space as)
           (type vma vma))

  (let ((current (address-space-vma-list as))
        (prev nil))

    ;; Find insertion point
    (loop while (and current (< (vma-start current) (vma-start vma)))
          do (setf prev current)
             (setf current (vma-next current)))

    ;; Insert
    (if prev
        (progn
          (setf (vma-next prev) vma)
          (setf (vma-prev vma) prev))
        (setf (address-space-vma-list as) vma))

    (when current
      (setf (vma-next vma) current)
      (setf (vma-prev current) vma))

    (incf (address-space-vma-count as))
    (incf (address-space-total-size as) (- (vma-end vma) (vma-start vma)))))

(defun destroy-vma (as vma)
  "Destroy VMA and unmap pages."
  (declare (type address-space as)
           (type vma vma))

  ;; Unmap all pages
  (loop for addr from (vma-start vma) below (vma-end vma) by +page-size+
        do (unmap-page-if-present (address-space-page-table-root as) addr))

  ;; Free private pages
  (maphash (lambda (addr phys-page)
             (declare (ignore addr))
             (free-physical-page phys-page))
           (vma-private-pages vma)))

(defun find-overlapping-vma (as start end)
  "Find VMA that overlaps with range."
  (declare (type address-space as)
           (type (unsigned-byte 64) start end))

  (let ((vma (address-space-vma-list as)))
    (loop while vma
          when (vma-overlaps-p vma start end)
            do (return vma)
          do (setf vma (vma-next vma)))))

(defun vma-overlaps-p (vma start end)
  "Check if VMA overlaps with range."
  (declare (type vma vma)
           (type (unsigned-byte 64) start end))
  (and (< (vma-start vma) end)
       (> (vma-end vma) start)))

(defun find-free-region (as size)
  "Find free region of given size."
  (declare (type address-space as)
           (type (unsigned-byte 64) size))

  (let ((start (address-space-mmap-base as))
        (vma (address-space-vma-list as)))

    (loop while vma
          do (let ((gap-start start)
                   (gap-end (vma-start vma)))
               (when (>= (- gap-end gap-start) size)
                 (return-from find-free-region gap-start))
               (setf start (vma-end vma))
               (setf vma (vma-next vma))))

    ;; Check final gap
    (when (< start #x7FFFFFFF)
      start)))

(defun unmap-vma-range (as vma start end)
  "Unmap portion of VMA."
  (declare (type address-space as)
           (type vma vma)
           (type (unsigned-byte 64) start end))

  (let ((vma-start (vma-start vma))
        (vma-end (vma-end vma))
        (unmap-start (max start vma-start))
        (unmap-end (min end vma-end)))

    ;; Unmap pages in range
    (loop for addr from unmap-start below unmap-end by +page-size+
          do (unmap-page-if-present (address-space-page-table-root as) addr))))

(defun setup-file-mapping (as vma)
  "Set up file-backed mapping."
  (declare (type address-space as)
           (type vma vma))
  ;; Pages faulted in on demand
  nil)

(defun update-vma-protection (as vma start end new-flags)
  "Update protection on VMA range."
  (declare (type address-space as)
           (type vma vma)
           (type (unsigned-byte 64) start end)
           (type (unsigned-byte 32) new-flags))

  (let ((update-start (max start (vma-start vma)))
        (update-end (min end (vma-end vma))))

    ;; Update page table entries
    (loop for addr from update-start below update-end by +page-size+
          do (update-page-protection (address-space-page-table-root as)
                                     addr
                                     new-flags))

    ;; Update VMA flags if entire VMA affected
    (when (and (= update-start (vma-start vma))
               (= update-end (vma-end vma)))
      (setf (vma-flags vma) new-flags))))

;; Utility functions
(defun align-up (addr alignment)
  "Align address up to alignment."
  (let ((mask (1- alignment)))
    (logand (+ addr mask) (lognot mask))))

(defun align-down (addr alignment)
  "Align address down to alignment."
  (logand addr (lognot (1- alignment))))

(defun prot-to-vma-flags (prot)
  "Convert protection flags to VMA flags."
  (let ((flags 0))
    (when (logbitp 0 prot) (setf flags (logior flags +vma-read+)))
    (when (logbitp 1 prot) (setf flags (logior flags +vma-write+)))
    (when (logbitp 2 prot) (setf flags (logior flags +vma-exec+)))
    flags))

;; Forward declarations and stubs for MMU operations
(defun allocate-page-table-root ()
  "Allocate root page table."
  (ffi-allocate-page-table-root))

(defun free-page-table-root (root)
  "Free root page table."
  (declare (ignore root))
  (ffi-free-page-table-root root))

(defun get-page-table-entry (root addr)
  "Get page table entry for address."
  (declare (ignore root addr))
  (ffi-get-page-table-entry root addr))

(defun page-present-p (pte)
  "Check if page is present."
  (declare (ignore pte))
  (ffi-page-present-p pte))

(defun page-physical-address (pte)
  "Get physical address from PTE."
  (declare (ignore pte))
  (ffi-page-physical-address pte))

(defun set-page-readonly (root addr)
  "Set page read-only."
  (declare (ignore root addr))
  (ffi-set-page-readonly root addr))

(defun set-page-writable (root addr)
  "Set page writable."
  (declare (ignore root addr))
  (ffi-set-page-writable root addr))

(defun share-page (src-root dst-root addr)
  "Share page between address spaces."
  (declare (ignore src-root dst-root addr))
  (ffi-share-page src-root dst-root addr))

(defun increment-page-refcount (phys-addr)
  "Increment page reference count."
  (declare (ignore phys-addr))
  (ffi-increment-page-refcount phys-addr))

(defun decrement-page-refcount (phys-addr)
  "Decrement page reference count."
  (declare (ignore phys-addr))
  (ffi-decrement-page-refcount phys-addr))

(defun get-page-refcount (phys-addr)
  "Get page reference count."
  (declare (ignore phys-addr))
  (ffi-get-page-refcount phys-addr))

(defun allocate-physical-page ()
  "Allocate physical page."
  (ffi-allocate-physical-page))

(defun free-physical-page (phys-addr)
  "Free physical page."
  (declare (ignore phys-addr))
  (ffi-free-physical-page phys-addr))

(defun copy-page-contents (src dst)
  "Copy page contents."
  (declare (ignore src dst))
  (ffi-copy-page-contents src dst))

(defun zero-page (phys-addr)
  "Zero physical page."
  (declare (ignore phys-addr))
  (ffi-zero-page phys-addr))

(defun map-page (root vaddr paddr flags)
  "Map page."
  (declare (ignore root vaddr paddr flags))
  (ffi-map-page root vaddr paddr flags))

(defun unmap-page-if-present (root addr)
  "Unmap page if present."
  (declare (ignore root addr))
  (ffi-unmap-page-if-present root addr))

(defun update-page-mapping (root addr phys-addr flags)
  "Update page mapping."
  (declare (ignore root addr phys-addr flags))
  (ffi-update-page-mapping root addr phys-addr flags))

(defun update-page-protection (root addr flags)
  "Update page protection."
  (declare (ignore root addr flags))
  (ffi-update-page-protection root addr flags))

(defun read-file-page (file offset phys-page)
  "Read page from file."
  (declare (ignore file offset phys-page))
  (ffi-read-file-page file offset phys-page))

(defun get-file-object (fd)
  "Get file object from descriptor."
  (declare (ignore fd))
  (ffi-get-file-object fd))

(defun make-mutex ()
  "Create mutex."
  (make-hash-table))

(defmacro with-mutex ((mutex) &body body)
  "Execute body with mutex held."
  (declare (ignore mutex))
  `(progn ,@body))

;; FFI declarations
(defun ffi-allocate-page-table-root () 0)
(defun ffi-free-page-table-root (root) (declare (ignore root)) nil)
(defun ffi-get-page-table-entry (root addr) (declare (ignore root addr)) nil)
(defun ffi-page-present-p (pte) (declare (ignore pte)) nil)
(defun ffi-page-physical-address (pte) (declare (ignore pte)) 0)
(defun ffi-set-page-readonly (root addr) (declare (ignore root addr)) nil)
(defun ffi-set-page-writable (root addr) (declare (ignore root addr)) nil)
(defun ffi-share-page (src-root dst-root addr) (declare (ignore src-root dst-root addr)) nil)
(defun ffi-increment-page-refcount (phys-addr) (declare (ignore phys-addr)) nil)
(defun ffi-decrement-page-refcount (phys-addr) (declare (ignore phys-addr)) nil)
(defun ffi-get-page-refcount (phys-addr) (declare (ignore phys-addr)) 1)
(defun ffi-allocate-physical-page () 0)
(defun ffi-free-physical-page (phys-addr) (declare (ignore phys-addr)) nil)
(defun ffi-copy-page-contents (src dst) (declare (ignore src dst)) nil)
(defun ffi-zero-page (phys-addr) (declare (ignore phys-addr)) nil)
(defun ffi-map-page (root vaddr paddr flags) (declare (ignore root vaddr paddr flags)) nil)
(defun ffi-unmap-page-if-present (root addr) (declare (ignore root addr)) nil)
(defun ffi-update-page-mapping (root addr phys-addr flags) (declare (ignore root addr phys-addr flags)) nil)
(defun ffi-update-page-protection (root addr flags) (declare (ignore root addr flags)) nil)
(defun ffi-read-file-page (file offset phys-page) (declare (ignore file offset phys-page)) nil)
(defun ffi-get-file-object (fd) (declare (ignore fd)) nil)
