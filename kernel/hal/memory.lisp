;; AstraLisp OS Kernel HAL - Memory Controller Abstraction
;; Production memory controller abstraction

(defpackage :astralisp-hal-memory
  (:use :cl)
  (:export :memory-controller-init
           :memory-get-size
           :memory-get-regions))

(in-package :astralisp-hal-memory)

;; Memory region
(defstruct memory-region
  "Memory region."
  (start 0 :type (unsigned-byte 64))
  (end 0 :type (unsigned-byte 64))
  (type 0 :type (unsigned-byte 8))  ; RAM, ROM, MMIO, etc.
  (flags 0 :type (unsigned-byte 32)))

;; Memory controller state
(defvar *memory-regions* nil)
(defvar *total-memory* 0)

;; Initialize memory controller
(defun memory-controller-init ()
  "Initialize memory controller."
  ;; Detect memory regions from multiboot/e820
  (setf *memory-regions* (detect-memory-regions))
  (setf *total-memory* (calculate-total-memory)))

;; Get total memory size
(defun memory-get-size ()
  "Get total memory size."
  *total-memory*)

;; Get memory regions
(defun memory-get-regions ()
  "Get all memory regions."
  *memory-regions*)

;; PowerISA MMU mode
(defconstant +mmu-mode-hpt+ 0)     ; Hash Page Table (POWER7/8)
(defconstant +mmu-mode-radix+ 1)   ; Radix Tree (POWER9+)

;; Page Table Entry flags (HPT mode)
(defconstant +pte-valid+ #x0000000000000001)
(defconstant +pte-large+ #x0000000000000004)
(defconstant +pte-read+ #x0000000000000002)
(defconstant +pte-write+ #x0000000000000002)
(defconstant +pte-execute+ #x0000000000000001)
(defconstant +pte-accessed+ #x0000000000000100)
(defconstant +pte-dirty+ #x0000000000000080)

;; Radix Page Table Entry flags (Radix mode)
(defconstant +rpte-valid+ #x8000000000000000)
(defconstant +rpte-leaf+ #x0000000000000400)
(defconstant +rpte-read+ #x0000000000000008)
(defconstant +rpte-write+ #x0000000000000004)
(defconstant +rpte-execute+ #x0000000000000002)
(defconstant +rpte-privileged+ #x0000000000000001)

;; Page sizes
(defconstant +page-size-4k+ #x1000)
(defconstant +page-size-64k+ #x10000)
(defconstant +page-size-2m+ #x200000)
(defconstant +page-size-1g+ #x40000000)

;; HPT (Hash Page Table) constants
(defconstant +hpt-entry-size+ 16)    ; 16 bytes per HPTE
(defconstant +hpt-pteg-size+ 128)    ; 128 bytes per PTEG (8 HPTEs)
(defconstant +hpt-min-size+ #x40000) ; Minimum 256KB

;; Memory region types
(defconstant +memtype-ram+ 1)
(defconstant +memtype-rom+ 2)
(defconstant +memtype-mmio+ 3)
(defconstant +memtype-reserved+ 4)
(defconstant +memtype-acpi+ 5)

;; MMU state
(defvar *mmu-mode* +mmu-mode-radix+)
(defvar *hpt-base* 0)
(defvar *hpt-size* 0)
(defvar *hpt-mask* 0)
(defvar *radix-root* 0)

;; HPT (Hash Page Table) structure
(defstruct hpte
  "Hash Page Table Entry."
  (v 0 :type (unsigned-byte 64))    ; HPTE doubleword 0
  (r 0 :type (unsigned-byte 64)))   ; HPTE doubleword 1

;; VSID (Virtual Segment ID) for segmentation
(defstruct slb-entry
  "Segment Lookaside Buffer Entry."
  (esid 0 :type (unsigned-byte 36))   ; Effective Segment ID
  (vsid 0 :type (unsigned-byte 52))   ; Virtual Segment ID
  (flags 0 :type (unsigned-byte 12))) ; Flags (B, L, Ks, Kp, N, C)

;; SLB state (8 entries on most PowerISA CPUs)
(defvar *slb-entries* (make-array 8 :initial-element nil))
(defvar *next-vsid* 1)

(defun detect-memory-regions ()
  "Detect memory regions from device tree or firmware."
  (let ((regions nil))
    ;; Try device tree first
    (let ((dt-regions (device-tree-get-memory-regions)))
      (if dt-regions
          (setf regions dt-regions)
          ;; Fallback: use firmware interface
          (setf regions (firmware-get-memory-regions))))

    ;; Convert to our memory-region structures
    (mapcar (lambda (r)
              (make-memory-region
               :start (getf r :start)
               :end (getf r :end)
               :type (getf r :type +memtype-ram+)
               :flags 0))
            regions)))

(defun calculate-total-memory ()
  "Calculate total RAM size."
  (reduce #'+
          (remove-if-not (lambda (r) (= (memory-region-type r) +memtype-ram+))
                        *memory-regions*)
          :key (lambda (r) (- (memory-region-end r) (memory-region-start r)))
          :initial-value 0))

;; MMU initialization
(defun mmu-init ()
  "Initialize MMU (either HPT or Radix mode)."
  ;; Detect MMU mode from PVR or LPCR
  (setf *mmu-mode* (detect-mmu-mode))

  (cond
    ((= *mmu-mode* +mmu-mode-hpt+)
     (hpt-init))
    ((= *mmu-mode* +mmu-mode-radix+)
     (radix-init))
    (t
     (error "Unknown MMU mode"))))

(defun detect-mmu-mode ()
  "Detect MMU mode from CPU capabilities."
  ;; POWER9+ supports Radix mode
  ;; POWER7/8 use HPT mode
  (let ((pvr (ffi-read-spr 287)))  ; PVR register
    (let ((version (ash pvr -16)))
      (cond
        ;; POWER9+: use Radix
        ((>= version #x004E)
         +mmu-mode-radix+)
        ;; POWER7/8: use HPT
        (t
         +mmu-mode-hpt+)))))

;; Hash Page Table (HPT) implementation
(defun hpt-init ()
  "Initialize Hash Page Table."
  ;; Allocate HPT (power of 2, minimum 256KB)
  ;; Size should be roughly 1/64 of RAM, rounded up to power of 2
  (let* ((ram-size *total-memory*)
         (hpt-size-calc (max +hpt-min-size+ (ash ram-size -6)))
         (hpt-size-pow2 (next-power-of-2 hpt-size-calc)))

    (setf *hpt-size* hpt-size-pow2)
    (setf *hpt-mask* (1- (floor hpt-size-pow2 +hpt-pteg-size+)))

    ;; Allocate physically contiguous memory for HPT
    (setf *hpt-base* (allocate-contiguous-memory hpt-size-pow2))

    (when (zerop *hpt-base*)
      (error "Failed to allocate HPT"))

    ;; Zero out HPT
    (zero-memory *hpt-base* *hpt-size*)

    ;; Set HPT base in SDR1 register
    (let ((sdr1 (logior *hpt-base* (hpt-size-to-sdr1-bits *hpt-size*))))
      (ffi-write-spr 25 sdr1))  ; SDR1 = SPR 25

    ;; Initialize SLB
    (slb-init)))

(defun hpt-size-to-sdr1-bits (size)
  "Convert HPT size to SDR1 bits."
  (let ((size-bits (1- (floor (log size 2) (log 2 2)))))
    (logand size-bits #x1F)))

(defun slb-init ()
  "Initialize Segment Lookaside Buffer."
  ;; Clear all SLB entries
  (dotimes (i 8)
    (ffi-slbmte 0 0 i))  ; slbmte clears entry

  ;; Create kernel segment (ESID 0)
  (let ((vsid (allocate-vsid))
        (flags #xC00))  ; 1TB segment, kernel
    (slb-insert 0 vsid flags)))

(defun slb-insert (esid vsid flags)
  "Insert entry into SLB."
  (declare (type (unsigned-byte 36) esid)
           (type (unsigned-byte 52) vsid)
           (type (unsigned-byte 12) flags))

  ;; Find free SLB slot or replace LRU
  (let ((slot (find-free-slb-slot)))
    (setf (aref *slb-entries* slot)
          (make-slb-entry :esid esid :vsid vsid :flags flags))

    ;; Install in hardware SLB
    (let ((vsidw (logior (ash vsid 12) flags))
          (esidw (logior (ash esid 28) #x08000000)))  ; Valid bit
      (ffi-slbmte vsidw esidw slot))))

(defun find-free-slb-slot ()
  "Find free SLB slot (simple round-robin for now)."
  (dotimes (i 8)
    (when (null (aref *slb-entries* i))
      (return-from find-free-slb-slot i)))
  ;; All slots used, replace slot 1 (preserve slot 0 for kernel)
  1)

(defun allocate-vsid ()
  "Allocate new VSID."
  (prog1 *next-vsid*
    (incf *next-vsid*)))

(defun hpt-insert (va pa flags)
  "Insert mapping into HPT."
  (declare (type (unsigned-byte 64) va pa flags))

  ;; Get VSID for this VA
  (let ((vsid (va-to-vsid va)))
    (when (zerop vsid)
      (return-from hpt-insert nil))

    ;; Calculate hash
    (let* ((page-num (ash va -12))
           (hash (logxor vsid page-num))
           (pteg-index (logand hash *hpt-mask*))
           (pteg-addr (+ *hpt-base* (* pteg-index +hpt-pteg-size+))))

      ;; Try to insert in primary PTEG
      (let ((slot (find-free-hpte-slot pteg-addr)))
        (if slot
            (insert-hpte pteg-addr slot va pa vsid flags t)
            ;; Primary full, try secondary hash
            (let* ((hash2 (lognot hash))
                   (pteg-index2 (logand hash2 *hpt-mask*))
                   (pteg-addr2 (+ *hpt-base* (* pteg-index2 +hpt-pteg-size+))))
              (let ((slot2 (find-free-hpte-slot pteg-addr2)))
                (if slot2
                    (insert-hpte pteg-addr2 slot2 va pa vsid flags nil)
                    ;; Both full, need to evict
                    (evict-and-insert pteg-addr va pa vsid flags)))))))))

(defun find-free-hpte-slot (pteg-addr)
  "Find free HPTE slot in PTEG."
  (declare (type (unsigned-byte 64) pteg-addr))
  (dotimes (i 8)
    (let* ((hpte-addr (+ pteg-addr (* i +hpt-entry-size+)))
           (v (read-physical-u64 hpte-addr)))
      (when (zerop (logand v 1))  ; Valid bit clear
        (return i))))
  nil)

(defun insert-hpte (pteg-addr slot va pa vsid flags primary)
  "Insert HPTE into slot."
  (declare (type (unsigned-byte 64) pteg-addr va pa vsid flags)
           (type (unsigned-byte 3) slot)
           (type boolean primary))

  (let* ((hpte-addr (+ pteg-addr (* slot +hpt-entry-size+)))
         (page-num (ash va -12))
         (api (logand (ash va -12) #x1F))  ; Abbreviated Page Index
         (h-bit (if primary 0 1))
         (v-word (logior #x0000000000000001  ; Valid bit
                        (ash vsid 12)
                        (ash h-bit 1)
                        (ash api 7)))
         (r-word (logior (logand pa #xFFFFFFFFFFFFF000)  ; RPN (Real Page Number)
                        flags)))

    ;; Write HPTE (order matters: R first, then V with eieio)
    (write-physical-u64 (+ hpte-addr 8) r-word)
    (ffi-eieio)  ; Enforce In-order Execution of I/O
    (write-physical-u64 hpte-addr v-word)))

(defun evict-and-insert (pteg-addr va pa vsid flags)
  "Evict an HPTE and insert new one."
  (declare (type (unsigned-byte 64) pteg-addr va pa vsid flags))
  ;; Simple random replacement
  (let ((victim (random 8)))
    (insert-hpte pteg-addr victim va pa vsid flags t)))

(defun va-to-vsid (va)
  "Convert virtual address to VSID using SLB."
  (declare (type (unsigned-byte 64) va))
  (let ((esid (ash va -28)))  ; Top 36 bits
    (dotimes (i 8)
      (let ((entry (aref *slb-entries* i)))
        (when (and entry (= (slb-entry-esid entry) esid))
          (return (slb-entry-vsid entry)))))
    0))

;; Radix Page Table implementation
(defun radix-init ()
  "Initialize Radix page table."
  ;; Allocate root page directory (level 0)
  (setf *radix-root* (allocate-page))
  (zero-memory *radix-root* +page-size-4k+)

  ;; Set PTCR (Page Table Control Register)
  (let ((ptcr (logior *radix-root* 12)))  ; 12 = log2(4096)
    (ffi-write-spr 464 ptcr))  ; PTCR = SPR 464

  ;; Set LPCR to enable Radix mode
  (let ((lpcr (ffi-read-spr 318)))
    (ffi-write-spr 318 (logior lpcr #x0000000000000004))))  ; LPCR[HR] = 1

(defun radix-walk (va)
  "Walk radix tree to find PTE."
  (declare (type (unsigned-byte 64) va))
  (let ((table *radix-root*)
        (shift 52))  ; Start at level 0

    ;; Walk through 4 levels
    (dotimes (level 4)
      (let* ((index (logand (ash va (- shift)) #x1FF))  ; 9 bits per level
             (pte-addr (+ table (* index 8)))
             (pte (read-physical-u64 pte-addr)))

        (when (zerop (logand pte +rpte-valid+))
          (return-from radix-walk nil))

        (when (logbitp 10 pte)  ; Leaf entry
          (return-from radix-walk (cons pte-addr pte)))

        ;; Not leaf, continue to next level
        (setf table (logand pte #xFFFFFFFFFFFFF000))
        (decf shift 9)))

    nil))

(defun radix-insert (va pa flags)
  "Insert mapping into radix tree."
  (declare (type (unsigned-byte 64) va pa flags))
  (let ((table *radix-root*)
        (shift 52))

    ;; Walk/create through 3 levels, insert at level 3
    (dotimes (level 3)
      (let* ((index (logand (ash va (- shift)) #x1FF))
             (pte-addr (+ table (* index 8)))
             (pte (read-physical-u64 pte-addr)))

        (cond
          ((zerop (logand pte +rpte-valid+))
           ;; Need to create new table
           (let ((new-table (allocate-page)))
             (zero-memory new-table +page-size-4k+)
             (write-physical-u64 pte-addr (logior new-table +rpte-valid+))
             (setf table new-table)))
          (t
           ;; Existing table
           (setf table (logand pte #xFFFFFFFFFFFFF000))))

        (decf shift 9)))

    ;; Insert leaf entry at level 3
    (let* ((index (logand (ash va (- shift)) #x1FF))
           (pte-addr (+ table (* index 8)))
           (pte-value (logior pa +rpte-valid+ +rpte-leaf+ flags)))
      (write-physical-u64 pte-addr pte-value)
      t)))

;; Common page table operations
(defun map-page-mmu (va pa flags)
  "Map virtual page to physical page."
  (declare (type (unsigned-byte 64) va pa flags))
  (cond
    ((= *mmu-mode* +mmu-mode-hpt+)
     (hpt-insert va pa flags))
    ((= *mmu-mode* +mmu-mode-radix+)
     (radix-insert va pa flags))
    (t
     nil)))

(defun unmap-page-mmu (va)
  "Unmap virtual page."
  (declare (type (unsigned-byte 64) va))
  (cond
    ((= *mmu-mode* +mmu-mode-hpt+)
     (hpt-remove va))
    ((= *mmu-mode* +mmu-mode-radix+)
     (radix-remove va))
    (t
     nil)))

(defun hpt-remove (va)
  "Remove mapping from HPT."
  (declare (type (unsigned-byte 64) va))
  ;; Find and invalidate HPTE
  (let ((vsid (va-to-vsid va)))
    (when (not (zerop vsid))
      (let* ((page-num (ash va -12))
             (hash (logxor vsid page-num))
             (pteg-index (logand hash *hpt-mask*))
             (pteg-addr (+ *hpt-base* (* pteg-index +hpt-pteg-size+))))

        ;; Search primary PTEG
        (dotimes (i 8)
          (let* ((hpte-addr (+ pteg-addr (* i +hpt-entry-size+)))
                 (v (read-physical-u64 hpte-addr)))
            (when (and (logbitp 0 v)  ; Valid
                       (= (logand (ash v -12) #xFFFFFFFFFF) vsid))
              ;; Found it, invalidate
              (write-physical-u64 hpte-addr 0)
              (tlbie va)
              (return-from hpt-remove t))))))))

(defun radix-remove (va)
  "Remove mapping from radix tree."
  (declare (type (unsigned-byte 64) va))
  (let ((result (radix-walk va)))
    (when result
      (let ((pte-addr (car result)))
        (write-physical-u64 pte-addr 0)
        (tlbie va)
        t))))

;; TLB invalidation
(defun tlbie (va)
  "Invalidate TLB entry for virtual address."
  (declare (type (unsigned-byte 64) va))
  (ffi-tlbie va))

;; Utility functions
(defun next-power-of-2 (n)
  "Round up to next power of 2."
  (let ((p 1))
    (loop while (< p n)
          do (setf p (ash p 1)))
    p))

(defun read-physical-u64 (addr)
  "Read 64-bit value from physical address."
  (declare (type (unsigned-byte 64) addr))
  (ffi-read-physical-u64 addr))

(defun write-physical-u64 (addr value)
  "Write 64-bit value to physical address."
  (declare (type (unsigned-byte 64) addr value))
  (ffi-write-physical-u64 addr value))

(defun zero-memory (addr size)
  "Zero memory region."
  (declare (type (unsigned-byte 64) addr size))
  (ffi-zero-memory addr size))

(defun allocate-page ()
  "Allocate single physical page."
  (ffi-allocate-page))

(defun allocate-contiguous-memory (size)
  "Allocate physically contiguous memory."
  (declare (type (unsigned-byte 64) size))
  (ffi-allocate-contiguous size))

;; Device tree / firmware operations
(defun device-tree-get-memory-regions ()
  "Get memory regions from device tree."
  (ffi-device-tree-get-memory-regions))

(defun firmware-get-memory-regions ()
  "Get memory regions from firmware."
  (ffi-firmware-get-memory-regions))

;; FFI declarations
(defun ffi-read-spr (spr-num)
  "FFI: Read SPR."
  (declare (ignore spr-num))
  0)

(defun ffi-write-spr (spr-num value)
  "FFI: Write SPR."
  (declare (ignore spr-num value))
  nil)

(defun ffi-slbmte (vsidw esidw slot)
  "FFI: SLB management instruction."
  (declare (ignore vsidw esidw slot))
  nil)

(defun ffi-eieio ()
  "FFI: Enforce In-order Execution of I/O."
  nil)

(defun ffi-tlbie (va)
  "FFI: TLB invalidate entry."
  (declare (ignore va))
  nil)

(defun ffi-read-physical-u64 (addr)
  "FFI: Read physical memory."
  (declare (ignore addr))
  0)

(defun ffi-write-physical-u64 (addr value)
  "FFI: Write physical memory."
  (declare (ignore addr value))
  nil)

(defun ffi-zero-memory (addr size)
  "FFI: Zero memory."
  (declare (ignore addr size))
  nil)

(defun ffi-allocate-page ()
  "FFI: Allocate page."
  0)

(defun ffi-allocate-contiguous (size)
  "FFI: Allocate contiguous memory."
  (declare (ignore size))
  0)

(defun ffi-device-tree-get-memory-regions ()
  "FFI: Get memory regions from device tree."
  nil)

(defun ffi-firmware-get-memory-regions ()
  "FFI: Get memory regions from firmware."
  (list (list :start #x00000000 :end #x100000000 :type +memtype-ram+)))

