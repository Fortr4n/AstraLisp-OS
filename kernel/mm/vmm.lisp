;;;; Virtual Memory Management for AstraLisp-OS
;;;; Comprehensive PowerISA MMU implementation with:
;;;; - Radix page tables (4-level: PGD -> PUD -> PMD -> PTE)
;;;; - Physical page allocator (buddy system)
;;;; - Virtual memory allocator
;;;; - TLB management
;;;; - Page fault handling
;;;; - Memory mapping and protection

(in-package :astralisp-kernel)

;;; ============================================================================
;;; Constants and Definitions
;;; ============================================================================

;;; Page Sizes
(defconstant +page-shift+ 12)               ; 4KB pages
(defconstant +page-size+ 4096)              ; 4KB
(defconstant +huge-page-shift+ 21)          ; 2MB huge pages
(defconstant +huge-page-size+ #x200000)     ; 2MB
(defconstant +giga-page-shift+ 30)          ; 1GB giga pages
(defconstant +giga-page-size+ #x40000000)   ; 1GB

;;; Page Table Levels (Radix Tree for POWER9+)
(defconstant +pgd-shift+ 39)                ; Page Global Directory
(defconstant +pud-shift+ 30)                ; Page Upper Directory
(defconstant +pmd-shift+ 21)                ; Page Middle Directory
(defconstant +pte-shift+ 12)                ; Page Table Entry

(defconstant +ptrs-per-pgd+ 512)            ; 9 bits
(defconstant +ptrs-per-pud+ 512)            ; 9 bits
(defconstant +ptrs-per-pmd+ 512)            ; 9 bits
(defconstant +ptrs-per-pte+ 512)            ; 9 bits

;;; Page Table Entry Flags (PowerISA Radix)
(defconstant +pte-valid+        #x8000000000000000) ; Valid entry
(defconstant +pte-leaf+         #x4000000000000000) ; Leaf entry (actual page)
(defconstant +pte-sw-managed+   #x2000000000000000) ; Software managed
(defconstant +pte-hw-lock+      #x1000000000000000) ; Hardware lock
(defconstant +pte-read+         #x0000000000000004) ; Read permission
(defconstant +pte-write+        #x0000000000000002) ; Write permission
(defconstant +pte-exec+         #x0000000000000001) ; Execute permission
(defconstant +pte-user+         #x0000000000000008) ; User accessible
(defconstant +pte-global+       #x0000000000000010) ; Global mapping
(defconstant +pte-accessed+     #x0000000000000020) ; Accessed bit
(defconstant +pte-dirty+        #x0000000000000040) ; Dirty bit
(defconstant +pte-cache-inhibit+ #x0000000000000080) ; Cache inhibited
(defconstant +pte-write-through+ #x0000000000000100) ; Write-through

;;; Page Frame Number Mask
(defconstant +pte-pfn-mask+ #x00FFFFFFFFFFF000)

;;; Virtual Address Space Layout
(defconstant +kernel-start+ #xC000000000000000)  ; Kernel virtual start
(defconstant +kernel-end+   #xFFFFFFFFFFFFFFFF)  ; Kernel virtual end
(defconstant +user-start+   #x0000000000010000)  ; User virtual start
(defconstant +user-end+     #x00007FFFFFFFFFFF)  ; User virtual end
(defconstant +vmalloc-start+ #xD000000000000000) ; vmalloc area start
(defconstant +vmalloc-end+   #xDFFFFFFFFFFFFFFF) ; vmalloc area end

;;; Memory Zones
(defconstant +zone-dma+ 0)       ; DMA-capable memory (< 4GB)
(defconstant +zone-normal+ 1)    ; Normal memory
(defconstant +zone-highmem+ 2)   ; High memory (> physical addressing limit)

;;; Buddy Allocator Orders
(defconstant +max-order+ 11)     ; Maximum 2^11 pages = 8MB

;;; Page Flags
(defconstant +page-flag-reserved+ #x0001)
(defconstant +page-flag-locked+   #x0002)
(defconstant +page-flag-dirty+    #x0004)
(defconstant +page-flag-lru+      #x0008)
(defconstant +page-flag-active+   #x0010)
(defconstant +page-flag-slab+     #x0020)
(defconstant +page-flag-compound+ #x0040)

;;; VM Area Flags
(defconstant +vm-read+     #x01)
(defconstant +vm-write+    #x02)
(defconstant +vm-exec+     #x04)
(defconstant +vm-shared+   #x08)
(defconstant +vm-growsdown+ #x10)
(defconstant +vm-dontcopy+ #x20)
(defconstant +vm-dontexpand+ #x40)

;;; ============================================================================
;;; Data Structures
;;; ============================================================================

;;; Page Descriptor
(defstruct page
  "Physical page descriptor."
  (pfn 0 :type (unsigned-byte 64))          ; Page frame number
  (flags 0 :type (unsigned-byte 32))        ; Page flags
  (count 0 :type (unsigned-byte 32))        ; Reference count
  (mapping 0 :type (unsigned-byte 64))      ; Address space mapping
  (index 0 :type (unsigned-byte 64))        ; Index within mapping
  (lru nil)                                  ; LRU list linkage
  (private 0 :type (unsigned-byte 64)))     ; Private data

;;; Free Area for Buddy Allocator
(defstruct free-area
  "Free area for specific order in buddy allocator."
  (free-list nil :type list)                ; List of free blocks
  (nr-free 0 :type (unsigned-byte 32)))     ; Number of free blocks

;;; Memory Zone
(defstruct mem-zone
  "Memory zone descriptor."
  (name "" :type string)                    ; Zone name
  (start-pfn 0 :type (unsigned-byte 64))    ; First page frame number
  (end-pfn 0 :type (unsigned-byte 64))      ; Last page frame number
  (free-pages 0 :type (unsigned-byte 64))   ; Number of free pages
  (free-areas (make-array (1+ +max-order+) :initial-element nil)) ; Buddy free lists
  (lock nil))                                ; Zone lock

;;; Page Table Entry (in-memory representation)
(defstruct pte
  "Page table entry."
  (value 0 :type (unsigned-byte 64)))

;;; Page Middle Directory Entry
(defstruct pmd
  "Page middle directory entry."
  (value 0 :type (unsigned-byte 64)))

;;; Page Upper Directory Entry
(defstruct pud
  "Page upper directory entry."
  (value 0 :type (unsigned-byte 64)))

;;; Page Global Directory Entry
(defstruct pgd
  "Page global directory entry."
  (value 0 :type (unsigned-byte 64)))

;;; Virtual Memory Area
(defstruct vm-area
  "Virtual memory area descriptor."
  (start 0 :type (unsigned-byte 64))        ; Start virtual address
  (end 0 :type (unsigned-byte 64))          ; End virtual address (exclusive)
  (flags 0 :type (unsigned-byte 32))        ; Protection flags
  (pgoff 0 :type (unsigned-byte 64))        ; Offset in file/device
  (file nil)                                 ; Backing file (if any)
  (ops nil)                                  ; VM operations
  (next nil)                                 ; Next VMA
  (prev nil))                                ; Previous VMA

;;; Address Space
(defstruct mm-struct
  "Memory management structure for process."
  (pgd 0 :type (unsigned-byte 64))          ; Page global directory
  (mmap nil)                                 ; List of VMAs
  (map-count 0 :type (unsigned-byte 32))    ; Number of VMAs
  (total-vm 0 :type (unsigned-byte 64))     ; Total mapped pages
  (locked-vm 0 :type (unsigned-byte 64))    ; Locked pages
  (stack-vm 0 :type (unsigned-byte 64))     ; Stack pages
  (start-code 0 :type (unsigned-byte 64))   ; Code segment start
  (end-code 0 :type (unsigned-byte 64))     ; Code segment end
  (start-data 0 :type (unsigned-byte 64))   ; Data segment start
  (end-data 0 :type (unsigned-byte 64))     ; Data segment end
  (start-brk 0 :type (unsigned-byte 64))    ; Heap start
  (brk 0 :type (unsigned-byte 64))          ; Current heap end
  (start-stack 0 :type (unsigned-byte 64))  ; Stack start
  (lock nil))                                ; MM lock

;;; ============================================================================
;;; Global State
;;; ============================================================================

(defvar *mem-zones* nil
  "Array of memory zones")

(defvar *page-array* nil
  "Array of all page descriptors")

(defvar *total-pages* 0
  "Total number of physical pages")

(defvar *kernel-pgd* 0
  "Kernel page global directory")

(defvar *vmm-lock* nil
  "Global VMM lock")

;;; ============================================================================
;;; PowerISA Assembly Operations
;;; ============================================================================

(defun asm-tlbie (va)
  "TLB invalidate entry (PowerISA tlbie instruction)."
  (declare (type (unsigned-byte 64) va))
  ;; In real implementation, this would be inline assembly
  ;; tlbie rB, rA, RIC, PRS, R
  ;; For now, FFI call to assembly stub
  (tlbie-stub va))

(defun asm-tlbiel (va)
  "TLB invalidate entry local (PowerISA tlbiel instruction)."
  (declare (type (unsigned-byte 64) va))
  ;; tlbiel rB, rA, RIC, PRS, R
  (tlbiel-stub va))

(defun asm-tlbsync ()
  "TLB synchronize (PowerISA tlbsync instruction)."
  ;; tlbsync
  (tlbsync-stub))

(defun asm-ptesync ()
  "Page table synchronize (PowerISA ptesync instruction)."
  ;; ptesync
  (ptesync-stub))

(defun asm-slbia ()
  "Segment lookaside buffer invalidate all."
  ;; slbia
  (slbia-stub))

(defun asm-mtspr-pid (pid)
  "Set Process ID register."
  (declare (type (unsigned-byte 32) pid))
  ;; mtspr SPRN_PID, rS
  (mtspr-pid-stub pid))

(defun asm-mfspr-pid ()
  "Get Process ID register."
  ;; mfspr rD, SPRN_PID
  (mfspr-pid-stub))

;;; ============================================================================
;;; Page Table Management
;;; ============================================================================

(defun pgd-index (va)
  "Get PGD index from virtual address."
  (declare (type (unsigned-byte 64) va))
  (logand (ash va (- +pgd-shift+)) (1- +ptrs-per-pgd+)))

(defun pud-index (va)
  "Get PUD index from virtual address."
  (declare (type (unsigned-byte 64) va))
  (logand (ash va (- +pud-shift+)) (1- +ptrs-per-pud+)))

(defun pmd-index (va)
  "Get PMD index from virtual address."
  (declare (type (unsigned-byte 64) va))
  (logand (ash va (- +pmd-shift+)) (1- +ptrs-per-pmd+)))

(defun pte-index (va)
  "Get PTE index from virtual address."
  (declare (type (unsigned-byte 64) va))
  (logand (ash va (- +pte-shift+)) (1- +ptrs-per-pte+)))

(defun pte-present-p (pte-val)
  "Check if PTE is present."
  (declare (type (unsigned-byte 64) pte-val))
  (logtest pte-val +pte-valid+))

(defun pte-pfn (pte-val)
  "Extract physical page frame number from PTE."
  (declare (type (unsigned-byte 64) pte-val))
  (ash (logand pte-val +pte-pfn-mask+) (- +page-shift+)))

(defun pfn-to-pte (pfn flags)
  "Create PTE from page frame number and flags."
  (declare (type (unsigned-byte 64) pfn)
           (type (unsigned-byte 64) flags))
  (logior (ash pfn +page-shift+) flags +pte-valid+ +pte-leaf+))

(defun alloc-page-table ()
  "Allocate a page table (512 entries = 4KB)."
  (let ((page (alloc-pages 0 +zone-normal+)))
    (when page
      (let ((pfn (page-pfn page)))
        ;; Clear page table
        (let ((pa (pfn-to-phys pfn)))
          (dotimes (i +page-size+)
            (mem-write-u8 (+ pa i) 0))
          pa)))))

(defun free-page-table (pt-addr)
  "Free a page table."
  (declare (type (unsigned-byte 64) pt-addr))
  (let ((pfn (phys-to-pfn pt-addr)))
    (free-pages pfn 0)))

(defun pgd-offset (pgd-base va)
  "Get PGD entry address."
  (declare (type (unsigned-byte 64) pgd-base va))
  (+ pgd-base (* (pgd-index va) 8)))

(defun pud-offset (pgd-val va)
  "Get PUD entry address from PGD value."
  (declare (type (unsigned-byte 64) pgd-val va))
  (let ((pud-base (logand pgd-val +pte-pfn-mask+)))
    (+ pud-base (* (pud-index va) 8))))

(defun pmd-offset (pud-val va)
  "Get PMD entry address from PUD value."
  (declare (type (unsigned-byte 64) pud-val va))
  (let ((pmd-base (logand pud-val +pte-pfn-mask+)))
    (+ pmd-base (* (pmd-index va) 8))))

(defun pte-offset (pmd-val va)
  "Get PTE entry address from PMD value."
  (declare (type (unsigned-byte 64) pmd-val va))
  (let ((pte-base (logand pmd-val +pte-pfn-mask+)))
    (+ pte-base (* (pte-index va) 8))))

;;; ============================================================================
;;; Page Table Walking
;;; ============================================================================

(defun walk-page-table (pgd-base va &optional create)
  "Walk page table hierarchy to get PTE address.
   If CREATE is true, allocate missing levels."
  (declare (type (unsigned-byte 64) pgd-base va)
           (type boolean create))

  ;; Get PGD entry
  (let ((pgd-addr (pgd-offset pgd-base va)))
    (let ((pgd-val (mem-read-u64 pgd-addr)))

      ;; Check if PGD present
      (unless (pte-present-p pgd-val)
        (if create
            ;; Allocate PUD
            (let ((pud-table (alloc-page-table)))
              (unless pud-table
                (return-from walk-page-table nil))
              (setf pgd-val (logior pud-table +pte-valid+))
              (mem-write-u64 pgd-addr pgd-val))
            (return-from walk-page-table nil)))

      ;; Get PUD entry
      (let ((pud-addr (pud-offset pgd-val va)))
        (let ((pud-val (mem-read-u64 pud-addr)))

          ;; Check if huge page (1GB)
          (when (logtest pud-val +pte-leaf+)
            (return-from walk-page-table pud-addr))

          ;; Check if PUD present
          (unless (pte-present-p pud-val)
            (if create
                ;; Allocate PMD
                (let ((pmd-table (alloc-page-table)))
                  (unless pmd-table
                    (return-from walk-page-table nil))
                  (setf pud-val (logior pmd-table +pte-valid+))
                  (mem-write-u64 pud-addr pud-val))
                (return-from walk-page-table nil)))

          ;; Get PMD entry
          (let ((pmd-addr (pmd-offset pud-val va)))
            (let ((pmd-val (mem-read-u64 pmd-addr)))

              ;; Check if huge page (2MB)
              (when (logtest pmd-val +pte-leaf+)
                (return-from walk-page-table pmd-addr))

              ;; Check if PMD present
              (unless (pte-present-p pmd-val)
                (if create
                    ;; Allocate PTE
                    (let ((pte-table (alloc-page-table)))
                      (unless pte-table
                        (return-from walk-page-table nil))
                      (setf pmd-val (logior pte-table +pte-valid+))
                      (mem-write-u64 pmd-addr pmd-val))
                    (return-from walk-page-table nil)))

              ;; Get PTE entry
              (let ((pte-addr (pte-offset pmd-val va)))
                pte-addr))))))))

(defun virt-to-phys (va)
  "Translate virtual address to physical address."
  (declare (type (unsigned-byte 64) va))

  ;; Check if kernel direct mapping
  (when (>= va +kernel-start+)
    ;; Kernel direct map: subtract kernel base
    (return-from virt-to-phys (- va +kernel-start+)))

  ;; Walk page tables
  (let ((pte-addr (walk-page-table *kernel-pgd* va nil)))
    (unless pte-addr
      (return-from virt-to-phys 0))

    (let ((pte-val (mem-read-u64 pte-addr)))
      (unless (pte-present-p pte-val)
        (return-from virt-to-phys 0))

      ;; Get physical frame and add offset
      (let ((pfn (pte-pfn pte-val))
            (offset (logand va (1- +page-size+))))
        (+ (pfn-to-phys pfn) offset)))))

(defun phys-to-virt (pa)
  "Translate physical address to virtual address (kernel mapping)."
  (declare (type (unsigned-byte 64) pa))
  (+ pa +kernel-start+))

;;; ============================================================================
;;; Page Mapping
;;; ============================================================================

(defun map-page (pgd-base va pa flags)
  "Map virtual page to physical page."
  (declare (type (unsigned-byte 64) pgd-base va pa flags))

  ;; Walk page table (create if needed)
  (let ((pte-addr (walk-page-table pgd-base va t)))
    (unless pte-addr
      (return-from map-page nil))

    ;; Create PTE
    (let ((pfn (phys-to-pfn pa)))
      (let ((pte-val (pfn-to-pte pfn flags)))
        ;; Write PTE
        (mem-write-u64 pte-addr pte-val)
        ;; Ensure write is visible
        (asm-ptesync)
        ;; Invalidate TLB
        (asm-tlbie va)
        (asm-tlbsync)
        t))))

(defun unmap-page (pgd-base va)
  "Unmap virtual page."
  (declare (type (unsigned-byte 64) pgd-base va))

  ;; Walk page table
  (let ((pte-addr (walk-page-table pgd-base va nil)))
    (unless pte-addr
      (return-from unmap-page nil))

    ;; Clear PTE
    (mem-write-u64 pte-addr 0)
    ;; Ensure write is visible
    (asm-ptesync)
    ;; Invalidate TLB
    (asm-tlbie va)
    (asm-tlbsync)
    t))

(defun map-pages (pgd-base va pa num-pages flags)
  "Map multiple consecutive pages."
  (declare (type (unsigned-byte 64) pgd-base va pa)
           (type (unsigned-byte 32) num-pages)
           (type (unsigned-byte 64) flags))

  (dotimes (i num-pages)
    (unless (map-page pgd-base
                     (+ va (* i +page-size+))
                     (+ pa (* i +page-size+))
                     flags)
      (return-from map-pages nil)))
  t)

(defun unmap-pages (pgd-base va num-pages)
  "Unmap multiple consecutive pages."
  (declare (type (unsigned-byte 64) pgd-base va)
           (type (unsigned-byte 32) num-pages))

  (dotimes (i num-pages)
    (unmap-page pgd-base (+ va (* i +page-size+))))
  t)

(defun change-page-prot (pgd-base va new-flags)
  "Change page protection flags."
  (declare (type (unsigned-byte 64) pgd-base va new-flags))

  (let ((pte-addr (walk-page-table pgd-base va nil)))
    (unless pte-addr
      (return-from change-page-prot nil))

    (let ((pte-val (mem-read-u64 pte-addr)))
      (unless (pte-present-p pte-val)
        (return-from change-page-prot nil))

      ;; Preserve PFN, update flags
      (let ((pfn (pte-pfn pte-val)))
        (let ((new-pte (pfn-to-pte pfn new-flags)))
          (mem-write-u64 pte-addr new-pte)
          (asm-ptesync)
          (asm-tlbie va)
          (asm-tlbsync)
          t)))))

;;; ============================================================================
;;; Physical Page Allocator (Buddy System)
;;; ============================================================================

(defun pfn-to-phys (pfn)
  "Convert page frame number to physical address."
  (declare (type (unsigned-byte 64) pfn))
  (ash pfn +page-shift+))

(defun phys-to-pfn (pa)
  "Convert physical address to page frame number."
  (declare (type (unsigned-byte 64) pa))
  (ash pa (- +page-shift+)))

(defun get-page (pfn)
  "Get page descriptor from PFN."
  (declare (type (unsigned-byte 64) pfn))
  (when (< pfn *total-pages*)
    (aref *page-array* pfn)))

(defun buddy-pfn (pfn order)
  "Get buddy page frame number."
  (declare (type (unsigned-byte 64) pfn)
           (type (unsigned-byte 8) order))
  (logxor pfn (ash 1 order)))

(defun pfn-aligned-p (pfn order)
  "Check if PFN is aligned for given order."
  (declare (type (unsigned-byte 64) pfn)
           (type (unsigned-byte 8) order))
  (zerop (logand pfn (1- (ash 1 order)))))

(defun expand-pages (zone pfn order current-order)
  "Expand higher order block into lower order blocks."
  (declare (type mem-zone zone)
           (type (unsigned-byte 64) pfn)
           (type (unsigned-byte 8) order current-order))

  (loop while (> current-order order) do
    (decf current-order)
    (let ((buddy (+ pfn (ash 1 current-order))))
      ;; Add buddy to free list
      (let ((area (aref (mem-zone-free-areas zone) current-order)))
        (push buddy (free-area-free-list area))
        (incf (free-area-nr-free area))))))

(defun alloc-pages (order zone-type)
  "Allocate 2^order contiguous pages."
  (declare (type (unsigned-byte 8) order)
           (type (unsigned-byte 8) zone-type))

  (when (> order +max-order+)
    (return-from alloc-pages nil))

  (let ((zone (aref *mem-zones* zone-type)))
    (with-spinlock ((mem-zone-lock zone))

      ;; Try to find free block of requested order
      (loop for current-order from order to +max-order+ do
        (let ((area (aref (mem-zone-free-areas zone) current-order)))
          (when (and area (free-area-free-list area))
            ;; Found free block
            (let ((pfn (pop (free-area-free-list area))))
              (decf (free-area-nr-free area))
              (decf (mem-zone-free-pages zone) (ash 1 order))

              ;; Expand if necessary
              (when (> current-order order)
                (expand-pages zone pfn order current-order))

              ;; Mark pages as allocated
              (dotimes (i (ash 1 order))
                (let ((page (get-page (+ pfn i))))
                  (when page
                    (setf (page-count page) 1)
                    (setf (page-flags page)
                          (logand (page-flags page)
                                 (lognot +page-flag-lru+))))))

              ;; Return first page
              (return-from alloc-pages (get-page pfn))))))

      ;; No free memory
      nil)))

(defun free-pages (pfn order)
  "Free 2^order contiguous pages starting at PFN."
  (declare (type (unsigned-byte 64) pfn)
           (type (unsigned-byte 8) order))

  (when (> order +max-order+)
    (return-from free-pages nil))

  (let ((page (get-page pfn)))
    (unless page
      (return-from free-pages nil))

    ;; Determine zone
    (let ((zone (find-zone-for-pfn pfn)))
      (unless zone
        (return-from free-pages nil))

      (with-spinlock ((mem-zone-lock zone))

        ;; Try to coalesce with buddy
        (loop while (<= order +max-order+) do
          (let ((buddy-pfn (buddy-pfn pfn order)))

            ;; Check if buddy is valid and free
            (let ((buddy-page (get-page buddy-pfn)))
              (when (or (null buddy-page)
                       (not (zerop (page-count buddy-page)))
                       (not (pfn-aligned-p (min pfn buddy-pfn) (1+ order))))
                ;; Cannot coalesce - add to free list
                (let ((area (aref (mem-zone-free-areas zone) order)))
                  (push pfn (free-area-free-list area))
                  (incf (free-area-nr-free area))
                  (incf (mem-zone-free-pages zone) (ash 1 order)))
                (return))

              ;; Remove buddy from free list
              (let ((area (aref (mem-zone-free-areas zone) order)))
                (setf (free-area-free-list area)
                      (remove buddy-pfn (free-area-free-list area)))
                (decf (free-area-nr-free area)))

              ;; Merge with buddy
              (setf pfn (min pfn buddy-pfn))
              (incf order)))))

        ;; Mark pages as free
        (dotimes (i (ash 1 order))
          (let ((p (get-page (+ pfn i))))
            (when p
              (setf (page-count p) 0))))

        t))))

(defun find-zone-for-pfn (pfn)
  "Find memory zone containing PFN."
  (declare (type (unsigned-byte 64) pfn))
  (dotimes (i (length *mem-zones*))
    (let ((zone (aref *mem-zones* i)))
      (when (and (>= pfn (mem-zone-start-pfn zone))
                (< pfn (mem-zone-end-pfn zone)))
        (return zone)))))

;;; ============================================================================
;;; Virtual Memory Allocator
;;; ============================================================================

(defvar *vmalloc-start-addr* +vmalloc-start+)
(defvar *vmalloc-lock* nil)

(defun vmalloc (size)
  "Allocate virtually contiguous memory."
  (declare (type (unsigned-byte 64) size))

  (let ((num-pages (ceiling size +page-size+)))
    (with-spinlock (*vmalloc-lock*)

      ;; Find free virtual address range
      (let ((va *vmalloc-start-addr*))

        ;; Allocate physical pages
        (let ((pages nil))
          (dotimes (i num-pages)
            (let ((page (alloc-pages 0 +zone-normal+)))
              (unless page
                ;; Cleanup on failure
                (dolist (p pages)
                  (free-pages (page-pfn p) 0))
                (return-from vmalloc 0))
              (push page pages)))

          (setf pages (nreverse pages))

          ;; Map pages
          (dotimes (i num-pages)
            (let ((page (nth i pages)))
              (unless (map-page *kernel-pgd*
                               (+ va (* i +page-size+))
                               (pfn-to-phys (page-pfn page))
                               (logior +pte-read+ +pte-write+))
                ;; Cleanup on failure
                (dotimes (j i)
                  (unmap-page *kernel-pgd* (+ va (* j +page-size+))))
                (dolist (p pages)
                  (free-pages (page-pfn p) 0))
                (return-from vmalloc 0))))

          ;; Update vmalloc pointer
          (incf *vmalloc-start-addr* (* num-pages +page-size+))

          va)))))

(defun vfree (va)
  "Free virtually allocated memory."
  (declare (type (unsigned-byte 64) va))

  ;; TODO: Track vmalloc allocations to know size
  ;; For now, caller must track size
  (warn "vfree not fully implemented - memory leak")
  t)

;;; ============================================================================
;;; TLB Management
;;; ============================================================================

(defun flush-tlb-all ()
  "Flush all TLB entries."
  ;; Invalidate all
  (asm-slbia)
  (asm-tlbsync))

(defun flush-tlb-range (start end)
  "Flush TLB entries in address range."
  (declare (type (unsigned-byte 64) start end))

  (let ((addr start))
    (loop while (< addr end) do
      (asm-tlbie addr)
      (incf addr +page-size+)))
  (asm-tlbsync))

(defun flush-tlb-page (va)
  "Flush single TLB entry."
  (declare (type (unsigned-byte 64) va))
  (asm-tlbie va)
  (asm-tlbsync))

(defun flush-tlb-mm (mm)
  "Flush TLB for entire address space."
  (declare (type mm-struct mm))
  (declare (ignore mm))
  ;; In production, would be more selective
  (flush-tlb-all))

;;; ============================================================================
;;; Page Fault Handling
;;; ============================================================================

(defun handle-page-fault (va error-code)
  "Handle page fault exception."
  (declare (type (unsigned-byte 64) va)
           (type (unsigned-byte 32) error-code))

  ;; Determine fault type
  (let ((write-fault (logbitp 1 error-code))
        (user-fault (logbitp 2 error-code))
        (exec-fault (logbitp 3 error-code)))

    ;; Get current mm
    (let ((mm (get-current-mm)))
      (unless mm
        ;; Kernel fault - panic
        (kernel-panic "Page fault in kernel mode at %lx" va))

      ;; Find VMA
      (let ((vma (find-vma mm va)))
        (unless vma
          ;; No VMA - segfault
          (send-signal-segv va)
          (return-from handle-page-fault nil))

        ;; Check permissions
        (when (and write-fault (not (logtest (vm-area-flags vma) +vm-write+)))
          (send-signal-segv va)
          (return-from handle-page-fault nil))

        (when (and exec-fault (not (logtest (vm-area-flags vma) +vm-exec+)))
          (send-signal-segv va)
          (return-from handle-page-fault nil))

        ;; Handle demand paging
        (handle-demand-paging mm vma va write-fault)))))

(defun handle-demand-paging (mm vma va write-fault)
  "Handle demand paging - allocate page on first access."
  (declare (type mm-struct mm)
           (type vm-area vma)
           (type (unsigned-byte 64) va)
           (type boolean write-fault))

  ;; Allocate physical page
  (let ((page (alloc-pages 0 +zone-normal+)))
    (unless page
      ;; Out of memory
      (send-signal-kill)
      (return-from handle-demand-paging nil))

    ;; Clear page
    (let ((pa (pfn-to-phys (page-pfn page))))
      (dotimes (i +page-size+)
        (mem-write-u8 (+ pa i) 0)))

    ;; Determine flags
    (let ((flags (logior +pte-user+
                        (if (logtest (vm-area-flags vma) +vm-read+) +pte-read+ 0)
                        (if (logtest (vm-area-flags vma) +vm-write+) +pte-write+ 0)
                        (if (logtest (vm-area-flags vma) +vm-exec+) +pte-exec+ 0))))

      ;; Map page
      (let ((page-va (logand va (lognot (1- +page-size+)))))
        (unless (map-page (mm-struct-pgd mm)
                         page-va
                         (pfn-to-phys (page-pfn page))
                         flags)
          (free-pages (page-pfn page) 0)
          (send-signal-kill)
          (return-from handle-demand-paging nil))))

    t))

;;; ============================================================================
;;; VMA Management
;;; ============================================================================

(defun find-vma (mm addr)
  "Find VMA containing address."
  (declare (type mm-struct mm)
           (type (unsigned-byte 64) addr))

  (let ((vma (mm-struct-mmap mm)))
    (loop while vma do
      (when (and (>= addr (vm-area-start vma))
                (< addr (vm-area-end vma)))
        (return vma))
      (setf vma (vm-area-next vma)))))

(defun insert-vma (mm vma)
  "Insert VMA into address space."
  (declare (type mm-struct mm)
           (type vm-area vma))

  (with-spinlock ((mm-struct-lock mm))
    ;; Simple insertion at head (production would maintain sorted order)
    (setf (vm-area-next vma) (mm-struct-mmap mm))
    (when (mm-struct-mmap mm)
      (setf (vm-area-prev (mm-struct-mmap mm)) vma))
    (setf (mm-struct-mmap mm) vma)
    (incf (mm-struct-map-count mm))
    t))

(defun remove-vma (mm vma)
  "Remove VMA from address space."
  (declare (type mm-struct mm)
           (type vm-area vma))

  (with-spinlock ((mm-struct-lock mm))
    (when (vm-area-prev vma)
      (setf (vm-area-next (vm-area-prev vma)) (vm-area-next vma)))
    (when (vm-area-next vma)
      (setf (vm-area-prev (vm-area-next vma)) (vm-area-prev vma)))
    (when (eq vma (mm-struct-mmap mm))
      (setf (mm-struct-mmap mm) (vm-area-next vma)))
    (decf (mm-struct-map-count mm))
    t))

(defun do-mmap (mm addr length prot flags file offset)
  "Map memory region."
  (declare (type mm-struct mm)
           (type (unsigned-byte 64) addr length offset)
           (type (unsigned-byte 32) prot flags))

  ;; Align to page boundary
  (setf addr (logand addr (lognot (1- +page-size+))))
  (setf length (logand (+ length (1- +page-size+)) (lognot (1- +page-size+))))

  ;; Create VMA
  (let ((vma (make-vm-area
              :start addr
              :end (+ addr length)
              :flags prot
              :pgoff (ash offset (- +page-shift+))
              :file file)))

    ;; Insert VMA
    (insert-vma mm vma)

    addr))

(defun do-munmap (mm addr length)
  "Unmap memory region."
  (declare (type mm-struct mm)
           (type (unsigned-byte 64) addr length))

  ;; Find and remove VMAs in range
  (let ((vma (mm-struct-mmap mm)))
    (loop while vma do
      (let ((next (vm-area-next vma)))
        (when (and (< (vm-area-start vma) (+ addr length))
                  (> (vm-area-end vma) addr))
          ;; VMA overlaps - unmap pages
          (let ((start (max (vm-area-start vma) addr))
                (end (min (vm-area-end vma) (+ addr length))))
            (unmap-pages (mm-struct-pgd mm)
                        start
                        (ash (- end start) (- +page-shift+))))
          ;; Remove VMA
          (remove-vma mm vma))
        (setf vma next))))

  t)

;;; ============================================================================
;;; Initialization
;;; ============================================================================

(defun init-mem-zone (zone-type name start-pfn end-pfn)
  "Initialize memory zone."
  (declare (type (unsigned-byte 8) zone-type)
           (type string name)
           (type (unsigned-byte 64) start-pfn end-pfn))

  (let ((zone (make-mem-zone
               :name name
               :start-pfn start-pfn
               :end-pfn end-pfn
               :free-pages 0
               :lock (make-spinlock))))

    ;; Initialize free areas
    (dotimes (order (1+ +max-order+))
      (setf (aref (mem-zone-free-areas zone) order)
            (make-free-area :free-list nil :nr-free 0)))

    ;; Add all pages to buddy allocator
    (let ((nr-pages (- end-pfn start-pfn)))
      ;; Add pages in largest possible blocks
      (let ((pfn start-pfn))
        (loop while (< pfn end-pfn) do
          (let ((order +max-order+))
            ;; Find largest order that fits
            (loop while (> order 0) do
              (if (and (pfn-aligned-p pfn order)
                      (<= (+ pfn (ash 1 order)) end-pfn))
                  (return)
                  (decf order)))

            ;; Add to free list
            (let ((area (aref (mem-zone-free-areas zone) order)))
              (push pfn (free-area-free-list area))
              (incf (free-area-nr-free area))
              (incf (mem-zone-free-pages zone) (ash 1 order)))

            (incf pfn (ash 1 order))))))

    zone))

(defun vmm-init (mem-start mem-size)
  "Initialize virtual memory management."
  (declare (type (unsigned-byte 64) mem-start mem-size))

  (setf *vmm-lock* (make-spinlock))
  (setf *vmalloc-lock* (make-spinlock))

  ;; Calculate total pages
  (setf *total-pages* (ash mem-size (- +page-shift+)))

  ;; Allocate page array
  (let ((page-array-size (* *total-pages* (cffi:foreign-type-size 'page))))
    ;; In production, would use bootmem allocator
    (setf *page-array* (make-array *total-pages* :element-type 'page
                                   :initial-element (make-page))))

  ;; Initialize page descriptors
  (dotimes (pfn *total-pages*)
    (setf (aref *page-array* pfn)
          (make-page :pfn pfn :flags 0 :count 0)))

  ;; Setup memory zones
  (setf *mem-zones* (make-array 3))

  ;; Zone DMA: first 4GB
  (let ((dma-end-pfn (min *total-pages* (ash #x100000000 (- +page-shift+)))))
    (setf (aref *mem-zones* +zone-dma+)
          (init-mem-zone +zone-dma+ "DMA" 0 dma-end-pfn)))

  ;; Zone Normal: rest of memory
  (let ((normal-start-pfn (ash #x100000000 (- +page-shift+))))
    (when (< normal-start-pfn *total-pages*)
      (setf (aref *mem-zones* +zone-normal+)
            (init-mem-zone +zone-normal+ "Normal"
                          normal-start-pfn *total-pages*))))

  ;; Allocate kernel page global directory
  (let ((pgd-page (alloc-pages 0 +zone-normal+)))
    (setf *kernel-pgd* (pfn-to-phys (page-pfn pgd-page)))

    ;; Clear PGD
    (dotimes (i +page-size+)
      (mem-write-u8 (+ *kernel-pgd* i) 0)))

  ;; Map kernel text/data (identity mapping)
  ;; In production, would map based on kernel layout
  (let ((kernel-pages 1024))  ; 4MB kernel
    (map-pages *kernel-pgd*
              +kernel-start+
              0
              kernel-pages
              (logior +pte-read+ +pte-write+ +pte-exec+ +pte-global+)))

  (printk "VMM: Initialized %d MB memory\n" (ash mem-size -20))
  (printk "VMM: %d pages, kernel PGD at 0x%lx\n" *total-pages* *kernel-pgd*)
  t)

;;; ============================================================================
;;; Forward Declarations
;;; ============================================================================

(declaim (ftype (function () t) get-current-mm))
(declaim (ftype (function ((unsigned-byte 64)) t) send-signal-segv))
(declaim (ftype (function () t) send-signal-kill))
(declaim (ftype (function (string &rest t) t) kernel-panic))
(declaim (ftype (function (string &rest t) t) printk))
(declaim (ftype (function () t) make-spinlock))
(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 8)) mem-read-u8))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 8)) t) mem-write-u8))
(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 64)) mem-read-u64))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64)) t) mem-write-u64))

;;; PowerISA assembly stubs (implemented in assembly)
(declaim (ftype (function ((unsigned-byte 64)) t) tlbie-stub))
(declaim (ftype (function ((unsigned-byte 64)) t) tlbiel-stub))
(declaim (ftype (function () t) tlbsync-stub))
(declaim (ftype (function () t) ptesync-stub))
(declaim (ftype (function () t) slbia-stub))
(declaim (ftype (function ((unsigned-byte 32)) t) mtspr-pid-stub))
(declaim (ftype (function () (unsigned-byte 32)) mfspr-pid-stub))

(provide 'vmm)
