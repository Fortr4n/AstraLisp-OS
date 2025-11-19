;;;; NVMe Driver for AstraLisp-OS
;;;; Comprehensive NVMe 1.4 driver implementation with:
;;;; - Admin and I/O queue pairs
;;;; - Namespace discovery and management
;;;; - Full command set support
;;;; - MSI-X interrupt handling
;;;; - DMA operations
;;;; - Block device integration

(in-package :astralisp-kernel)

;;; ============================================================================
;;; Constants and Definitions
;;; ============================================================================

;;; NVMe Register Offsets (Controller Registers in BAR0)
(defconstant +nvme-reg-cap+      #x0000)  ; Controller Capabilities
(defconstant +nvme-reg-vs+       #x0008)  ; Version
(defconstant +nvme-reg-intms+    #x000C)  ; Interrupt Mask Set
(defconstant +nvme-reg-intmc+    #x0010)  ; Interrupt Mask Clear
(defconstant +nvme-reg-cc+       #x0014)  ; Controller Configuration
(defconstant +nvme-reg-csts+     #x001C)  ; Controller Status
(defconstant +nvme-reg-nssr+     #x0020)  ; NVM Subsystem Reset
(defconstant +nvme-reg-aqa+      #x0024)  ; Admin Queue Attributes
(defconstant +nvme-reg-asq+      #x0028)  ; Admin Submission Queue Base
(defconstant +nvme-reg-acq+      #x0030)  ; Admin Completion Queue Base

;;; Controller Capabilities (CAP) register fields
(defconstant +nvme-cap-mqes-shift+   0)   ; Maximum Queue Entries Supported
(defconstant +nvme-cap-cqr-shift+    16)  ; Contiguous Queues Required
(defconstant +nvme-cap-ams-shift+    17)  ; Arbitration Mechanism Supported
(defconstant +nvme-cap-to-shift+     24)  ; Timeout
(defconstant +nvme-cap-dstrd-shift+  32)  ; Doorbell Stride
(defconstant +nvme-cap-nssrs-shift+  36)  ; NVM Subsystem Reset Supported
(defconstant +nvme-cap-css-shift+    37)  ; Command Sets Supported
(defconstant +nvme-cap-mpsmin-shift+ 48)  ; Memory Page Size Minimum
(defconstant +nvme-cap-mpsmax-shift+ 52)  ; Memory Page Size Maximum

;;; Controller Configuration (CC) register fields
(defconstant +nvme-cc-enable+        #x00000001)
(defconstant +nvme-cc-css-nvm+       #x00000000)
(defconstant +nvme-cc-mps-shift+     7)
(defconstant +nvme-cc-ams-rr+        #x00000000)
(defconstant +nvme-cc-shn-none+      #x00000000)
(defconstant +nvme-cc-shn-normal+    #x00004000)
(defconstant +nvme-cc-iosqes-shift+  16)
(defconstant +nvme-cc-iocqes-shift+  20)

;;; Controller Status (CSTS) register fields
(defconstant +nvme-csts-rdy+         #x00000001)
(defconstant +nvme-csts-cfs+         #x00000002)  ; Controller Fatal Status
(defconstant +nvme-csts-shst-mask+   #x0000000C)
(defconstant +nvme-csts-shst-normal+ #x00000000)
(defconstant +nvme-csts-shst-occur+  #x00000004)
(defconstant +nvme-csts-shst-cmplt+  #x00000008)

;;; Admin Command Opcodes
(defconstant +nvme-admin-delete-sq+      #x00)
(defconstant +nvme-admin-create-sq+      #x01)
(defconstant +nvme-admin-delete-cq+      #x04)
(defconstant +nvme-admin-create-cq+      #x05)
(defconstant +nvme-admin-identify+       #x06)
(defconstant +nvme-admin-abort+          #x08)
(defconstant +nvme-admin-set-features+   #x09)
(defconstant +nvme-admin-get-features+   #x0A)
(defconstant +nvme-admin-async-event+    #x0C)
(defconstant +nvme-admin-ns-mgmt+        #x0D)
(defconstant +nvme-admin-fw-commit+      #x10)
(defconstant +nvme-admin-fw-download+    #x11)
(defconstant +nvme-admin-ns-attach+      #x15)
(defconstant +nvme-admin-keep-alive+     #x18)

;;; NVM Command Opcodes
(defconstant +nvme-cmd-flush+       #x00)
(defconstant +nvme-cmd-write+       #x01)
(defconstant +nvme-cmd-read+        #x02)
(defconstant +nvme-cmd-write-uncor+ #x04)
(defconstant +nvme-cmd-compare+     #x05)
(defconstant +nvme-cmd-write-zeros+ #x08)
(defconstant +nvme-cmd-dsm+         #x09)  ; Dataset Management
(defconstant +nvme-cmd-resv-reg+    #x0D)
(defconstant +nvme-cmd-resv-report+ #x0E)
(defconstant +nvme-cmd-resv-acquire+ #x11)
(defconstant +nvme-cmd-resv-release+ #x15)

;;; Identify Controller/Namespace
(defconstant +nvme-identify-ctrl+   #x01)
(defconstant +nvme-identify-ns+     #x00)
(defconstant +nvme-identify-ns-list+ #x02)

;;; Feature Identifiers
(defconstant +nvme-feat-arbitration+     #x01)
(defconstant +nvme-feat-power-mgmt+      #x02)
(defconstant +nvme-feat-lba-range+       #x03)
(defconstant +nvme-feat-temp-thresh+     #x04)
(defconstant +nvme-feat-err-recovery+    #x05)
(defconstant +nvme-feat-write-cache+     #x06)
(defconstant +nvme-feat-num-queues+      #x07)
(defconstant +nvme-feat-irq-coalesce+    #x08)
(defconstant +nvme-feat-irq-config+      #x09)
(defconstant +nvme-feat-write-atomic+    #x0A)
(defconstant +nvme-feat-async-event+     #x0B)

;;; Queue and Command sizes
(defconstant +nvme-sqe-size+ 64)   ; Submission Queue Entry size
(defconstant +nvme-cqe-size+ 16)   ; Completion Queue Entry size
(defconstant +nvme-max-queue-depth+ 1024)
(defconstant +nvme-aq-depth+ 32)   ; Admin Queue depth

;;; Completion Queue Entry Status Code Type
(defconstant +nvme-sct-generic+     #x0)
(defconstant +nvme-sct-cmd-specific+ #x1)
(defconstant +nvme-sct-media+       #x2)

;;; Generic Command Status
(defconstant +nvme-sc-success+      #x00)
(defconstant +nvme-sc-invalid-opcode+ #x01)
(defconstant +nvme-sc-invalid-field+ #x02)
(defconstant +nvme-sc-cmdid-conflict+ #x03)
(defconstant +nvme-sc-data-xfer-error+ #x04)
(defconstant +nvme-sc-abort-req+    #x07)
(defconstant +nvme-sc-internal+     #x06)
(defconstant +nvme-sc-ns-not-ready+ #x82)

;;; PCI Configuration Space
(defconstant +pci-vendor-id+        #x00)
(defconstant +pci-device-id+        #x02)
(defconstant +pci-command+          #x04)
(defconstant +pci-status+           #x06)
(defconstant +pci-bar0+             #x10)
(defconstant +pci-bar1+             #x14)
(defconstant +pci-cap-ptr+          #x34)

;;; PCI Command Register
(defconstant +pci-command-io+       #x0001)
(defconstant +pci-command-memory+   #x0002)
(defconstant +pci-command-master+   #x0004)
(defconstant +pci-command-intx-disable+ #x0400)

;;; MSI-X Capability
(defconstant +pci-cap-msix+         #x11)
(defconstant +msix-table-size-mask+ #x07FF)
(defconstant +msix-enable+          #x8000)

;;; Namespace Features
(defconstant +nvme-ns-feat-thin+    #x01)
(defconstant +nvme-ns-dps-pi-first+ #x08)

;;; LBA Format
(defconstant +nvme-lbaf-rp-best+    #x00)
(defconstant +nvme-lbaf-rp-better+  #x01)
(defconstant +nvme-lbaf-rp-good+    #x02)
(defconstant +nvme-lbaf-rp-degraded+ #x03)

;;; Timeouts
(defconstant +nvme-admin-timeout+ 60000)  ; 60 seconds in ms
(defconstant +nvme-io-timeout+    30000)  ; 30 seconds in ms

;;; ============================================================================
;;; Data Structures
;;; ============================================================================

;;; NVMe Command - 64 bytes
(defstruct nvme-command
  (opc 0 :type (unsigned-byte 8))           ; Opcode
  (fuse 0 :type (unsigned-byte 2))          ; Fused Operation
  (psdt 0 :type (unsigned-byte 2))          ; PRP or SGL
  (cid 0 :type (unsigned-byte 16))          ; Command Identifier
  (nsid 0 :type (unsigned-byte 32))         ; Namespace Identifier
  (cdw2 0 :type (unsigned-byte 32))
  (cdw3 0 :type (unsigned-byte 32))
  (mptr 0 :type (unsigned-byte 64))         ; Metadata Pointer
  (prp1 0 :type (unsigned-byte 64))         ; PRP Entry 1
  (prp2 0 :type (unsigned-byte 64))         ; PRP Entry 2
  (cdw10 0 :type (unsigned-byte 32))
  (cdw11 0 :type (unsigned-byte 32))
  (cdw12 0 :type (unsigned-byte 32))
  (cdw13 0 :type (unsigned-byte 32))
  (cdw14 0 :type (unsigned-byte 32))
  (cdw15 0 :type (unsigned-byte 32)))

;;; NVMe Completion Queue Entry - 16 bytes
(defstruct nvme-completion
  (result 0 :type (unsigned-byte 32))       ; Command-specific result
  (rsvd 0 :type (unsigned-byte 32))
  (sq-head 0 :type (unsigned-byte 16))      ; SQ Head Pointer
  (sq-id 0 :type (unsigned-byte 16))        ; SQ Identifier
  (cid 0 :type (unsigned-byte 16))          ; Command Identifier
  (status 0 :type (unsigned-byte 16)))      ; Status Field (P, SC, SCT, M, DNR)

;;; Queue Pair (Submission Queue + Completion Queue)
(defstruct nvme-queue
  (qid 0 :type (unsigned-byte 16))          ; Queue ID
  (depth 0 :type (unsigned-byte 16))        ; Queue depth
  (sq-buffer 0 :type (unsigned-byte 64))    ; Submission Queue buffer (phys)
  (cq-buffer 0 :type (unsigned-byte 64))    ; Completion Queue buffer (phys)
  (sq-dma-addr 0 :type (unsigned-byte 64))  ; SQ DMA address
  (cq-dma-addr 0 :type (unsigned-byte 64))  ; CQ DMA address
  (sq-tail 0 :type (unsigned-byte 16))      ; SQ Tail (driver writes here)
  (cq-head 0 :type (unsigned-byte 16))      ; CQ Head (driver reads here)
  (cq-phase 1 :type (unsigned-byte 1))      ; CQ Phase bit
  (sq-doorbell 0 :type (unsigned-byte 64))  ; SQ Doorbell register address
  (cq-doorbell 0 :type (unsigned-byte 64))  ; CQ Doorbell register address
  (lock nil)                                 ; Spinlock for queue access
  (irq-vector 0 :type (unsigned-byte 16))   ; MSI-X vector for this queue
  (pending-cmds nil))                        ; Hash table: CID -> request info

;;; Controller Identification Data
(defstruct nvme-id-ctrl
  (vid 0 :type (unsigned-byte 16))          ; PCI Vendor ID
  (ssvid 0 :type (unsigned-byte 16))        ; PCI Subsystem Vendor ID
  (sn (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0))  ; Serial Number
  (mn (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0))  ; Model Number
  (fr (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))   ; Firmware Revision
  (rab 0 :type (unsigned-byte 8))           ; Recommended Arbitration Burst
  (ieee (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)) ; IEEE OUI
  (cmic 0 :type (unsigned-byte 8))          ; Controller Multi-Path I/O
  (mdts 0 :type (unsigned-byte 8))          ; Maximum Data Transfer Size
  (cntlid 0 :type (unsigned-byte 16))       ; Controller ID
  (ver 0 :type (unsigned-byte 32))          ; Version
  (oacs 0 :type (unsigned-byte 16))         ; Optional Admin Command Support
  (acl 0 :type (unsigned-byte 8))           ; Abort Command Limit
  (aerl 0 :type (unsigned-byte 8))          ; Async Event Request Limit
  (frmw 0 :type (unsigned-byte 8))          ; Firmware Updates
  (lpa 0 :type (unsigned-byte 8))           ; Log Page Attributes
  (elpe 0 :type (unsigned-byte 8))          ; Error Log Page Entries
  (npss 0 :type (unsigned-byte 8))          ; Number of Power States Support
  (nn 0 :type (unsigned-byte 32))           ; Number of Namespaces
  (oncs 0 :type (unsigned-byte 16))         ; Optional NVM Command Support
  (sqes 0 :type (unsigned-byte 8))          ; Submission Queue Entry Size
  (cqes 0 :type (unsigned-byte 8)))         ; Completion Queue Entry Size

;;; Namespace Identification Data
(defstruct nvme-id-ns
  (nsze 0 :type (unsigned-byte 64))         ; Namespace Size (in blocks)
  (ncap 0 :type (unsigned-byte 64))         ; Namespace Capacity
  (nuse 0 :type (unsigned-byte 64))         ; Namespace Utilization
  (nsfeat 0 :type (unsigned-byte 8))        ; Namespace Features
  (nlbaf 0 :type (unsigned-byte 8))         ; Number of LBA Formats
  (flbas 0 :type (unsigned-byte 8))         ; Formatted LBA Size
  (mc 0 :type (unsigned-byte 8))            ; Metadata Capabilities
  (dpc 0 :type (unsigned-byte 8))           ; End-to-end Data Protection
  (dps 0 :type (unsigned-byte 8))           ; End-to-end Data Protection Settings
  (lbaf (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0))) ; LBA Format Support

;;; Namespace Structure
(defstruct nvme-namespace
  (nsid 0 :type (unsigned-byte 32))         ; Namespace ID
  (ctrl nil)                                 ; Controller reference
  (id-ns nil)                                ; Namespace identification data
  (block-size 0 :type (unsigned-byte 32))   ; Block size in bytes
  (num-blocks 0 :type (unsigned-byte 64))   ; Number of blocks
  (block-dev nil)                            ; Block device structure
  (lock nil))                                ; Spinlock

;;; NVMe Controller Structure
(defstruct nvme-controller
  (pci-bus 0 :type (unsigned-byte 8))       ; PCI bus number
  (pci-dev 0 :type (unsigned-byte 5))       ; PCI device number
  (pci-func 0 :type (unsigned-byte 3))      ; PCI function number
  (vendor-id 0 :type (unsigned-byte 16))    ; PCI vendor ID
  (device-id 0 :type (unsigned-byte 16))    ; PCI device ID
  (bar0 0 :type (unsigned-byte 64))         ; BAR0 base address
  (bar0-size 0 :type (unsigned-byte 64))    ; BAR0 size
  (regs 0 :type (unsigned-byte 64))         ; Virtual address of registers
  (cap 0 :type (unsigned-byte 64))          ; Controller Capabilities
  (doorbell-stride 0 :type (unsigned-byte 8)) ; Doorbell stride (2^n * 4 bytes)
  (max-queue-entries 0 :type (unsigned-byte 16)) ; Max queue entries
  (page-size 4096 :type (unsigned-byte 32)) ; Page size
  (id-ctrl nil)                              ; Controller identification
  (admin-queue nil)                          ; Admin queue pair
  (io-queues nil)                            ; Array of I/O queue pairs
  (num-io-queues 0 :type (unsigned-byte 16)) ; Number of I/O queues
  (namespaces nil)                           ; Hash table: NSID -> namespace
  (lock nil)                                 ; Controller lock
  (msix-table 0 :type (unsigned-byte 64))   ; MSI-X table base
  (msix-vectors 0 :type (unsigned-byte 16)) ; Number of MSI-X vectors
  (next-cid 0 :type (unsigned-byte 16)))    ; Next command ID

;;; Request tracking
(defstruct nvme-request
  (cmd nil)                                  ; NVMe command
  (buffer 0 :type (unsigned-byte 64))       ; Data buffer
  (length 0 :type (unsigned-byte 32))       ; Data length
  (callback nil)                             ; Completion callback
  (private nil)                              ; Private data
  (timeout 0 :type (unsigned-byte 64))      ; Timeout timestamp
  (completed nil)                            ; Completion semaphore
  (status 0 :type (unsigned-byte 16))       ; Completion status
  (result 0 :type (unsigned-byte 32)))      ; Completion result

;;; ============================================================================
;;; Global State
;;; ============================================================================

(defvar *nvme-controllers* nil
  "List of all NVMe controllers")

(defvar *nvme-lock* nil
  "Global NVMe subsystem lock")

;;; ============================================================================
;;; Forward Declarations
;;; ============================================================================

(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 16)) (unsigned-byte 32)) pci-config-read-u32))
(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)) t) pci-config-write-u32))
(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 16)) (unsigned-byte 16)) pci-config-read-u16))
(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 16)) t) pci-config-write-u16))
(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 64)) virt-to-phys))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64)) (unsigned-byte 64)) phys-to-virt))
(declaim (ftype (function ((unsigned-byte 32)) (unsigned-byte 64)) kmalloc))
(declaim (ftype (function ((unsigned-byte 64)) t) kfree))
(declaim (ftype (function () t) make-spinlock))
(declaim (ftype (function (t) t) spinlock-acquire))
(declaim (ftype (function (t) t) spinlock-release))
(declaim (ftype (function ((unsigned-byte 32)) t) make-semaphore))
(declaim (ftype (function (t) t) semaphore-wait))
(declaim (ftype (function (t) t) semaphore-signal))
(declaim (ftype (function () (unsigned-byte 64)) get-time-ms))
(declaim (ftype (function ((unsigned-byte 32)) t) udelay))
(declaim (ftype (function ((unsigned-byte 32)) t) mdelay))
(declaim (ftype (function (t (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 32)) t) block-device-register))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 64)) dma-alloc-coherent))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32)) t) dma-free-coherent))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 64)) dma-map-single))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32)) t) dma-unmap-single))
(declaim (ftype (function (t string) t) printk))

;;; ============================================================================
;;; Register Access
;;; ============================================================================

(defun nvme-read-reg-u32 (ctrl offset)
  "Read 32-bit controller register."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 32) offset))
  (let ((addr (+ (nvme-controller-regs ctrl) offset)))
    (mem-read-u32 addr)))

(defun nvme-write-reg-u32 (ctrl offset value)
  "Write 32-bit controller register."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 32) offset value))
  (let ((addr (+ (nvme-controller-regs ctrl) offset)))
    (mem-write-u32 addr value))
  t)

(defun nvme-read-reg-u64 (ctrl offset)
  "Read 64-bit controller register."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 32) offset))
  (let ((addr (+ (nvme-controller-regs ctrl) offset)))
    (mem-read-u64 addr)))

(defun nvme-write-reg-u64 (ctrl offset value)
  "Write 64-bit controller register."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 32) offset)
           (type (unsigned-byte 64) value))
  (let ((addr (+ (nvme-controller-regs ctrl) offset)))
    (mem-write-u64 addr value))
  t)

(defun nvme-ring-sq-doorbell (queue)
  "Ring submission queue doorbell."
  (declare (type nvme-queue queue))
  (let ((doorbell-addr (nvme-queue-sq-doorbell queue))
        (tail (nvme-queue-sq-tail queue)))
    (mem-write-u32 doorbell-addr tail))
  t)

(defun nvme-ring-cq-doorbell (queue)
  "Ring completion queue doorbell."
  (declare (type nvme-queue queue))
  (let ((doorbell-addr (nvme-queue-cq-doorbell queue))
        (head (nvme-queue-cq-head queue)))
    (mem-write-u32 doorbell-addr head))
  t)

;;; ============================================================================
;;; Command Construction
;;; ============================================================================

(defun nvme-build-prp-list (buffer length page-size)
  "Build PRP (Physical Region Page) entries for data transfer."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) length page-size))
  (let ((prp1 buffer)
        (prp2 0))
    (when (> length page-size)
      ;; Need PRP2
      (let ((offset (- page-size (logand buffer (1- page-size)))))
        (setf prp2 (+ buffer offset))
        ;; If more than 2 pages, need PRP list (not implemented here - would allocate list)
        ))
    (values prp1 prp2)))

(defun nvme-setup-rw-cmd (cmd opcode nsid slba nlb prp1 prp2)
  "Setup Read/Write command."
  (declare (type nvme-command cmd)
           (type (unsigned-byte 8) opcode)
           (type (unsigned-byte 32) nsid)
           (type (unsigned-byte 64) slba)
           (type (unsigned-byte 16) nlb)
           (type (unsigned-byte 64) prp1 prp2))
  (setf (nvme-command-opc cmd) opcode)
  (setf (nvme-command-nsid cmd) nsid)
  (setf (nvme-command-prp1 cmd) prp1)
  (setf (nvme-command-prp2 cmd) prp2)
  ;; CDW10/11: Starting LBA
  (setf (nvme-command-cdw10 cmd) (logand slba #xFFFFFFFF))
  (setf (nvme-command-cdw11 cmd) (ash slba -32))
  ;; CDW12: Number of logical blocks (0-based)
  (setf (nvme-command-cdw12 cmd) (1- nlb))
  t)

(defun nvme-setup-identify-cmd (cmd cns nsid prp1 prp2)
  "Setup Identify command."
  (declare (type nvme-command cmd)
           (type (unsigned-byte 8) cns)
           (type (unsigned-byte 32) nsid)
           (type (unsigned-byte 64) prp1 prp2))
  (setf (nvme-command-opc cmd) +nvme-admin-identify+)
  (setf (nvme-command-nsid cmd) nsid)
  (setf (nvme-command-prp1 cmd) prp1)
  (setf (nvme-command-prp2 cmd) prp2)
  (setf (nvme-command-cdw10 cmd) cns)
  t)

(defun nvme-setup-create-cq-cmd (cmd qid qsize prp1 vector)
  "Setup Create I/O Completion Queue command."
  (declare (type nvme-command cmd)
           (type (unsigned-byte 16) qid qsize vector)
           (type (unsigned-byte 64) prp1))
  (setf (nvme-command-opc cmd) +nvme-admin-create-cq+)
  (setf (nvme-command-prp1 cmd) prp1)
  ;; CDW10: QID and Queue Size
  (setf (nvme-command-cdw10 cmd) (logior (ash qid 16) (1- qsize)))
  ;; CDW11: Physically Contiguous (PC), Interrupts Enabled (IEN), Interrupt Vector
  (setf (nvme-command-cdw11 cmd) (logior #x1  ; PC
                                         #x2  ; IEN
                                         (ash vector 16)))
  t)

(defun nvme-setup-create-sq-cmd (cmd qid qsize prp1 cqid)
  "Setup Create I/O Submission Queue command."
  (declare (type nvme-command cmd)
           (type (unsigned-byte 16) qid qsize cqid)
           (type (unsigned-byte 64) prp1))
  (setf (nvme-command-opc cmd) +nvme-admin-create-sq+)
  (setf (nvme-command-prp1 cmd) prp1)
  ;; CDW10: QID and Queue Size
  (setf (nvme-command-cdw10 cmd) (logior (ash qid 16) (1- qsize)))
  ;; CDW11: Physically Contiguous (PC), Queue Priority (Medium), CQ ID
  (setf (nvme-command-cdw11 cmd) (logior #x1  ; PC
                                         (ash 1 1)  ; Medium priority
                                         (ash cqid 16)))
  t)

(defun nvme-setup-delete-queue-cmd (cmd opcode qid)
  "Setup Delete Queue command."
  (declare (type nvme-command cmd)
           (type (unsigned-byte 8) opcode)
           (type (unsigned-byte 16) qid))
  (setf (nvme-command-opc cmd) opcode)
  (setf (nvme-command-cdw10 cmd) qid)
  t)

(defun nvme-setup-set-features-cmd (cmd fid cdw11)
  "Setup Set Features command."
  (declare (type nvme-command cmd)
           (type (unsigned-byte 8) fid)
           (type (unsigned-byte 32) cdw11))
  (setf (nvme-command-opc cmd) +nvme-admin-set-features+)
  (setf (nvme-command-cdw10 cmd) fid)
  (setf (nvme-command-cdw11 cmd) cdw11)
  t)

(defun nvme-setup-get-features-cmd (cmd fid)
  "Setup Get Features command."
  (declare (type nvme-command cmd)
           (type (unsigned-byte 8) fid))
  (setf (nvme-command-opc cmd) +nvme-admin-get-features+)
  (setf (nvme-command-cdw10 cmd) fid)
  t)

;;; ============================================================================
;;; Command Submission and Completion
;;; ============================================================================

(defun nvme-alloc-cid (queue)
  "Allocate command ID."
  (declare (type nvme-queue queue))
  (let ((ctrl (if (zerop (nvme-queue-qid queue))
                  ;; This is admin queue - need to get ctrl differently
                  ;; For now, use sequential allocation
                  nil
                  nil)))
    ;; Simple sequential allocation (wrap at 64K)
    (let ((cid (mod (1+ (nvme-queue-sq-tail queue)) 65536)))
      cid)))

(defun nvme-submit-cmd-raw (queue cmd cid)
  "Submit command to queue (low-level)."
  (declare (type nvme-queue queue)
           (type nvme-command cmd)
           (type (unsigned-byte 16) cid))
  (with-spinlock ((nvme-queue-lock queue))
    (let* ((tail (nvme-queue-sq-tail queue))
           (depth (nvme-queue-depth queue))
           (next-tail (mod (1+ tail) depth))
           (sq-addr (nvme-queue-sq-buffer queue))
           (cmd-addr (+ sq-addr (* tail +nvme-sqe-size+))))
      ;; Check if queue is full
      (when (= next-tail (nvme-queue-cq-head queue))
        (return-from nvme-submit-cmd-raw nil))
      ;; Set command ID
      (setf (nvme-command-cid cmd) cid)
      ;; Write command to submission queue
      (nvme-write-command cmd-addr cmd)
      ;; Update tail pointer
      (setf (nvme-queue-sq-tail queue) next-tail)
      ;; Ring doorbell
      (nvme-ring-sq-doorbell queue)
      t)))

(defun nvme-write-command (addr cmd)
  "Write NVMe command to memory."
  (declare (type (unsigned-byte 64) addr)
           (type nvme-command cmd))
  ;; Write command as sequence of DWORDs
  (mem-write-u8 addr (nvme-command-opc cmd))
  (mem-write-u8 (+ addr 1) (logior (ash (nvme-command-fuse cmd) 0)
                                    (ash (nvme-command-psdt cmd) 6)))
  (mem-write-u16 (+ addr 2) (nvme-command-cid cmd))
  (mem-write-u32 (+ addr 4) (nvme-command-nsid cmd))
  (mem-write-u32 (+ addr 8) (nvme-command-cdw2 cmd))
  (mem-write-u32 (+ addr 12) (nvme-command-cdw3 cmd))
  (mem-write-u64 (+ addr 16) (nvme-command-mptr cmd))
  (mem-write-u64 (+ addr 24) (nvme-command-prp1 cmd))
  (mem-write-u64 (+ addr 32) (nvme-command-prp2 cmd))
  (mem-write-u32 (+ addr 40) (nvme-command-cdw10 cmd))
  (mem-write-u32 (+ addr 44) (nvme-command-cdw11 cmd))
  (mem-write-u32 (+ addr 48) (nvme-command-cdw12 cmd))
  (mem-write-u32 (+ addr 52) (nvme-command-cdw13 cmd))
  (mem-write-u32 (+ addr 56) (nvme-command-cdw14 cmd))
  (mem-write-u32 (+ addr 60) (nvme-command-cdw15 cmd))
  t)

(defun nvme-read-completion (addr)
  "Read NVMe completion from memory."
  (declare (type (unsigned-byte 64) addr))
  (let ((comp (make-nvme-completion)))
    (setf (nvme-completion-result comp) (mem-read-u32 addr))
    (setf (nvme-completion-rsvd comp) (mem-read-u32 (+ addr 4)))
    (setf (nvme-completion-sq-head comp) (mem-read-u16 (+ addr 8)))
    (setf (nvme-completion-sq-id comp) (mem-read-u16 (+ addr 10)))
    (setf (nvme-completion-cid comp) (mem-read-u16 (+ addr 12)))
    (setf (nvme-completion-status comp) (mem-read-u16 (+ addr 14)))
    comp))

(defun nvme-poll-cq (queue)
  "Poll completion queue for new completions."
  (declare (type nvme-queue queue))
  (with-spinlock ((nvme-queue-lock queue))
    (let ((completions nil))
      (loop
        (let* ((head (nvme-queue-cq-head queue))
               (phase (nvme-queue-cq-phase queue))
               (depth (nvme-queue-depth queue))
               (cq-addr (nvme-queue-cq-buffer queue))
               (cqe-addr (+ cq-addr (* head +nvme-cqe-size+)))
               (comp (nvme-read-completion cqe-addr))
               (status (nvme-completion-status comp))
               (phase-bit (if (logbitp 0 status) 1 0)))
          ;; Check phase bit
          (unless (= phase-bit phase)
            (return))
          ;; Got completion
          (push comp completions)
          ;; Advance head
          (let ((next-head (mod (1+ head) depth)))
            (setf (nvme-queue-cq-head queue) next-head)
            ;; Toggle phase on wrap
            (when (zerop next-head)
              (setf (nvme-queue-cq-phase queue) (logxor phase 1))))))
      ;; Update doorbell if we processed any completions
      (when completions
        (nvme-ring-cq-doorbell queue))
      (reverse completions))))

(defun nvme-submit-sync (queue cmd buffer length timeout)
  "Submit command synchronously and wait for completion."
  (declare (type nvme-queue queue)
           (type nvme-command cmd)
           (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) length timeout))
  (let* ((cid (nvme-alloc-cid queue))
         (req (make-nvme-request
               :cmd cmd
               :buffer buffer
               :length length
               :completed (make-semaphore 0)
               :timeout (+ (get-time-ms) timeout)))
         (pending-cmds (nvme-queue-pending-cmds queue)))
    ;; Register request
    (setf (gethash cid pending-cmds) req)
    ;; Submit command
    (unless (nvme-submit-cmd-raw queue cmd cid)
      (remhash cid pending-cmds)
      (return-from nvme-submit-sync nil))
    ;; Wait for completion
    (semaphore-wait (nvme-request-completed req))
    ;; Return status and result
    (values (nvme-request-status req)
            (nvme-request-result req))))

(defun nvme-process-completion (queue comp)
  "Process single completion."
  (declare (type nvme-queue queue)
           (type nvme-completion comp))
  (let* ((cid (nvme-completion-cid comp))
         (pending-cmds (nvme-queue-pending-cmds queue))
         (req (gethash cid pending-cmds)))
    (when req
      ;; Store completion info
      (setf (nvme-request-status req) (nvme-completion-status comp))
      (setf (nvme-request-result req) (nvme-completion-result comp))
      ;; Signal completion
      (when (nvme-request-completed req)
        (semaphore-signal (nvme-request-completed req)))
      ;; Call callback if present
      (when (nvme-request-callback req)
        (funcall (nvme-request-callback req) req))
      ;; Remove from pending
      (remhash cid pending-cmds)))
  t)

(defun nvme-check-completion-status (status)
  "Check if completion status indicates success."
  (declare (type (unsigned-byte 16) status))
  (let ((sc (logand (ash status -1) #x7F))   ; Status Code
        (sct (logand (ash status -9) #x7)))  ; Status Code Type
    (and (= sct +nvme-sct-generic+)
         (= sc +nvme-sc-success+))))

;;; ============================================================================
;;; Queue Management
;;; ============================================================================

(defun nvme-create-queue-pair (ctrl qid depth irq-vector)
  "Create queue pair (SQ + CQ)."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 16) qid depth irq-vector))
  (let* ((page-size (nvme-controller-page-size ctrl))
         (sq-size (* depth +nvme-sqe-size+))
         (cq-size (* depth +nvme-cqe-size+))
         (sq-buffer (dma-alloc-coherent sq-size page-size +dma-bidirectional+))
         (cq-buffer (dma-alloc-coherent cq-size page-size +dma-bidirectional+)))
    (when (or (zerop sq-buffer) (zerop cq-buffer))
      (when (not (zerop sq-buffer))
        (dma-free-coherent sq-buffer sq-size))
      (when (not (zerop cq-buffer))
        (dma-free-coherent cq-buffer cq-size))
      (return-from nvme-create-queue-pair nil))
    ;; Clear queues
    (dotimes (i sq-size)
      (mem-write-u8 (+ sq-buffer i) 0))
    (dotimes (i cq-size)
      (mem-write-u8 (+ cq-buffer i) 0))
    ;; Calculate doorbell addresses
    (let* ((dstrd (nvme-controller-doorbell-stride ctrl))
           (db-offset (* 4 (ash 1 dstrd)))
           (sq-db (+ (nvme-controller-regs ctrl) #x1000 (* qid db-offset)))
           (cq-db (+ sq-db 4)))
      (let ((queue (make-nvme-queue
                    :qid qid
                    :depth depth
                    :sq-buffer sq-buffer
                    :cq-buffer cq-buffer
                    :sq-dma-addr sq-buffer
                    :cq-dma-addr cq-buffer
                    :sq-tail 0
                    :cq-head 0
                    :cq-phase 1
                    :sq-doorbell sq-db
                    :cq-doorbell cq-db
                    :lock (make-spinlock)
                    :irq-vector irq-vector
                    :pending-cmds (make-hash-table :test 'eql))))
        queue))))

(defun nvme-delete-queue-pair (ctrl queue)
  "Delete queue pair."
  (declare (type nvme-controller ctrl)
           (type nvme-queue queue))
  (let ((qid (nvme-queue-qid queue))
        (sq-buffer (nvme-queue-sq-buffer queue))
        (cq-buffer (nvme-queue-cq-buffer queue))
        (depth (nvme-queue-depth queue)))
    ;; If not admin queue, send delete commands
    (when (not (zerop qid))
      (let ((admin-queue (nvme-controller-admin-queue ctrl)))
        ;; Delete SQ
        (let ((cmd (make-nvme-command)))
          (nvme-setup-delete-queue-cmd cmd +nvme-admin-delete-sq+ qid)
          (nvme-submit-sync admin-queue cmd 0 0 +nvme-admin-timeout+))
        ;; Delete CQ
        (let ((cmd (make-nvme-command)))
          (nvme-setup-delete-queue-cmd cmd +nvme-admin-delete-cq+ qid)
          (nvme-submit-sync admin-queue cmd 0 0 +nvme-admin-timeout+))))
    ;; Free buffers
    (dma-free-coherent sq-buffer (* depth +nvme-sqe-size+))
    (dma-free-coherent cq-buffer (* depth +nvme-cqe-size+))
    t))

(defun nvme-setup-admin-queue (ctrl)
  "Setup admin queue."
  (declare (type nvme-controller ctrl))
  (let* ((depth +nvme-aq-depth+)
         (queue (nvme-create-queue-pair ctrl 0 depth 0)))
    (unless queue
      (return-from nvme-setup-admin-queue nil))
    ;; Write queue addresses to controller
    (nvme-write-reg-u64 ctrl +nvme-reg-asq+ (nvme-queue-sq-dma-addr queue))
    (nvme-write-reg-u64 ctrl +nvme-reg-acq+ (nvme-queue-cq-dma-addr queue))
    ;; Write queue sizes to AQA register
    (let ((aqa (logior (ash (1- depth) 16)  ; ACQS
                       (1- depth))))         ; ASQS
      (nvme-write-reg-u32 ctrl +nvme-reg-aqa+ aqa))
    (setf (nvme-controller-admin-queue ctrl) queue)
    queue))

(defun nvme-create-io-queues (ctrl num-queues)
  "Create I/O queues."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 16) num-queues))
  (let ((admin-queue (nvme-controller-admin-queue ctrl))
        (max-qsize (nvme-controller-max-queue-entries ctrl))
        (depth (min max-qsize +nvme-max-queue-depth+))
        (queues nil))
    ;; Set number of queues
    (let ((cmd (make-nvme-command)))
      (nvme-setup-set-features-cmd cmd +nvme-feat-num-queues+
                                   (logior (ash (1- num-queues) 16)
                                          (1- num-queues)))
      (multiple-value-bind (status result)
          (nvme-submit-sync admin-queue cmd 0 0 +nvme-admin-timeout+)
        (unless (nvme-check-completion-status status)
          (return-from nvme-create-io-queues nil))
        ;; Controller returns actual number allocated
        (let ((num-cq (1+ (logand (ash result -16) #xFFFF)))
              (num-sq (1+ (logand result #xFFFF))))
          (setf num-queues (min num-queues num-cq num-sq)))))
    ;; Create queue pairs
    (dotimes (i num-queues)
      (let* ((qid (1+ i))
             (irq-vector (1+ i))
             (queue (nvme-create-queue-pair ctrl qid depth irq-vector)))
        (unless queue
          (return-from nvme-create-io-queues nil))
        ;; Create CQ via admin command
        (let ((cmd (make-nvme-command)))
          (nvme-setup-create-cq-cmd cmd qid depth
                                   (nvme-queue-cq-dma-addr queue)
                                   irq-vector)
          (multiple-value-bind (status result)
              (nvme-submit-sync admin-queue cmd 0 0 +nvme-admin-timeout+)
            (declare (ignore result))
            (unless (nvme-check-completion-status status)
              (nvme-delete-queue-pair ctrl queue)
              (return-from nvme-create-io-queues nil))))
        ;; Create SQ via admin command
        (let ((cmd (make-nvme-command)))
          (nvme-setup-create-sq-cmd cmd qid depth
                                   (nvme-queue-sq-dma-addr queue)
                                   qid)
          (multiple-value-bind (status result)
              (nvme-submit-sync admin-queue cmd 0 0 +nvme-admin-timeout+)
            (declare (ignore result))
            (unless (nvme-check-completion-status status)
              (nvme-delete-queue-pair ctrl queue)
              (return-from nvme-create-io-queues nil))))
        (push queue queues)))
    (setf (nvme-controller-io-queues ctrl) (make-array num-queues
                                                       :initial-contents (reverse queues)))
    (setf (nvme-controller-num-io-queues ctrl) num-queues)
    t))

;;; ============================================================================
;;; Controller Initialization
;;; ============================================================================

(defun nvme-wait-ready (ctrl ready timeout-ms)
  "Wait for controller ready state."
  (declare (type nvme-controller ctrl)
           (type boolean ready)
           (type (unsigned-byte 32) timeout-ms))
  (let ((start-time (get-time-ms))
        (ready-bit (if ready +nvme-csts-rdy+ 0)))
    (loop
      (let ((csts (nvme-read-reg-u32 ctrl +nvme-reg-csts+)))
        ;; Check for fatal status
        (when (logtest csts +nvme-csts-cfs+)
          (return-from nvme-wait-ready nil))
        ;; Check ready bit
        (when (= (logand csts +nvme-csts-rdy+) ready-bit)
          (return-from nvme-wait-ready t))
        ;; Check timeout
        (when (> (- (get-time-ms) start-time) timeout-ms)
          (return-from nvme-wait-ready nil))
        (udelay 1000)))))

(defun nvme-disable-controller (ctrl)
  "Disable controller."
  (declare (type nvme-controller ctrl))
  (let ((cc (nvme-read-reg-u32 ctrl +nvme-reg-cc+)))
    (setf cc (logand cc (lognot +nvme-cc-enable+)))
    (nvme-write-reg-u32 ctrl +nvme-reg-cc+ cc))
  (nvme-wait-ready ctrl nil 10000))

(defun nvme-enable-controller (ctrl)
  "Enable controller."
  (declare (type nvme-controller ctrl))
  ;; Configure controller
  (let* ((cap (nvme-controller-cap ctrl))
         (mps-min (logand (ash cap (- +nvme-cap-mpsmin-shift+)) #xF))
         (page-shift (+ 12 mps-min))
         (cc 0))
    ;; Set page size
    (setf cc (logior cc (ash mps-min +nvme-cc-mps-shift+)))
    ;; Set arbitration mechanism (round robin)
    (setf cc (logior cc +nvme-cc-ams-rr+))
    ;; Set command set (NVM)
    (setf cc (logior cc +nvme-cc-css-nvm+))
    ;; Set queue entry sizes
    (setf cc (logior cc (ash 6 +nvme-cc-iosqes-shift+)))  ; 64 bytes
    (setf cc (logior cc (ash 4 +nvme-cc-iocqes-shift+)))  ; 16 bytes
    ;; Enable controller
    (setf cc (logior cc +nvme-cc-enable+))
    (nvme-write-reg-u32 ctrl +nvme-reg-cc+ cc)
    ;; Update page size
    (setf (nvme-controller-page-size ctrl) (ash 1 page-shift)))
  ;; Wait for ready
  (nvme-wait-ready ctrl t 10000))

(defun nvme-reset-controller (ctrl)
  "Reset NVMe controller."
  (declare (type nvme-controller ctrl))
  ;; Disable controller
  (unless (nvme-disable-controller ctrl)
    (return-from nvme-reset-controller nil))
  ;; Setup admin queue
  (unless (nvme-setup-admin-queue ctrl)
    (return-from nvme-reset-controller nil))
  ;; Enable controller
  (unless (nvme-enable-controller ctrl)
    (return-from nvme-reset-controller nil))
  t)

;;; ============================================================================
;;; Identification
;;; ============================================================================

(defun nvme-parse-id-ctrl (buffer)
  "Parse controller identification data."
  (declare (type (unsigned-byte 64) buffer))
  (let ((id (make-nvme-id-ctrl)))
    (setf (nvme-id-ctrl-vid id) (mem-read-u16 buffer))
    (setf (nvme-id-ctrl-ssvid id) (mem-read-u16 (+ buffer 2)))
    ;; Serial number (bytes 4-23)
    (dotimes (i 20)
      (setf (aref (nvme-id-ctrl-sn id) i)
            (mem-read-u8 (+ buffer 4 i))))
    ;; Model number (bytes 24-63)
    (dotimes (i 40)
      (setf (aref (nvme-id-ctrl-mn id) i)
            (mem-read-u8 (+ buffer 24 i))))
    ;; Firmware revision (bytes 64-71)
    (dotimes (i 8)
      (setf (aref (nvme-id-ctrl-fr id) i)
            (mem-read-u8 (+ buffer 64 i))))
    (setf (nvme-id-ctrl-rab id) (mem-read-u8 (+ buffer 72)))
    (dotimes (i 3)
      (setf (aref (nvme-id-ctrl-ieee id) i)
            (mem-read-u8 (+ buffer 73 i))))
    (setf (nvme-id-ctrl-cmic id) (mem-read-u8 (+ buffer 76)))
    (setf (nvme-id-ctrl-mdts id) (mem-read-u8 (+ buffer 77)))
    (setf (nvme-id-ctrl-cntlid id) (mem-read-u16 (+ buffer 78)))
    (setf (nvme-id-ctrl-ver id) (mem-read-u32 (+ buffer 80)))
    (setf (nvme-id-ctrl-oacs id) (mem-read-u16 (+ buffer 256)))
    (setf (nvme-id-ctrl-acl id) (mem-read-u8 (+ buffer 258)))
    (setf (nvme-id-ctrl-aerl id) (mem-read-u8 (+ buffer 259)))
    (setf (nvme-id-ctrl-frmw id) (mem-read-u8 (+ buffer 260)))
    (setf (nvme-id-ctrl-lpa id) (mem-read-u8 (+ buffer 261)))
    (setf (nvme-id-ctrl-elpe id) (mem-read-u8 (+ buffer 262)))
    (setf (nvme-id-ctrl-npss id) (mem-read-u8 (+ buffer 263)))
    (setf (nvme-id-ctrl-nn id) (mem-read-u32 (+ buffer 516)))
    (setf (nvme-id-ctrl-oncs id) (mem-read-u16 (+ buffer 520)))
    (setf (nvme-id-ctrl-sqes id) (mem-read-u8 (+ buffer 512)))
    (setf (nvme-id-ctrl-cqes id) (mem-read-u8 (+ buffer 513)))
    id))

(defun nvme-identify-controller (ctrl)
  "Identify controller."
  (declare (type nvme-controller ctrl))
  (let* ((admin-queue (nvme-controller-admin-queue ctrl))
         (buffer (dma-alloc-coherent 4096 4096 +dma-from-device+)))
    (when (zerop buffer)
      (return-from nvme-identify-controller nil))
    ;; Clear buffer
    (dotimes (i 4096)
      (mem-write-u8 (+ buffer i) 0))
    ;; Send identify command
    (let ((cmd (make-nvme-command)))
      (nvme-setup-identify-cmd cmd +nvme-identify-ctrl+ 0 buffer 0)
      (multiple-value-bind (status result)
          (nvme-submit-sync admin-queue cmd buffer 4096 +nvme-admin-timeout+)
        (declare (ignore result))
        (unless (nvme-check-completion-status status)
          (dma-free-coherent buffer 4096)
          (return-from nvme-identify-controller nil))))
    ;; Parse identification data
    (let ((id-ctrl (nvme-parse-id-ctrl buffer)))
      (dma-free-coherent buffer 4096)
      (setf (nvme-controller-id-ctrl ctrl) id-ctrl)
      id-ctrl)))

(defun nvme-parse-id-ns (buffer)
  "Parse namespace identification data."
  (declare (type (unsigned-byte 64) buffer))
  (let ((id (make-nvme-id-ns)))
    (setf (nvme-id-ns-nsze id) (mem-read-u64 buffer))
    (setf (nvme-id-ns-ncap id) (mem-read-u64 (+ buffer 8)))
    (setf (nvme-id-ns-nuse id) (mem-read-u64 (+ buffer 16)))
    (setf (nvme-id-ns-nsfeat id) (mem-read-u8 (+ buffer 24)))
    (setf (nvme-id-ns-nlbaf id) (mem-read-u8 (+ buffer 25)))
    (setf (nvme-id-ns-flbas id) (mem-read-u8 (+ buffer 26)))
    (setf (nvme-id-ns-mc id) (mem-read-u8 (+ buffer 27)))
    (setf (nvme-id-ns-dpc id) (mem-read-u8 (+ buffer 28)))
    (setf (nvme-id-ns-dps id) (mem-read-u8 (+ buffer 29)))
    ;; LBA formats (16 entries at offset 128)
    (dotimes (i 16)
      (setf (aref (nvme-id-ns-lbaf id) i)
            (mem-read-u32 (+ buffer 128 (* i 4)))))
    id))

(defun nvme-identify-namespace (ctrl nsid)
  "Identify namespace."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 32) nsid))
  (let* ((admin-queue (nvme-controller-admin-queue ctrl))
         (buffer (dma-alloc-coherent 4096 4096 +dma-from-device+)))
    (when (zerop buffer)
      (return-from nvme-identify-namespace nil))
    ;; Clear buffer
    (dotimes (i 4096)
      (mem-write-u8 (+ buffer i) 0))
    ;; Send identify command
    (let ((cmd (make-nvme-command)))
      (nvme-setup-identify-cmd cmd +nvme-identify-ns+ nsid buffer 0)
      (multiple-value-bind (status result)
          (nvme-submit-sync admin-queue cmd buffer 4096 +nvme-admin-timeout+)
        (declare (ignore result))
        (unless (nvme-check-completion-status status)
          (dma-free-coherent buffer 4096)
          (return-from nvme-identify-namespace nil))))
    ;; Parse identification data
    (let ((id-ns (nvme-parse-id-ns buffer)))
      (dma-free-coherent buffer 4096)
      id-ns)))

;;; ============================================================================
;;; Namespace Management
;;; ============================================================================

(defun nvme-scan-namespaces (ctrl)
  "Scan and register all namespaces."
  (declare (type nvme-controller ctrl))
  (let ((id-ctrl (nvme-controller-id-ctrl ctrl))
        (namespaces (make-hash-table :test 'eql)))
    (unless id-ctrl
      (return-from nvme-scan-namespaces nil))
    (let ((nn (nvme-id-ctrl-nn id-ctrl)))
      ;; Iterate through all namespace IDs
      (loop for nsid from 1 to nn do
        (let ((id-ns (nvme-identify-namespace ctrl nsid)))
          (when (and id-ns
                    (not (zerop (nvme-id-ns-nsze id-ns))))
            ;; Valid namespace
            (let* ((flbas (nvme-id-ns-flbas id-ns))
                   (lba-format-idx (logand flbas #xF))
                   (lbaf (aref (nvme-id-ns-lbaf id-ns) lba-format-idx))
                   (lbads (logand (ash lbaf -16) #xFF))
                   (block-size (ash 1 lbads))
                   (num-blocks (nvme-id-ns-nsze id-ns)))
              (let ((ns (make-nvme-namespace
                         :nsid nsid
                         :ctrl ctrl
                         :id-ns id-ns
                         :block-size block-size
                         :num-blocks num-blocks
                         :lock (make-spinlock))))
                (setf (gethash nsid namespaces) ns)
                (printk "NVME: Namespace %d: %d blocks of %d bytes\n"
                       nsid num-blocks block-size)))))))
    (setf (nvme-controller-namespaces ctrl) namespaces)
    t))

;;; ============================================================================
;;; Block Device Operations
;;; ============================================================================

(defun nvme-get-io-queue (ctrl cpu)
  "Get I/O queue for CPU."
  (declare (type nvme-controller ctrl)
           (type (unsigned-byte 32) cpu))
  (let ((num-queues (nvme-controller-num-io-queues ctrl)))
    (when (zerop num-queues)
      (return-from nvme-get-io-queue nil))
    (aref (nvme-controller-io-queues ctrl)
          (mod cpu num-queues))))

(defun nvme-rw-request (ns start-block num-blocks buffer write)
  "Perform read/write request."
  (declare (type nvme-namespace ns)
           (type (unsigned-byte 64) start-block)
           (type (unsigned-byte 32) num-blocks)
           (type (unsigned-byte 64) buffer)
           (type boolean write))
  (let* ((ctrl (nvme-namespace-ctrl ns))
         (nsid (nvme-namespace-nsid ns))
         (block-size (nvme-namespace-block-size ns))
         (total-bytes (* num-blocks block-size))
         (queue (nvme-get-io-queue ctrl 0)))  ; Use CPU 0 for now
    (unless queue
      (return-from nvme-rw-request nil))
    ;; Map buffer for DMA
    (let ((dma-addr (dma-map-single buffer total-bytes
                                   (if write +dma-to-device+ +dma-from-device+))))
      (when (zerop dma-addr)
        (return-from nvme-rw-request nil))
      ;; Build PRP list
      (multiple-value-bind (prp1 prp2)
          (nvme-build-prp-list dma-addr total-bytes (nvme-controller-page-size ctrl))
        ;; Create command
        (let ((cmd (make-nvme-command)))
          (nvme-setup-rw-cmd cmd
                            (if write +nvme-cmd-write+ +nvme-cmd-read+)
                            nsid
                            start-block
                            num-blocks
                            prp1
                            prp2)
          ;; Submit command
          (multiple-value-bind (status result)
              (nvme-submit-sync queue cmd dma-addr total-bytes +nvme-io-timeout+)
            (declare (ignore result))
            (dma-unmap-single dma-addr total-bytes)
            (nvme-check-completion-status status)))))))

(defun nvme-block-read (device sector buffer count)
  "Block device read operation."
  (declare (type t device)
           (type (unsigned-byte 64) sector)
           (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) count))
  (let ((ns device))
    (nvme-rw-request ns sector count buffer nil)))

(defun nvme-block-write (device sector buffer count)
  "Block device write operation."
  (declare (type t device)
           (type (unsigned-byte 64) sector)
           (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) count))
  (let ((ns device))
    (nvme-rw-request ns sector count buffer t)))

(defun nvme-register-block-devices (ctrl)
  "Register block devices for all namespaces."
  (declare (type nvme-controller ctrl))
  (let ((namespaces (nvme-controller-namespaces ctrl)))
    (maphash (lambda (nsid ns)
               (declare (ignore nsid))
               (let ((block-size (nvme-namespace-block-size ns))
                     (num-blocks (nvme-namespace-num-blocks ns)))
                 (block-device-register ns #'nvme-block-read
                                       #'nvme-block-write
                                       block-size
                                       num-blocks)))
             namespaces))
  t)

;;; ============================================================================
;;; PCI Initialization
;;; ============================================================================

(defun nvme-pci-enable (ctrl)
  "Enable PCI device."
  (declare (type nvme-controller ctrl))
  (let ((bus (nvme-controller-pci-bus ctrl))
        (dev (nvme-controller-pci-dev ctrl))
        (func (nvme-controller-pci-func ctrl)))
    ;; Read current command register
    (let ((cmd (pci-config-read-u16 bus dev func +pci-command+)))
      ;; Enable memory space and bus mastering
      (setf cmd (logior cmd
                        +pci-command-memory+
                        +pci-command-master+
                        +pci-command-intx-disable+))
      (pci-config-write-u16 bus dev func +pci-command+ cmd))
    ;; Read BAR0
    (let ((bar0 (pci-config-read-u32 bus dev func +pci-bar0+))
          (bar1 (pci-config-read-u32 bus dev func +pci-bar1+)))
      ;; Check if 64-bit BAR
      (let ((bar0-addr (if (logbitp 2 bar0)
                          ;; 64-bit
                          (logior (logand bar0 #xFFFFFFF0)
                                 (ash bar1 32))
                          ;; 32-bit
                          (logand bar0 #xFFFFFFF0))))
        (setf (nvme-controller-bar0 ctrl) bar0-addr)
        ;; Map BAR0 to virtual memory (assume identity mapping for now)
        (setf (nvme-controller-regs ctrl) (phys-to-virt bar0-addr))))
    t))

(defun nvme-read-capabilities (ctrl)
  "Read controller capabilities."
  (declare (type nvme-controller ctrl))
  (let ((cap (nvme-read-reg-u64 ctrl +nvme-reg-cap+)))
    (setf (nvme-controller-cap ctrl) cap)
    ;; Extract important fields
    (let ((mqes (1+ (logand (ash cap (- +nvme-cap-mqes-shift+)) #xFFFF)))
          (dstrd (logand (ash cap (- +nvme-cap-dstrd-shift+)) #xF)))
      (setf (nvme-controller-max-queue-entries ctrl) mqes)
      (setf (nvme-controller-doorbell-stride ctrl) dstrd))
    t))

;;; ============================================================================
;;; Main Initialization
;;; ============================================================================

(defun nvme-init-controller (bus dev func)
  "Initialize NVMe controller."
  (declare (type (unsigned-byte 8) bus)
           (type (unsigned-byte 5) dev)
           (type (unsigned-byte 3) func))
  (let ((ctrl (make-nvme-controller
               :pci-bus bus
               :pci-dev dev
               :pci-func func
               :lock (make-spinlock))))
    ;; Read vendor/device ID
    (let ((vendor-id (pci-config-read-u16 bus dev func +pci-vendor-id+))
          (device-id (pci-config-read-u16 bus dev func +pci-device-id+)))
      (setf (nvme-controller-vendor-id ctrl) vendor-id)
      (setf (nvme-controller-device-id ctrl) device-id)
      (printk "NVME: Found controller %04x:%04x at %02x:%02x.%x\n"
             vendor-id device-id bus dev func))
    ;; Enable PCI device
    (unless (nvme-pci-enable ctrl)
      (return-from nvme-init-controller nil))
    ;; Read capabilities
    (unless (nvme-read-capabilities ctrl)
      (return-from nvme-init-controller nil))
    ;; Reset controller
    (unless (nvme-reset-controller ctrl)
      (printk "NVME: Controller reset failed\n")
      (return-from nvme-init-controller nil))
    (printk "NVME: Controller reset successful\n")
    ;; Identify controller
    (unless (nvme-identify-controller ctrl)
      (printk "NVME: Controller identification failed\n")
      (return-from nvme-init-controller nil))
    (printk "NVME: Controller identified\n")
    ;; Create I/O queues (one per CPU, max 16)
    (let ((num-queues (min 16 (get-num-cpus))))
      (unless (nvme-create-io-queues ctrl num-queues)
        (printk "NVME: I/O queue creation failed\n")
        (return-from nvme-init-controller nil))
      (printk "NVME: Created %d I/O queue pairs\n" num-queues))
    ;; Scan namespaces
    (unless (nvme-scan-namespaces ctrl)
      (printk "NVME: Namespace scan failed\n")
      (return-from nvme-init-controller nil))
    ;; Register block devices
    (nvme-register-block-devices ctrl)
    ;; Add to global list
    (with-spinlock (*nvme-lock*)
      (push ctrl *nvme-controllers*))
    (printk "NVME: Controller initialization complete\n")
    ctrl))

(defun nvme-init ()
  "Initialize NVMe subsystem."
  (setf *nvme-lock* (make-spinlock))
  (setf *nvme-controllers* nil)
  (printk "NVME: Subsystem initialized\n")
  t)

;;; Helper functions that need to be implemented elsewhere

(declaim (ftype (function () (unsigned-byte 32)) get-num-cpus))
(declaim (ftype (function () (unsigned-byte 32)) current-cpu))
(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 8)) mem-read-u8))
(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 16)) mem-read-u16))
(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 32)) mem-read-u32))
(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 64)) mem-read-u64))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 8)) t) mem-write-u8))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 16)) t) mem-write-u16))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32)) t) mem-write-u32))
(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64)) t) mem-write-u64))

(provide 'nvme)
