;; AstraLisp OS Kernel IPC - Capabilities
;; Production capability-based security system with delegation and revocation

(defpackage :astralisp-capabilities
  (:use :cl)
  (:export :cap-init
           :cap-create
           :cap-derive
           :cap-check
           :cap-grant
           :cap-revoke
           :cap-revoke-tree
           :cap-seal
           :cap-unseal
           :cap-validate
           :cap-marshal
           :cap-unmarshal
           :cap-get-permissions
           :cap-set-domain
           :with-capability))

(in-package :astralisp-capabilities)

;;; Capability Type Constants

(defconstant +cap-type-null+ 0 "Null capability")
(defconstant +cap-type-memory+ 1 "Memory region capability")
(defconstant +cap-type-file+ 2 "File capability")
(defconstant +cap-type-directory+ 3 "Directory capability")
(defconstant +cap-type-socket+ 4 "Socket capability")
(defconstant +cap-type-device+ 5 "Device capability")
(defconstant +cap-type-process+ 6 "Process capability")
(defconstant +cap-type-thread+ 7 "Thread capability")
(defconstant +cap-type-ipc-channel+ 8 "IPC channel capability")
(defconstant +cap-type-rpc-interface+ 9 "RPC interface capability")
(defconstant +cap-type-semaphore+ 10 "Semaphore capability")
(defconstant +cap-type-mutex+ 11 "Mutex capability")
(defconstant +cap-type-event+ 12 "Event capability")
(defconstant +cap-type-timer+ 13 "Timer capability")
(defconstant +cap-type-interrupt+ 14 "Interrupt capability")
(defconstant +cap-type-dma+ 15 "DMA capability")
(defconstant +cap-type-framebuffer+ 16 "Framebuffer capability")
(defconstant +cap-type-namespace+ 17 "Namespace capability")
(defconstant +cap-type-capability+ 18 "Capability to create capabilities")

;;; Permission Flags (32-bit bitmask)

;; Generic permissions (0-7)
(defconstant +perm-read+ #x00000001 "Read permission")
(defconstant +perm-write+ #x00000002 "Write permission")
(defconstant +perm-execute+ #x00000004 "Execute permission")
(defconstant +perm-grant+ #x00000008 "Grant permission (delegate)")
(defconstant +perm-derive+ #x00000010 "Derive permission (attenuate)")
(defconstant +perm-revoke+ #x00000020 "Revoke permission")
(defconstant +perm-seal+ #x00000040 "Seal permission")
(defconstant +perm-unseal+ #x00000080 "Unseal permission")

;; Memory-specific permissions (8-15)
(defconstant +perm-mem-map+ #x00000100 "Map memory")
(defconstant +perm-mem-unmap+ #x00000200 "Unmap memory")
(defconstant +perm-mem-resize+ #x00000400 "Resize memory")
(defconstant +perm-mem-share+ #x00000800 "Share memory")
(defconstant +perm-mem-cache-control+ #x00001000 "Cache control")

;; File-specific permissions (16-23)
(defconstant +perm-file-create+ #x00010000 "Create files")
(defconstant +perm-file-delete+ #x00020000 "Delete files")
(defconstant +perm-file-rename+ #x00040000 "Rename files")
(defconstant +perm-file-stat+ #x00080000 "Stat files")
(defconstant +perm-file-chmod+ #x00100000 "Change permissions")

;; Process-specific permissions (24-31)
(defconstant +perm-proc-kill+ #x01000000 "Kill process")
(defconstant +perm-proc-suspend+ #x02000000 "Suspend process")
(defconstant +perm-proc-debug+ #x04000000 "Debug process")
(defconstant +perm-proc-priority+ #x08000000 "Change priority")

;;; Capability Flags

(defconstant +cap-flag-sealed+ #x0001 "Capability is sealed")
(defconstant +cap-flag-ephemeral+ #x0002 "Capability is ephemeral")
(defconstant +cap-flag-transferable+ #x0004 "Capability can be transferred")
(defconstant +cap-flag-delegable+ #x0008 "Capability can be delegated")
(defconstant +cap-flag-revocable+ #x0010 "Capability can be revoked")
(defconstant +cap-flag-expired+ #x0020 "Capability has expired")

;;; Data Structures

(defstruct capability
  "Security capability with fine-grained permissions."
  (id 0 :type (unsigned-byte 64))              ; Unique capability ID
  (type 0 :type (unsigned-byte 8))             ; Capability type
  (object-id 0 :type (unsigned-byte 64))       ; Protected object ID
  (permissions 0 :type (unsigned-byte 32))      ; Permission bitmask
  (flags 0 :type (unsigned-byte 16))           ; Capability flags
  (owner-pid 0 :type (unsigned-byte 32))       ; Owning process
  (creator-pid 0 :type (unsigned-byte 32))     ; Creating process
  (parent-id 0 :type (unsigned-byte 64))       ; Parent capability ID (for derivation)
  (domain-id 0 :type (unsigned-byte 32))       ; Security domain
  (ref-count 0 :type (unsigned-byte 32))       ; Reference count
  (seal-key 0 :type (unsigned-byte 64))        ; Seal key (if sealed)
  (creation-time 0 :type (unsigned-byte 64))   ; Creation timestamp
  (expiration-time 0 :type (unsigned-byte 64)) ; Expiration timestamp (0 = never)
  (bounds-start 0 :type (unsigned-byte 64))    ; Start address/offset (for memory/file caps)
  (bounds-end 0 :type (unsigned-byte 64))      ; End address/offset
  (metadata nil :type list))                   ; Additional metadata

(defstruct capability-domain
  "Security domain for capability isolation."
  (id 0 :type (unsigned-byte 32))
  (name "" :type string)
  (parent-id 0 :type (unsigned-byte 32))
  (capabilities (make-hash-table) :type hash-table)
  (policies nil :type list))

(defstruct capability-revocation-list
  "Capability revocation tracking."
  (revoked-ids (make-hash-table) :type hash-table)
  (revocation-tree (make-hash-table) :type hash-table))  ; Maps parent -> children

(defstruct capability-audit-entry
  "Audit log entry for capability operations."
  (timestamp 0 :type (unsigned-byte 64))
  (operation nil :type symbol)
  (cap-id 0 :type (unsigned-byte 64))
  (pid 0 :type (unsigned-byte 32))
  (success nil :type boolean)
  (reason "" :type string))

;;; Global State

(defvar *cap-initialized* nil)
(defvar *capabilities* (make-hash-table) "Global capability table")
(defvar *capability-id-counter* 0 "Capability ID counter")
(defvar *capability-domains* (make-hash-table) "Security domains")
(defvar *domain-id-counter* 0 "Domain ID counter")
(defvar *revocation-list* nil "Capability revocation tracking")
(defvar *capability-lock* nil "Global capability lock")
(defvar *audit-log* nil "Capability audit log")
(defvar *audit-enabled* t "Enable audit logging")
(defvar *max-audit-entries* 10000 "Maximum audit log entries")

;;; Initialization

(defun cap-init ()
  "Initialize capability system."
  (when *cap-initialized*
    (return-from cap-init t))
  (setf *capability-lock* (make-mutex))
  (setf *revocation-list* (make-capability-revocation-list))
  (setf *audit-log* nil)

  ;; Create default domain
  (let ((default-domain (make-capability-domain
                         :id 0
                         :name "default"
                         :parent-id 0)))
    (setf (gethash 0 *capability-domains*) default-domain))

  (setf *cap-initialized* t)
  t)

;;; Capability Creation

(defun cap-create (type object-id permissions &key
                   (bounds-start 0)
                   (bounds-end 0)
                   (expiration 0)
                   (domain-id 0)
                   (flags +cap-flag-transferable+)
                   (metadata nil))
  "Create new capability with specified permissions and bounds."
  (declare (type (unsigned-byte 8) type)
           (type (unsigned-byte 64) object-id)
           (type (unsigned-byte 32) permissions)
           (type (unsigned-byte 64) bounds-start bounds-end expiration)
           (type (unsigned-byte 32) domain-id)
           (type (unsigned-byte 16) flags))

  (with-mutex (*capability-lock*)
    (let* ((current-pid (get-current-pid))
           (cap-id (atomic-incf *capability-id-counter*))
           (current-time (get-tick-count))
           (cap (make-capability
                 :id cap-id
                 :type type
                 :object-id object-id
                 :permissions permissions
                 :flags flags
                 :owner-pid current-pid
                 :creator-pid current-pid
                 :parent-id 0
                 :domain-id domain-id
                 :ref-count 1
                 :seal-key 0
                 :creation-time current-time
                 :expiration-time expiration
                 :bounds-start bounds-start
                 :bounds-end bounds-end
                 :metadata metadata)))

      ;; Validate capability
      (unless (validate-capability-internal cap)
        (audit-capability-operation 'create cap-id current-pid nil "Invalid capability parameters")
        (return-from cap-create nil))

      ;; Store capability
      (setf (gethash cap-id *capabilities*) cap)

      ;; Add to domain
      (let ((domain (gethash domain-id *capability-domains*)))
        (when domain
          (setf (gethash cap-id (capability-domain-capabilities domain)) t)))

      ;; Audit
      (audit-capability-operation 'create cap-id current-pid t "")

      cap-id)))

(defun cap-derive (parent-cap-id new-permissions &key
                   (new-bounds-start nil)
                   (new-bounds-end nil)
                   (new-expiration nil)
                   (new-flags nil))
  "Derive new capability from parent with attenuated permissions."
  (declare (type (unsigned-byte 64) parent-cap-id)
           (type (unsigned-byte 32) new-permissions))

  (with-mutex (*capability-lock*)
    (let ((parent-cap (gethash parent-cap-id *capabilities*))
          (current-pid (get-current-pid)))

      (unless parent-cap
        (audit-capability-operation 'derive parent-cap-id current-pid nil "Parent capability not found")
        (return-from cap-derive nil))

      ;; Check if parent capability allows derivation
      (unless (logtest (capability-permissions parent-cap) +perm-derive+)
        (audit-capability-operation 'derive parent-cap-id current-pid nil "Derive permission denied")
        (return-from cap-derive nil))

      ;; Check ownership or grant permission
      (unless (or (= (capability-owner-pid parent-cap) current-pid)
                  (logtest (capability-permissions parent-cap) +perm-grant+))
        (audit-capability-operation 'derive parent-cap-id current-pid nil "Not owner")
        (return-from cap-derive nil))

      ;; Ensure new permissions are subset of parent (attenuation only)
      (unless (= (logand new-permissions (capability-permissions parent-cap))
                 new-permissions)
        (audit-capability-operation 'derive parent-cap-id current-pid nil "Permission escalation attempt")
        (return-from cap-derive nil))

      ;; Derive bounds (must be within parent bounds)
      (let* ((derived-start (if new-bounds-start
                               (max new-bounds-start (capability-bounds-start parent-cap))
                               (capability-bounds-start parent-cap)))
             (derived-end (if new-bounds-end
                             (min new-bounds-end (capability-bounds-end parent-cap))
                             (capability-bounds-end parent-cap)))
             (derived-exp (if new-expiration
                             (if (zerop (capability-expiration-time parent-cap))
                                 new-expiration
                                 (min new-expiration (capability-expiration-time parent-cap)))
                             (capability-expiration-time parent-cap)))
             (derived-flags (if new-flags
                               (logand new-flags (capability-flags parent-cap))
                               (capability-flags parent-cap)))
             (cap-id (atomic-incf *capability-id-counter*))
             (current-time (get-tick-count))
             (derived-cap (make-capability
                           :id cap-id
                           :type (capability-type parent-cap)
                           :object-id (capability-object-id parent-cap)
                           :permissions new-permissions
                           :flags derived-flags
                           :owner-pid current-pid
                           :creator-pid current-pid
                           :parent-id parent-cap-id
                           :domain-id (capability-domain-id parent-cap)
                           :ref-count 1
                           :seal-key 0
                           :creation-time current-time
                           :expiration-time derived-exp
                           :bounds-start derived-start
                           :bounds-end derived-end
                           :metadata (capability-metadata parent-cap))))

        ;; Validate bounds
        (when (> derived-start derived-end)
          (audit-capability-operation 'derive parent-cap-id current-pid nil "Invalid bounds")
          (return-from cap-derive nil))

        ;; Store derived capability
        (setf (gethash cap-id *capabilities*) derived-cap)

        ;; Track derivation for revocation
        (let ((children (gethash parent-cap-id
                                (capability-revocation-list-revocation-tree *revocation-list*))))
          (push cap-id children)
          (setf (gethash parent-cap-id
                        (capability-revocation-list-revocation-tree *revocation-list*))
                children))

        ;; Audit
        (audit-capability-operation 'derive cap-id current-pid t "")

        cap-id))))

;;; Capability Checking and Validation

(defun cap-check (cap-id required-permissions &key (check-bounds nil) (address 0) (length 0))
  "Check if capability grants required permissions."
  (declare (type (unsigned-byte 64) cap-id)
           (type (unsigned-byte 32) required-permissions)
           (type (unsigned-byte 64) address length))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*))
          (current-pid (get-current-pid)))

      (unless cap
        (return-from cap-check nil))

      ;; Check if revoked
      (when (gethash cap-id (capability-revocation-list-revoked-ids *revocation-list*))
        (return-from cap-check nil))

      ;; Check ownership
      (unless (= (capability-owner-pid cap) current-pid)
        (return-from cap-check nil))

      ;; Check expiration
      (when (and (not (zerop (capability-expiration-time cap)))
                 (>= (get-tick-count) (capability-expiration-time cap)))
        (setf (capability-flags cap)
              (logior (capability-flags cap) +cap-flag-expired+))
        (return-from cap-check nil))

      ;; Check sealed
      (when (logtest (capability-flags cap) +cap-flag-sealed+)
        (return-from cap-check nil))

      ;; Check permissions
      (unless (= (logand (capability-permissions cap) required-permissions)
                 required-permissions)
        (return-from cap-check nil))

      ;; Check bounds if requested
      (when check-bounds
        (let ((access-start address)
              (access-end (+ address length)))
          (unless (and (>= access-start (capability-bounds-start cap))
                      (<= access-end (capability-bounds-end cap)))
            (return-from cap-check nil))))

      t)))

(defun cap-validate (cap-id)
  "Validate capability integrity and status."
  (declare (type (unsigned-byte 64) cap-id))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*)))
      (when (null cap)
        (return-from cap-validate nil))

      (validate-capability-internal cap))))

(defun validate-capability-internal (cap)
  "Internal capability validation."
  (declare (type capability cap))

  ;; Check type bounds
  (when (> (capability-type cap) +cap-type-capability+)
    (return-from validate-capability-internal nil))

  ;; Check bounds consistency
  (when (and (not (zerop (capability-bounds-end cap)))
             (> (capability-bounds-start cap) (capability-bounds-end cap)))
    (return-from validate-capability-internal nil))

  ;; Check expiration
  (when (and (not (zerop (capability-expiration-time cap)))
             (>= (get-tick-count) (capability-expiration-time cap)))
    (return-from validate-capability-internal nil))

  ;; Check revocation
  (when (gethash (capability-id cap)
                (capability-revocation-list-revoked-ids *revocation-list*))
    (return-from validate-capability-internal nil))

  t)

;;; Capability Grant and Revocation

(defun cap-grant (cap-id target-pid &key (transfer nil))
  "Grant capability to another process."
  (declare (type (unsigned-byte 64) cap-id)
           (type (unsigned-byte 32) target-pid)
           (type boolean transfer))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*))
          (current-pid (get-current-pid)))

      (unless cap
        (audit-capability-operation 'grant cap-id current-pid nil "Capability not found")
        (return-from cap-grant nil))

      ;; Check ownership
      (unless (= (capability-owner-pid cap) current-pid)
        (audit-capability-operation 'grant cap-id current-pid nil "Not owner")
        (return-from cap-grant nil))

      ;; Check grant permission
      (unless (logtest (capability-permissions cap) +perm-grant+)
        (audit-capability-operation 'grant cap-id current-pid nil "Grant permission denied")
        (return-from cap-grant nil))

      ;; Check transferable flag
      (unless (logtest (capability-flags cap) +cap-flag-transferable+)
        (audit-capability-operation 'grant cap-id current-pid nil "Not transferable")
        (return-from cap-grant nil))

      (if transfer
          ;; Transfer ownership
          (progn
            (setf (capability-owner-pid cap) target-pid)
            (audit-capability-operation 'transfer cap-id current-pid t ""))
          ;; Grant copy (increment ref count)
          (progn
            (incf (capability-ref-count cap))
            (audit-capability-operation 'grant cap-id current-pid t "")))

      cap-id)))

(defun cap-revoke (cap-id)
  "Revoke capability (direct revocation only)."
  (declare (type (unsigned-byte 64) cap-id))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*))
          (current-pid (get-current-pid)))

      (unless cap
        (return-from cap-revoke nil))

      ;; Check ownership or revoke permission
      (unless (or (= (capability-owner-pid cap) current-pid)
                  (= (capability-creator-pid cap) current-pid)
                  (logtest (capability-permissions cap) +perm-revoke+))
        (audit-capability-operation 'revoke cap-id current-pid nil "Permission denied")
        (return-from cap-revoke nil))

      ;; Check revocable flag
      (unless (logtest (capability-flags cap) +cap-flag-revocable+)
        (audit-capability-operation 'revoke cap-id current-pid nil "Not revocable")
        (return-from cap-revoke nil))

      ;; Mark as revoked
      (setf (gethash cap-id (capability-revocation-list-revoked-ids *revocation-list*)) t)

      ;; Decrement ref count
      (decf (capability-ref-count cap))
      (when (<= (capability-ref-count cap) 0)
        (remhash cap-id *capabilities*))

      ;; Audit
      (audit-capability-operation 'revoke cap-id current-pid t "")

      t)))

(defun cap-revoke-tree (cap-id)
  "Revoke capability and all derived capabilities (recursive)."
  (declare (type (unsigned-byte 64) cap-id))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*))
          (current-pid (get-current-pid)))

      (unless cap
        (return-from cap-revoke-tree nil))

      ;; Check ownership or revoke permission
      (unless (or (= (capability-owner-pid cap) current-pid)
                  (= (capability-creator-pid cap) current-pid))
        (audit-capability-operation 'revoke-tree cap-id current-pid nil "Permission denied")
        (return-from cap-revoke-tree nil))

      ;; Revoke all children recursively
      (let ((children (gethash cap-id
                              (capability-revocation-list-revocation-tree *revocation-list*))))
        (dolist (child-id children)
          (cap-revoke-tree child-id)))

      ;; Revoke this capability
      (cap-revoke cap-id)

      ;; Remove from revocation tree
      (remhash cap-id (capability-revocation-list-revocation-tree *revocation-list*))

      t)))

;;; Sealing and Unsealing

(defun cap-seal (cap-id seal-key)
  "Seal capability to prevent modification and inspection."
  (declare (type (unsigned-byte 64) cap-id seal-key))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*))
          (current-pid (get-current-pid)))

      (unless cap
        (return-from cap-seal nil))

      ;; Check ownership
      (unless (= (capability-owner-pid cap) current-pid)
        (audit-capability-operation 'seal cap-id current-pid nil "Not owner")
        (return-from cap-seal nil))

      ;; Check seal permission
      (unless (logtest (capability-permissions cap) +perm-seal+)
        (audit-capability-operation 'seal cap-id current-pid nil "Seal permission denied")
        (return-from cap-seal nil))

      ;; Already sealed?
      (when (logtest (capability-flags cap) +cap-flag-sealed+)
        (return-from cap-seal nil))

      ;; Seal capability
      (setf (capability-seal-key cap) seal-key)
      (setf (capability-flags cap)
            (logior (capability-flags cap) +cap-flag-sealed+))

      ;; Audit
      (audit-capability-operation 'seal cap-id current-pid t "")

      t)))

(defun cap-unseal (cap-id seal-key)
  "Unseal capability with correct key."
  (declare (type (unsigned-byte 64) cap-id seal-key))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*))
          (current-pid (get-current-pid)))

      (unless cap
        (return-from cap-unseal nil))

      ;; Check sealed
      (unless (logtest (capability-flags cap) +cap-flag-sealed+)
        (return-from cap-unseal nil))

      ;; Check unseal permission
      (unless (logtest (capability-permissions cap) +perm-unseal+)
        (audit-capability-operation 'unseal cap-id current-pid nil "Unseal permission denied")
        (return-from cap-unseal nil))

      ;; Verify seal key
      (unless (= (capability-seal-key cap) seal-key)
        (audit-capability-operation 'unseal cap-id current-pid nil "Invalid seal key")
        (return-from cap-unseal nil))

      ;; Unseal capability
      (setf (capability-seal-key cap) 0)
      (setf (capability-flags cap)
            (logand (capability-flags cap) (lognot +cap-flag-sealed+)))

      ;; Audit
      (audit-capability-operation 'unseal cap-id current-pid t "")

      t)))

;;; Capability Domains

(defun cap-create-domain (name parent-id)
  "Create new security domain."
  (declare (type string name)
           (type (unsigned-byte 32) parent-id))

  (with-mutex (*capability-lock*)
    (let ((domain-id (atomic-incf *domain-id-counter*))
          (domain (make-capability-domain
                   :id domain-id
                   :name name
                   :parent-id parent-id)))
      (setf (gethash domain-id *capability-domains*) domain)
      domain-id)))

(defun cap-set-domain (cap-id domain-id)
  "Move capability to different domain."
  (declare (type (unsigned-byte 64) cap-id)
           (type (unsigned-byte 32) domain-id))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*))
          (domain (gethash domain-id *capability-domains*))
          (current-pid (get-current-pid)))

      (unless (and cap domain)
        (return-from cap-set-domain nil))

      ;; Check ownership
      (unless (= (capability-owner-pid cap) current-pid)
        (return-from cap-set-domain nil))

      ;; Remove from old domain
      (let ((old-domain (gethash (capability-domain-id cap) *capability-domains*)))
        (when old-domain
          (remhash cap-id (capability-domain-capabilities old-domain))))

      ;; Add to new domain
      (setf (capability-domain-id cap) domain-id)
      (setf (gethash cap-id (capability-domain-capabilities domain)) t)

      t)))

;;; Capability Permissions

(defun cap-get-permissions (cap-id)
  "Get capability permissions."
  (declare (type (unsigned-byte 64) cap-id))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*)))
      (when cap
        (capability-permissions cap)))))

;;; Marshalling for RPC

(defun cap-marshal (cap-id)
  "Marshal capability for transmission."
  (declare (type (unsigned-byte 64) cap-id))

  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*)))
      (unless cap
        (return-from cap-marshal nil))

      ;; Create capability descriptor for marshalling
      (list :cap-id (capability-id cap)
            :type (capability-type cap)
            :permissions (capability-permissions cap)
            :flags (capability-flags cap)
            :object-id (capability-object-id cap)
            :bounds-start (capability-bounds-start cap)
            :bounds-end (capability-bounds-end cap)
            :domain-id (capability-domain-id cap)))))

(defun cap-unmarshal (cap-descriptor sender-pid)
  "Unmarshal capability from transmission."
  (declare (type list cap-descriptor)
           (type (unsigned-byte 32) sender-pid))

  (with-mutex (*capability-lock*)
    (let* ((remote-cap-id (getf cap-descriptor :cap-id))
           (remote-cap (gethash remote-cap-id *capabilities*)))

      (unless remote-cap
        (return-from cap-unmarshal nil))

      ;; Verify sender owns the capability
      (unless (= (capability-owner-pid remote-cap) sender-pid)
        (return-from cap-unmarshal nil))

      ;; Check transferable flag
      (unless (logtest (capability-flags remote-cap) +cap-flag-transferable+)
        (return-from cap-unmarshal nil))

      ;; Create reference to existing capability
      (incf (capability-ref-count remote-cap))

      remote-cap-id)))

;;; Audit Logging

(defun audit-capability-operation (operation cap-id pid success reason)
  "Log capability operation to audit trail."
  (declare (type symbol operation)
           (type (unsigned-byte 64) cap-id)
           (type (unsigned-byte 32) pid)
           (type boolean success)
           (type string reason))

  (when *audit-enabled*
    (let ((entry (make-capability-audit-entry
                  :timestamp (get-tick-count)
                  :operation operation
                  :cap-id cap-id
                  :pid pid
                  :success success
                  :reason reason)))
      (push entry *audit-log*)

      ;; Limit audit log size
      (when (> (length *audit-log*) *max-audit-entries*)
        (setf *audit-log* (subseq *audit-log* 0 *max-audit-entries*)))))
  nil)

(defun cap-get-audit-log (&key (count 100))
  "Get recent audit log entries."
  (declare (type (unsigned-byte 32) count))
  (subseq *audit-log* 0 (min count (length *audit-log*))))

;;; Convenience Macro

(defmacro with-capability ((cap-id permissions &key bounds address length) &body body)
  "Execute body with capability check."
  `(if (cap-check ,cap-id ,permissions
                  :check-bounds ,bounds
                  :address ,(or address 0)
                  :length ,(or length 0))
       (progn ,@body)
       (error "Capability check failed for ~A" ,cap-id)))

;;; Helper Functions

(defun get-current-pid ()
  "Get current process ID (forward declaration)."
  1)

(defun get-tick-count ()
  "Get system tick count (forward declaration)."
  0)

(defun make-mutex ()
  "Create mutex (forward declaration)."
  nil)

(defmacro with-mutex ((mutex) &body body)
  "Execute body with mutex held (forward declaration)."
  `(progn ,@body))

(defun atomic-incf (place)
  "Atomic increment (forward declaration)."
  (incf place))

;;; Capability Type Predicates

(defun cap-is-memory-cap (cap-id)
  "Check if capability is memory capability."
  (declare (type (unsigned-byte 64) cap-id))
  (let ((cap (gethash cap-id *capabilities*)))
    (and cap (= (capability-type cap) +cap-type-memory+))))

(defun cap-is-file-cap (cap-id)
  "Check if capability is file capability."
  (declare (type (unsigned-byte 64) cap-id))
  (let ((cap (gethash cap-id *capabilities*)))
    (and cap (= (capability-type cap) +cap-type-file+))))

(defun cap-is-process-cap (cap-id)
  "Check if capability is process capability."
  (declare (type (unsigned-byte 64) cap-id))
  (let ((cap (gethash cap-id *capabilities*)))
    (and cap (= (capability-type cap) +cap-type-process+))))

;;; Statistics and Debugging

(defun cap-get-stats ()
  "Get capability system statistics."
  (with-mutex (*capability-lock*)
    (list :total-capabilities (hash-table-count *capabilities*)
          :total-domains (hash-table-count *capability-domains*)
          :revoked-count (hash-table-count
                         (capability-revocation-list-revoked-ids *revocation-list*))
          :audit-entries (length *audit-log*))))

(defun cap-dump-capability (cap-id)
  "Dump capability details for debugging."
  (declare (type (unsigned-byte 64) cap-id))
  (with-mutex (*capability-lock*)
    (let ((cap (gethash cap-id *capabilities*)))
      (when cap
        (format t "Capability ~A:~%" cap-id)
        (format t "  Type: ~A~%" (capability-type cap))
        (format t "  Permissions: ~8,'0X~%" (capability-permissions cap))
        (format t "  Flags: ~4,'0X~%" (capability-flags cap))
        (format t "  Owner PID: ~A~%" (capability-owner-pid cap))
        (format t "  Object ID: ~A~%" (capability-object-id cap))
        (format t "  Bounds: [~A, ~A]~%"
                (capability-bounds-start cap)
                (capability-bounds-end cap))
        (format t "  Ref Count: ~A~%" (capability-ref-count cap))
        (format t "  Domain: ~A~%" (capability-domain-id cap))
        t))))

