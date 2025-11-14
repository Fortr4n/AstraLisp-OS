;; AstraLisp OS Kernel IPC - Capabilities
;; Production capability-based security system

(defpackage :astralisp-capabilities
  (:use :cl)
  (:export :capability-create
           :capability-check
           :capability-grant
           :capability-revoke))

(in-package :astralisp-capabilities)

;; Capability types
(defconstant +cap-file+ 0)
(defconstant +cap-socket+ 1)
(defconstant +cap-memory+ 2)
(defconstant +cap-device+ 3)
(defconstant +cap-process+ 4)

;; Capability structure
(defstruct capability
  "Security capability."
  (id 0 :type (unsigned-byte 64))
  (type 0 :type (unsigned-byte 8))
  (object-id 0 :type (unsigned-byte 64))
  (permissions 0 :type (unsigned-byte 32))
  (owner 0 :type (unsigned-byte 32))  ; Process PID
  (ref-count 0 :type (unsigned-byte 32)))

;; Capability system state
(defvar *capabilities* (make-hash-table))
(defvar *capability-id-counter* 0)

;; Create capability
(defun capability-create (type object-id permissions owner)
  "Create new capability."
  (let ((cap (make-capability
              :id (incf *capability-id-counter*)
              :type type
              :object-id object-id
              :permissions permissions
              :owner owner)))
    (setf (gethash (capability-id cap) *capabilities*) cap)
    cap))

;; Check capability
(defun capability-check (cap-id permission)
  "Check if capability grants permission."
  (let ((cap (gethash cap-id *capabilities*)))
    (when cap
      (not (zerop (logand (capability-permissions cap) permission))))))

;; Grant capability
(defun capability-grant (cap-id target-pid)
  "Grant capability to process."
  (let ((cap (gethash cap-id *capabilities*)))
    (when cap
      (incf (capability-ref-count cap))
      cap-id)))

;; Revoke capability
(defun capability-revoke (cap-id)
  "Revoke capability."
  (let ((cap (gethash cap-id *capabilities*)))
    (when cap
      (decf (capability-ref-count cap))
      (when (zerop (capability-ref-count cap))
        (remhash cap-id *capabilities*)))))

