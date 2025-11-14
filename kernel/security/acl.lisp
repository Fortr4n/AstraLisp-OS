;; AstraLisp OS Kernel Security - Access Control Lists
;; Production ACL implementation

(defpackage :astralisp-acl
  (:use :cl)
  (:export :acl-create
           :acl-check
           :acl-add-entry
           :acl-remove-entry))

(in-package :astralisp-acl)

;; ACL entry
(defstruct acl-entry
  "ACL entry."
  (subject 0 :type (unsigned-byte 32))  ; Process/User ID
  (permissions 0 :type (unsigned-byte 32))
  (type 0 :type (unsigned-byte 8)))  ; Allow/Deny

;; ACL structure
(defstruct acl
  "Access Control List."
  (entries nil :type list)
  (default-permissions 0 :type (unsigned-byte 32)))

;; Create ACL
(defun acl-create (&key (default-permissions 0))
  "Create new ACL."
  (make-acl :default-permissions default-permissions))

;; Check ACL
(defun acl-check (acl subject permission)
  "Check if subject has permission in ACL."
  (let ((entry (find subject (acl-entries acl) :key #'acl-entry-subject)))
    (if entry
        (not (zerop (logand (acl-entry-permissions entry) permission)))
        (not (zerop (logand (acl-default-permissions acl) permission))))))

;; Add ACL entry
(defun acl-add-entry (acl subject permissions)
  "Add entry to ACL."
  (push (make-acl-entry :subject subject :permissions permissions)
        (acl-entries acl)))

;; Remove ACL entry
(defun acl-remove-entry (acl subject)
  "Remove entry from ACL."
  (setf (acl-entries acl)
        (remove subject (acl-entries acl) :key #'acl-entry-subject)))

