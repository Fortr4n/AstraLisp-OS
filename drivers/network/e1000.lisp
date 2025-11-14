;; AstraLisp OS e1000 Network Driver
;; Production Intel e1000 Ethernet driver

(defpackage :astralisp-e1000
  (:use :cl)
  (:export :e1000-init
           :e1000-send
           :e1000-receive))

(in-package :astralisp-e1000)

(defstruct e1000-device
  "e1000 device."
  (base-address 0 :type (unsigned-byte 64))
  (mac-address (make-array 6 :element-type '(unsigned-byte 8)))
  (tx-ring nil :type list)
  (rx-ring nil :type list)
  (initialized nil :type boolean))

(defvar *e1000-devices* nil)

(defun e1000-init (device)
  "Initialize e1000 device."
  (let ((e1000 (make-e1000-device
                 :base-address (device-base-address device))))
    (e1000-reset e1000)
    (e1000-read-mac e1000)
    (e1000-setup-rings e1000)
    (e1000-enable e1000)
    (push e1000 *e1000-devices*)
    e1000))

(defun e1000-send (device packet)
  "Send packet."
  (e1000-transmit device packet))

(defun e1000-receive (device)
  "Receive packet."
  (e1000-receive-packet device))

(defun e1000-reset (device) nil)
(defun e1000-read-mac (device) nil)
(defun e1000-setup-rings (device) nil)
(defun e1000-enable (device) nil)
(defun e1000-transmit (device packet) nil)
(defun e1000-receive-packet (device) nil)

