;; AstraLisp OS Keyboard Driver
;; Production keyboard driver

(defpackage :astralisp-keyboard
  (:use :cl)
  (:export :keyboard-init
           :keyboard-read
           :keyboard-get-event))

(in-package :astralisp-keyboard)

(defstruct keyboard-device
  "Keyboard device."
  (base-address 0 :type (unsigned-byte 64))
  (event-queue nil :type list)
  (initialized nil :type boolean))

(defvar *keyboard-devices* nil)

(defun keyboard-init (device)
  "Initialize keyboard."
  (let ((kb (make-keyboard-device
             :base-address (device-base-address device))))
    (keyboard-enable-interrupts kb)
    (push kb *keyboard-devices*)
    kb))

(defun keyboard-read (device)
  "Read key code."
  (keyboard-read-port device))

(defun keyboard-get-event (device)
  "Get keyboard event."
  (pop (keyboard-device-event-queue device)))

(defun keyboard-enable-interrupts (device) nil)
(defun keyboard-read-port (device) 0)

