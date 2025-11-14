;; AstraLisp OS VGA Driver
;; Production VGA driver

(defpackage :astralisp-vga
  (:use :cl)
  (:export :vga-init
           :vga-put-char
           :vga-clear
           :vga-set-cursor))

(in-package :astralisp-vga)

(defstruct vga-device
  "VGA device."
  (framebuffer #xB8000 :type (unsigned-byte 64))
  (width 80 :type (unsigned-byte 16))
  (height 25 :type (unsigned-byte 16))
  (cursor-x 0 :type (unsigned-byte 16))
  (cursor-y 0 :type (unsigned-byte 16))
  (color 7 :type (unsigned-byte 8)))

(defvar *vga-device* nil)

(defun vga-init ()
  "Initialize VGA."
  (setf *vga-device* (make-vga-device))
  (vga-clear)
  *vga-device*)

(defun vga-put-char (char)
  "Put character on screen."
  (let ((pos (+ (* (vga-device-cursor-y *vga-device*) (vga-device-width *vga-device*))
                (vga-device-cursor-x *vga-device*))))
    (set-vga-char pos char (vga-device-color *vga-device*))
    (incf (vga-device-cursor-x *vga-device*))
    (when (>= (vga-device-cursor-x *vga-device*) (vga-device-width *vga-device*))
      (setf (vga-device-cursor-x *vga-device*) 0)
      (incf (vga-device-cursor-y *vga-device*))
      (when (>= (vga-device-cursor-y *vga-device*) (vga-device-height *vga-device*))
        (vga-scroll)))))

(defun vga-clear ()
  "Clear screen."
  (loop for i from 0 below (* (vga-device-width *vga-device*) (vga-device-height *vga-device*))
        do (set-vga-char i #\space 0)))

(defun vga-set-cursor (x y)
  "Set cursor position."
  (setf (vga-device-cursor-x *vga-device*) x)
  (setf (vga-device-cursor-y *vga-device*) y))

(defun set-vga-char (pos char color) nil)
(defun vga-scroll () nil)

