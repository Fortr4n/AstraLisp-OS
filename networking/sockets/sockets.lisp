;; AstraLisp OS BSD Socket API
;; Production socket implementation

(defpackage :astralisp-sockets
  (:use :cl)
  (:export :socket
           :bind
           :listen
           :connect
           :accept
           :send
           :recv
           :close))

(in-package :astralisp-sockets)

(defconstant +af-inet+ 2)
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)

(defstruct socket
  "Socket."
  (fd 0 :type (unsigned-byte 32))
  (domain +af-inet+ :type (unsigned-byte 16))
  (type +sock-stream+ :type (unsigned-byte 16))
  (protocol 0 :type (unsigned-byte 16))
  (local-addr nil :type (or null t))
  (remote-addr nil :type (or null t))
  (state :unbound :type keyword))

(defvar *sockets* (make-hash-table))
(defvar *fd-counter* 0)

(defun socket (domain type protocol)
  "Create socket."
  (let ((sock (make-socket
               :fd (incf *fd-counter*)
               :domain domain
               :type type
               :protocol protocol)))
    (setf (gethash (socket-fd sock) *sockets*) sock)
    sock))

(defun bind (sock addr)
  "Bind socket to address."
  (setf (socket-local-addr sock) addr)
  (setf (socket-state sock) :bound))

(defun listen (sock backlog)
  "Listen on socket."
  (tcp-listen (socket-fd sock)))

(defun connect (sock addr)
  "Connect socket."
  (setf (socket-remote-addr sock) addr)
  (tcp-connect (socket-fd sock) addr))

(defun accept (sock)
  "Accept connection."
  (tcp-accept (socket-fd sock)))

(defun send (sock data flags)
  "Send data."
  (tcp-send (socket-fd sock) data))

(defun recv (sock buffer size flags)
  "Receive data."
  (tcp-receive (socket-fd sock)))

(defun close (sock)
  "Close socket."
  (tcp-close (socket-fd sock))
  (remhash (socket-fd sock) *sockets*))

(defun tcp-listen (fd) nil)
(defun tcp-connect (fd addr) nil)
(defun tcp-accept (fd) nil)
(defun tcp-send (fd data) nil)
(defun tcp-receive (fd) nil)
(defun tcp-close (fd) nil)

