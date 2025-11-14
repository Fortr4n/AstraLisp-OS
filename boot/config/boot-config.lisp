;; AstraLisp OS Boot Configuration Parser
;; Parses boot parameters and configuration

(defpackage :astralisp-boot-config
  (:use :cl)
  (:export :parse-boot-params
           :get-boot-param
           :boot-config))

(in-package :astralisp-boot-config)

(defvar *boot-params* (make-hash-table :test 'equal))

(defun parse-boot-params (cmdline)
  "Parse boot command line parameters.
   Format: key1=value1 key2=value2 ..."
  (clrhash *boot-params*)
  (when cmdline
    (let ((tokens (uiop:split-string cmdline :separator " ")))
      (dolist (token tokens)
        (let ((pos (position #\= token)))
          (if pos
              (let ((key (subseq token 0 pos))
                    (value (subseq token (1+ pos))))
                (setf (gethash key *boot-params*) value))
              (setf (gethash token *boot-params*) t))))))
  *boot-params*)

(defun get-boot-param (key &optional default)
  "Get boot parameter value."
  (gethash key *boot-params* default))

(defstruct boot-config
  "Boot configuration structure."
  (debug nil :type boolean)
  (maxcpus 4 :type integer)
  (memory nil :type (or null integer))
  (initrd nil :type (or null string))
  (root-device nil :type (or null string))
  (console "serial" :type string))

(defun make-boot-config-from-params ()
  "Create boot configuration from parsed parameters."
  (make-boot-config
   :debug (get-boot-param "debug" nil)
   :maxcpus (parse-integer (get-boot-param "maxcpus" "4") :junk-allowed t)
   :memory (let ((mem (get-boot-param "memory")))
             (when mem
               (parse-integer mem :junk-allowed t)))
   :initrd (get-boot-param "initrd")
   :root-device (get-boot-param "root")
   :console (get-boot-param "console" "serial")))

