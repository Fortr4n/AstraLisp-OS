;; AstraLisp OS Kernel IPC - Remote Procedure Calls
;; Production RPC implementation with comprehensive marshalling

(defpackage :astralisp-rpc
  (:use :cl)
  (:export :rpc-init
           :rpc-call
           :rpc-call-async
           :rpc-wait
           :rpc-cancel
           :rpc-register
           :rpc-unregister
           :rpc-handle
           :rpc-define-interface
           :marshal-value
           :unmarshal-value))

(in-package :astralisp-rpc)

;;; RPC Protocol Constants

(defconstant +rpc-version+ 1 "RPC protocol version")
(defconstant +rpc-magic+ #x52504321 "RPC magic number 'RPC!'")

;; RPC message types
(defconstant +rpc-type-call+ 0 "RPC call request")
(defconstant +rpc-type-reply+ 1 "RPC call reply")
(defconstant +rpc-type-cancel+ 2 "RPC call cancellation")
(defconstant +rpc-type-error+ 3 "RPC error response")

;; RPC status codes
(defconstant +rpc-status-pending+ 0 "RPC call pending")
(defconstant +rpc-status-success+ 1 "RPC call succeeded")
(defconstant +rpc-status-error+ 2 "RPC call failed")
(defconstant +rpc-status-timeout+ 3 "RPC call timed out")
(defconstant +rpc-status-cancelled+ 4 "RPC call cancelled")
(defconstant +rpc-status-no-handler+ 5 "No RPC handler found")
(defconstant +rpc-status-marshal-error+ 6 "Marshalling error")
(defconstant +rpc-status-unmarshal-error+ 7 "Unmarshalling error")

;; Type tags for marshalling
(defconstant +type-nil+ 0)
(defconstant +type-integer+ 1)
(defconstant +type-float+ 2)
(defconstant +type-string+ 3)
(defconstant +type-symbol+ 4)
(defconstant +type-list+ 5)
(defconstant +type-vector+ 6)
(defconstant +type-hash-table+ 7)
(defconstant +type-struct+ 8)
(defconstant +type-capability+ 9)
(defconstant +type-reference+ 10)  ; For circular references
(defconstant +type-bignum+ 11)
(defconstant +type-rational+ 12)
(defconstant +type-complex+ 13)
(defconstant +type-pathname+ 14)
(defconstant +type-byte-vector+ 15)

;;; RPC Data Structures

(defstruct rpc-header
  "RPC message header."
  (magic +rpc-magic+ :type (unsigned-byte 32))
  (version +rpc-version+ :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8))
  (flags 0 :type (unsigned-byte 8))
  (call-id 0 :type (unsigned-byte 64))
  (sequence 0 :type (unsigned-byte 32))
  (payload-size 0 :type (unsigned-byte 32))
  (checksum 0 :type (unsigned-byte 32)))

(defstruct rpc-call
  "RPC call structure."
  (id 0 :type (unsigned-byte 64))
  (interface-id 0 :type (unsigned-byte 32))
  (function-id 0 :type (unsigned-byte 32))
  (args nil :type list)
  (result nil :type t)
  (status +rpc-status-pending+ :type (unsigned-byte 8))
  (error-code 0 :type (unsigned-byte 32))
  (error-message nil :type (or null string))
  (sender-pid 0 :type (unsigned-byte 32))
  (receiver-pid 0 :type (unsigned-byte 32))
  (timeout 0 :type (unsigned-byte 64))
  (start-time 0 :type (unsigned-byte 64))
  (async nil :type boolean)
  (callback nil :type (or null function))
  (semaphore nil :type t))

(defstruct rpc-interface
  "RPC interface definition."
  (id 0 :type (unsigned-byte 32))
  (name "" :type string)
  (version 0 :type (unsigned-byte 16))
  (functions (make-hash-table) :type hash-table)
  (capabilities nil :type list))

(defstruct rpc-function
  "RPC function descriptor."
  (id 0 :type (unsigned-byte 32))
  (name "" :type string)
  (handler nil :type (or null function))
  (arg-types nil :type list)
  (return-type nil :type symbol)
  (required-capabilities nil :type list))

(defstruct marshal-context
  "Marshalling context for handling circular references."
  (buffer nil :type (or null (vector (unsigned-byte 8))))
  (position 0 :type (unsigned-byte 32))
  (object-table (make-hash-table :test 'eq) :type hash-table)
  (reference-counter 0 :type (unsigned-byte 32)))

(defstruct unmarshal-context
  "Unmarshalling context."
  (buffer nil :type (or null (vector (unsigned-byte 8))))
  (position 0 :type (unsigned-byte 32))
  (reference-table (make-hash-table) :type hash-table))

;;; RPC State

(defvar *rpc-initialized* nil)
(defvar *rpc-interfaces* (make-hash-table) "Registered RPC interfaces")
(defvar *rpc-call-id-counter* 0 "RPC call ID counter")
(defvar *pending-calls* (make-hash-table) "Pending RPC calls")
(defvar *rpc-lock* nil "Global RPC lock")

;;; RPC Initialization

(defun rpc-init ()
  "Initialize RPC subsystem."
  (when *rpc-initialized*
    (return-from rpc-init t))
  (setf *rpc-lock* (make-mutex))
  (setf *rpc-initialized* t)
  t)

;;; RPC Interface Management

(defun rpc-define-interface (interface-id name version)
  "Define RPC interface."
  (declare (type (unsigned-byte 32) interface-id)
           (type string name)
           (type (unsigned-byte 16) version))
  (with-mutex (*rpc-lock*)
    (let ((interface (make-rpc-interface
                      :id interface-id
                      :name name
                      :version version)))
      (setf (gethash interface-id *rpc-interfaces*) interface)
      interface)))

(defun rpc-register (interface-id function-id name handler arg-types return-type &key capabilities)
  "Register RPC function handler."
  (declare (type (unsigned-byte 32) interface-id function-id)
           (type string name)
           (type function handler)
           (type list arg-types))
  (with-mutex (*rpc-lock*)
    (let ((interface (gethash interface-id *rpc-interfaces*)))
      (when (null interface)
        (error "RPC interface ~A not found" interface-id))
      (let ((func (make-rpc-function
                   :id function-id
                   :name name
                   :handler handler
                   :arg-types arg-types
                   :return-type return-type
                   :required-capabilities capabilities)))
        (setf (gethash function-id (rpc-interface-functions interface)) func)
        t))))

(defun rpc-unregister (interface-id function-id)
  "Unregister RPC function."
  (declare (type (unsigned-byte 32) interface-id function-id))
  (with-mutex (*rpc-lock*)
    (let ((interface (gethash interface-id *rpc-interfaces*)))
      (when interface
        (remhash function-id (rpc-interface-functions interface))
        t))))

;;; Marshalling Implementation

(defun marshal-value (value context)
  "Marshal value to byte stream."
  (declare (type marshal-context context))

  ;; Check for circular references
  (let ((ref-id (gethash value (marshal-context-object-table context))))
    (when ref-id
      (marshal-u8 +type-reference+ context)
      (marshal-u32 ref-id context)
      (return-from marshal-value t)))

  ;; Record object for circular reference detection
  (when (or (consp value) (vectorp value) (hash-table-p value) (structurep value))
    (let ((ref-id (incf (marshal-context-reference-counter context))))
      (setf (gethash value (marshal-context-object-table context)) ref-id)))

  ;; Marshal by type
  (cond
    ((null value)
     (marshal-u8 +type-nil+ context))

    ((integerp value)
     (if (typep value '(signed-byte 64))
         (progn
           (marshal-u8 +type-integer+ context)
           (marshal-i64 value context))
         (progn
           (marshal-u8 +type-bignum+ context)
           (marshal-bignum value context))))

    ((floatp value)
     (marshal-u8 +type-float+ context)
     (marshal-f64 value context))

    ((stringp value)
     (marshal-u8 +type-string+ context)
     (marshal-string value context))

    ((symbolp value)
     (marshal-u8 +type-symbol+ context)
     (marshal-symbol value context))

    ((consp value)
     (marshal-u8 +type-list+ context)
     (marshal-list value context))

    ((vectorp value)
     (if (typep value '(vector (unsigned-byte 8)))
         (progn
           (marshal-u8 +type-byte-vector+ context)
           (marshal-byte-vector value context))
         (progn
           (marshal-u8 +type-vector+ context)
           (marshal-vector value context))))

    ((hash-table-p value)
     (marshal-u8 +type-hash-table+ context)
     (marshal-hash-table value context))

    ((rationalp value)
     (marshal-u8 +type-rational+ context)
     (marshal-rational value context))

    ((complexp value)
     (marshal-u8 +type-complex+ context)
     (marshal-complex value context))

    ((pathnamep value)
     (marshal-u8 +type-pathname+ context)
     (marshal-pathname value context))

    ((structurep value)
     (marshal-u8 +type-struct+ context)
     (marshal-struct value context))

    (t
     (error "Cannot marshal value of type ~A" (type-of value)))))

(defun unmarshal-value (context)
  "Unmarshal value from byte stream."
  (declare (type unmarshal-context context))
  (let ((type-tag (unmarshal-u8 context)))
    (case type-tag
      (#.+type-nil+ nil)
      (#.+type-integer+ (unmarshal-i64 context))
      (#.+type-float+ (unmarshal-f64 context))
      (#.+type-string+ (unmarshal-string context))
      (#.+type-symbol+ (unmarshal-symbol context))
      (#.+type-list+ (unmarshal-list context))
      (#.+type-vector+ (unmarshal-vector context))
      (#.+type-byte-vector+ (unmarshal-byte-vector context))
      (#.+type-hash-table+ (unmarshal-hash-table context))
      (#.+type-bignum+ (unmarshal-bignum context))
      (#.+type-rational+ (unmarshal-rational context))
      (#.+type-complex+ (unmarshal-complex context))
      (#.+type-pathname+ (unmarshal-pathname context))
      (#.+type-struct+ (unmarshal-struct context))
      (#.+type-reference+
       (let ((ref-id (unmarshal-u32 context)))
         (gethash ref-id (unmarshal-context-reference-table context))))
      (t (error "Unknown type tag: ~A" type-tag)))))

;;; Primitive Marshalling Functions

(defun marshal-u8 (value context)
  "Marshal unsigned 8-bit integer."
  (declare (type (unsigned-byte 8) value)
           (type marshal-context context))
  (let ((pos (marshal-context-position context))
        (buf (marshal-context-buffer context)))
    (setf (aref buf pos) value)
    (setf (marshal-context-position context) (1+ pos))))

(defun marshal-u32 (value context)
  "Marshal unsigned 32-bit integer (big-endian)."
  (declare (type (unsigned-byte 32) value)
           (type marshal-context context))
  (marshal-u8 (logand (ash value -24) #xFF) context)
  (marshal-u8 (logand (ash value -16) #xFF) context)
  (marshal-u8 (logand (ash value -8) #xFF) context)
  (marshal-u8 (logand value #xFF) context))

(defun marshal-u64 (value context)
  "Marshal unsigned 64-bit integer (big-endian)."
  (declare (type (unsigned-byte 64) value)
           (type marshal-context context))
  (marshal-u32 (logand (ash value -32) #xFFFFFFFF) context)
  (marshal-u32 (logand value #xFFFFFFFF) context))

(defun marshal-i64 (value context)
  "Marshal signed 64-bit integer."
  (declare (type (signed-byte 64) value)
           (type marshal-context context))
  (marshal-u64 (logand value #xFFFFFFFFFFFFFFFF) context))

(defun marshal-f64 (value context)
  "Marshal 64-bit float (IEEE 754)."
  (declare (type double-float value)
           (type marshal-context context))
  ;; Convert float to IEEE 754 representation
  (multiple-value-bind (significand exponent sign)
      (decode-float value)
    (let ((bits (encode-ieee754-double significand exponent sign)))
      (marshal-u64 bits context))))

(defun marshal-string (value context)
  "Marshal string with length prefix."
  (declare (type string value)
           (type marshal-context context))
  (let ((bytes (string-to-utf8 value)))
    (marshal-u32 (length bytes) context)
    (dotimes (i (length bytes))
      (marshal-u8 (aref bytes i) context))))

(defun marshal-symbol (value context)
  "Marshal symbol (package + name)."
  (declare (type symbol value)
           (type marshal-context context))
  (let ((package-name (if (symbol-package value)
                          (package-name (symbol-package value))
                          ""))
        (symbol-name (symbol-name value)))
    (marshal-string package-name context)
    (marshal-string symbol-name context)))

(defun marshal-list (value context)
  "Marshal list with length prefix."
  (declare (type list value)
           (type marshal-context context))
  (let ((length (length value)))
    (marshal-u32 length context)
    (dolist (item value)
      (marshal-value item context))))

(defun marshal-vector (value context)
  "Marshal vector with length prefix."
  (declare (type vector value)
           (type marshal-context context))
  (marshal-u32 (length value) context)
  (dotimes (i (length value))
    (marshal-value (aref value i) context)))

(defun marshal-byte-vector (value context)
  "Marshal byte vector efficiently."
  (declare (type (vector (unsigned-byte 8)) value)
           (type marshal-context context))
  (marshal-u32 (length value) context)
  (dotimes (i (length value))
    (marshal-u8 (aref value i) context)))

(defun marshal-hash-table (value context)
  "Marshal hash table."
  (declare (type hash-table value)
           (type marshal-context context))
  (let ((count (hash-table-count value)))
    (marshal-u32 count context)
    (maphash (lambda (k v)
               (marshal-value k context)
               (marshal-value v context))
             value)))

(defun marshal-rational (value context)
  "Marshal rational number."
  (declare (type rational value)
           (type marshal-context context))
  (marshal-i64 (numerator value) context)
  (marshal-i64 (denominator value) context))

(defun marshal-complex (value context)
  "Marshal complex number."
  (declare (type complex value)
           (type marshal-context context))
  (marshal-f64 (realpart value) context)
  (marshal-f64 (imagpart value) context))

(defun marshal-pathname (value context)
  "Marshal pathname."
  (declare (type pathname value)
           (type marshal-context context))
  (marshal-string (namestring value) context))

(defun marshal-bignum (value context)
  "Marshal arbitrary precision integer."
  (declare (type integer value)
           (type marshal-context context))
  ;; Convert to base-256 representation
  (let* ((negative (minusp value))
         (abs-value (abs value))
         (bytes nil))
    (loop while (> abs-value 0) do
      (push (logand abs-value #xFF) bytes)
      (setf abs-value (ash abs-value -8)))
    (marshal-u8 (if negative 1 0) context)
    (marshal-u32 (length bytes) context)
    (dolist (byte bytes)
      (marshal-u8 byte context))))

(defun marshal-struct (value context)
  "Marshal structure (limited support)."
  (declare (type marshal-context context))
  ;; Marshal struct type name and slot values
  (let ((type-name (type-of value)))
    (marshal-symbol type-name context)
    ;; For production use, would need reflection API to get slots
    (error "Struct marshalling not fully implemented for type ~A" type-name)))

;;; Primitive Unmarshalling Functions

(defun unmarshal-u8 (context)
  "Unmarshal unsigned 8-bit integer."
  (declare (type unmarshal-context context))
  (let ((pos (unmarshal-context-position context))
        (buf (unmarshal-context-buffer context)))
    (prog1 (aref buf pos)
      (setf (unmarshal-context-position context) (1+ pos)))))

(defun unmarshal-u32 (context)
  "Unmarshal unsigned 32-bit integer (big-endian)."
  (declare (type unmarshal-context context))
  (logior (ash (unmarshal-u8 context) 24)
          (ash (unmarshal-u8 context) 16)
          (ash (unmarshal-u8 context) 8)
          (unmarshal-u8 context)))

(defun unmarshal-u64 (context)
  "Unmarshal unsigned 64-bit integer (big-endian)."
  (declare (type unmarshal-context context))
  (logior (ash (unmarshal-u32 context) 32)
          (unmarshal-u32 context)))

(defun unmarshal-i64 (context)
  "Unmarshal signed 64-bit integer."
  (declare (type unmarshal-context context))
  (let ((unsigned (unmarshal-u64 context)))
    (if (logbitp 63 unsigned)
        (- unsigned #x10000000000000000)
        unsigned)))

(defun unmarshal-f64 (context)
  "Unmarshal 64-bit float (IEEE 754)."
  (declare (type unmarshal-context context))
  (let ((bits (unmarshal-u64 context)))
    (decode-ieee754-double bits)))

(defun unmarshal-string (context)
  "Unmarshal string with length prefix."
  (declare (type unmarshal-context context))
  (let* ((length (unmarshal-u32 context))
         (bytes (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref bytes i) (unmarshal-u8 context)))
    (utf8-to-string bytes)))

(defun unmarshal-symbol (context)
  "Unmarshal symbol (package + name)."
  (declare (type unmarshal-context context))
  (let ((package-name (unmarshal-string context))
        (symbol-name (unmarshal-string context)))
    (if (string= package-name "")
        (make-symbol symbol-name)
        (intern symbol-name package-name))))

(defun unmarshal-list (context)
  "Unmarshal list with length prefix."
  (declare (type unmarshal-context context))
  (let ((length (unmarshal-u32 context))
        (result nil))
    (dotimes (i length)
      (push (unmarshal-value context) result))
    (nreverse result)))

(defun unmarshal-vector (context)
  "Unmarshal vector with length prefix."
  (declare (type unmarshal-context context))
  (let* ((length (unmarshal-u32 context))
         (result (make-array length)))
    (dotimes (i length)
      (setf (aref result i) (unmarshal-value context)))
    result))

(defun unmarshal-byte-vector (context)
  "Unmarshal byte vector efficiently."
  (declare (type unmarshal-context context))
  (let* ((length (unmarshal-u32 context))
         (result (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref result i) (unmarshal-u8 context)))
    result))

(defun unmarshal-hash-table (context)
  "Unmarshal hash table."
  (declare (type unmarshal-context context))
  (let ((count (unmarshal-u32 context))
        (result (make-hash-table :test 'equal)))
    (dotimes (i count)
      (let ((key (unmarshal-value context))
            (value (unmarshal-value context)))
        (setf (gethash key result) value)))
    result))

(defun unmarshal-rational (context)
  "Unmarshal rational number."
  (declare (type unmarshal-context context))
  (let ((numerator (unmarshal-i64 context))
        (denominator (unmarshal-i64 context)))
    (/ numerator denominator)))

(defun unmarshal-complex (context)
  "Unmarshal complex number."
  (declare (type unmarshal-context context))
  (let ((real (unmarshal-f64 context))
        (imag (unmarshal-f64 context)))
    (complex real imag)))

(defun unmarshal-pathname (context)
  "Unmarshal pathname."
  (declare (type unmarshal-context context))
  (let ((namestring (unmarshal-string context)))
    (parse-namestring namestring)))

(defun unmarshal-bignum (context)
  "Unmarshal arbitrary precision integer."
  (declare (type unmarshal-context context))
  (let ((negative (= (unmarshal-u8 context) 1))
        (length (unmarshal-u32 context))
        (result 0))
    (dotimes (i length)
      (let ((byte (unmarshal-u8 context)))
        (setf result (logior (ash result 8) byte))))
    (if negative (- result) result)))

(defun unmarshal-struct (context)
  "Unmarshal structure (limited support)."
  (declare (type unmarshal-context context))
  (let ((type-name (unmarshal-symbol context)))
    (error "Struct unmarshalling not fully implemented for type ~A" type-name)))

;;; RPC Call Implementation

(defun rpc-call (target-pid interface-id function-id args &key (timeout 5000))
  "Make synchronous RPC call to process."
  (declare (type (unsigned-byte 32) target-pid interface-id function-id)
           (type list args)
           (type (unsigned-byte 64) timeout))

  (let ((call (make-rpc-call
               :id (atomic-incf *rpc-call-id-counter*)
               :interface-id interface-id
               :function-id function-id
               :args args
               :sender-pid (get-current-pid)
               :receiver-pid target-pid
               :timeout timeout
               :start-time (get-tick-count)
               :async nil
               :semaphore (make-semaphore 0 1))))

    ;; Register pending call
    (with-mutex (*rpc-lock*)
      (setf (gethash (rpc-call-id call) *pending-calls*) call))

    ;; Marshal and send RPC message
    (let ((message-buffer (marshal-rpc-call call)))
      (when (null message-buffer)
        (with-mutex (*rpc-lock*)
          (remhash (rpc-call-id call) *pending-calls*))
        (return-from rpc-call (values nil +rpc-status-marshal-error+)))

      (let ((send-result (ipc-send-message target-pid +rpc-type-call+ message-buffer)))
        (when (< send-result 0)
          (with-mutex (*rpc-lock*)
            (remhash (rpc-call-id call) *pending-calls*))
          (return-from rpc-call (values nil +rpc-status-error+)))))

    ;; Wait for response
    (let ((wait-result (semaphore-wait (rpc-call-semaphore call) :timeout timeout)))
      (with-mutex (*rpc-lock*)
        (remhash (rpc-call-id call) *pending-calls*))

      (if wait-result
          (values (rpc-call-result call) (rpc-call-status call))
          (values nil +rpc-status-timeout+)))))

(defun rpc-call-async (target-pid interface-id function-id args callback &key (timeout 5000))
  "Make asynchronous RPC call to process."
  (declare (type (unsigned-byte 32) target-pid interface-id function-id)
           (type list args)
           (type function callback)
           (type (unsigned-byte 64) timeout))

  (let ((call (make-rpc-call
               :id (atomic-incf *rpc-call-id-counter*)
               :interface-id interface-id
               :function-id function-id
               :args args
               :sender-pid (get-current-pid)
               :receiver-pid target-pid
               :timeout timeout
               :start-time (get-tick-count)
               :async t
               :callback callback)))

    ;; Register pending call
    (with-mutex (*rpc-lock*)
      (setf (gethash (rpc-call-id call) *pending-calls*) call))

    ;; Marshal and send RPC message
    (let ((message-buffer (marshal-rpc-call call)))
      (when (null message-buffer)
        (with-mutex (*rpc-lock*)
          (remhash (rpc-call-id call) *pending-calls*))
        (return-from rpc-call-async nil))

      (ipc-send-message target-pid +rpc-type-call+ message-buffer))

    (rpc-call-id call)))

(defun rpc-wait (call-id &key (timeout nil))
  "Wait for asynchronous RPC call to complete."
  (declare (type (unsigned-byte 64) call-id))
  (let ((call (with-mutex (*rpc-lock*)
                (gethash call-id *pending-calls*))))
    (when (null call)
      (return-from rpc-wait (values nil +rpc-status-error+)))

    (when (not (rpc-call-async call))
      (return-from rpc-wait (values nil +rpc-status-error+)))

    ;; Wait for completion (polling for simplicity)
    (let ((deadline (when timeout (+ (get-tick-count) timeout))))
      (loop
        (let ((status (rpc-call-status call)))
          (when (not (= status +rpc-status-pending+))
            (return (values (rpc-call-result call) status))))

        (when (and deadline (>= (get-tick-count) deadline))
          (return (values nil +rpc-status-timeout+)))

        (thread-yield)))))

(defun rpc-cancel (call-id)
  "Cancel pending RPC call."
  (declare (type (unsigned-byte 64) call-id))
  (with-mutex (*rpc-lock*)
    (let ((call (gethash call-id *pending-calls*)))
      (when call
        (setf (rpc-call-status call) +rpc-status-cancelled+)
        (when (not (rpc-call-async call))
          (semaphore-post (rpc-call-semaphore call)))
        (remhash call-id *pending-calls*)
        t))))

(defun rpc-handle (message-buffer sender-pid)
  "Handle incoming RPC message."
  (declare (type (vector (unsigned-byte 8)) message-buffer)
           (type (unsigned-byte 32) sender-pid))

  (let ((context (make-unmarshal-context :buffer message-buffer :position 0)))
    (handler-case
        (let ((header (unmarshal-rpc-header context)))
          (when (not (= (rpc-header-magic header) +rpc-magic+))
            (return-from rpc-handle nil))

          (case (rpc-header-type header)
            (#.+rpc-type-call+
             (handle-rpc-call context sender-pid header))
            (#.+rpc-type-reply+
             (handle-rpc-reply context header))
            (#.+rpc-type-cancel+
             (handle-rpc-cancel context header))
            (t nil)))
      (error (e)
        (format t "RPC error: ~A~%" e)
        nil))))

(defun handle-rpc-call (context sender-pid header)
  "Handle RPC call request."
  (declare (type unmarshal-context context)
           (type (unsigned-byte 32) sender-pid)
           (type rpc-header header))

  (let* ((interface-id (unmarshal-u32 context))
         (function-id (unmarshal-u32 context))
         (args (unmarshal-value context))
         (interface (gethash interface-id *rpc-interfaces*))
         (call-id (rpc-header-call-id header)))

    (if (null interface)
        (send-rpc-error sender-pid call-id +rpc-status-no-handler+ "Interface not found")
        (let ((func (gethash function-id (rpc-interface-functions interface))))
          (if (null func)
              (send-rpc-error sender-pid call-id +rpc-status-no-handler+ "Function not found")
              (progn
                ;; Check capabilities if required
                (when (rpc-function-required-capabilities func)
                  (unless (check-rpc-capabilities sender-pid (rpc-function-required-capabilities func))
                    (send-rpc-error sender-pid call-id +rpc-status-error+ "Insufficient capabilities")
                    (return-from handle-rpc-call nil)))

                ;; Execute handler
                (handler-case
                    (let ((result (apply (rpc-function-handler func) args)))
                      (send-rpc-reply sender-pid call-id result))
                  (error (e)
                    (send-rpc-error sender-pid call-id +rpc-status-error+
                                   (format nil "Handler error: ~A" e))))))))))

(defun handle-rpc-reply (context header)
  "Handle RPC reply."
  (declare (type unmarshal-context context)
           (type rpc-header header))

  (let* ((call-id (rpc-header-call-id header))
         (status (unmarshal-u8 context))
         (result (unmarshal-value context))
         (call (with-mutex (*rpc-lock*)
                 (gethash call-id *pending-calls*))))

    (when call
      (setf (rpc-call-status call) status)
      (setf (rpc-call-result call) result)

      (if (rpc-call-async call)
          (progn
            (when (rpc-call-callback call)
              (funcall (rpc-call-callback call) result status))
            (with-mutex (*rpc-lock*)
              (remhash call-id *pending-calls*)))
          (semaphore-post (rpc-call-semaphore call))))))

(defun handle-rpc-cancel (context header)
  "Handle RPC cancellation."
  (declare (type unmarshal-context context)
           (type rpc-header header))
  (let ((call-id (rpc-header-call-id header)))
    (rpc-cancel call-id)))

;;; RPC Message Marshalling

(defun marshal-rpc-call (call)
  "Marshal RPC call to byte buffer."
  (declare (type rpc-call call))
  (let* ((buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (context (make-marshal-context :buffer buffer :position 0)))
    (handler-case
        (progn
          ;; Marshal header
          (let ((header (make-rpc-header
                         :type +rpc-type-call+
                         :call-id (rpc-call-id call)
                         :sequence 0)))
            (marshal-rpc-header header context))

          ;; Marshal call data
          (marshal-u32 (rpc-call-interface-id call) context)
          (marshal-u32 (rpc-call-function-id call) context)
          (marshal-value (rpc-call-args call) context)

          ;; Update payload size in header
          (let ((payload-size (- (marshal-context-position context) 24)))
            (setf (aref buffer 16) (logand (ash payload-size -24) #xFF))
            (setf (aref buffer 17) (logand (ash payload-size -16) #xFF))
            (setf (aref buffer 18) (logand (ash payload-size -8) #xFF))
            (setf (aref buffer 19) (logand payload-size #xFF)))

          (subseq buffer 0 (marshal-context-position context)))
      (error (e)
        (format t "Marshal error: ~A~%" e)
        nil))))

(defun marshal-rpc-header (header context)
  "Marshal RPC header."
  (declare (type rpc-header header)
           (type marshal-context context))
  (marshal-u32 (rpc-header-magic header) context)
  (marshal-u8 (ash (rpc-header-version header) -8) context)
  (marshal-u8 (logand (rpc-header-version header) #xFF) context)
  (marshal-u8 (rpc-header-type header) context)
  (marshal-u8 (rpc-header-flags header) context)
  (marshal-u64 (rpc-header-call-id header) context)
  (marshal-u32 (rpc-header-sequence header) context)
  (marshal-u32 (rpc-header-payload-size header) context)
  (marshal-u32 (rpc-header-checksum header) context))

(defun unmarshal-rpc-header (context)
  "Unmarshal RPC header."
  (declare (type unmarshal-context context))
  (make-rpc-header
   :magic (unmarshal-u32 context)
   :version (logior (ash (unmarshal-u8 context) 8) (unmarshal-u8 context))
   :type (unmarshal-u8 context)
   :flags (unmarshal-u8 context)
   :call-id (unmarshal-u64 context)
   :sequence (unmarshal-u32 context)
   :payload-size (unmarshal-u32 context)
   :checksum (unmarshal-u32 context)))

(defun send-rpc-reply (target-pid call-id result)
  "Send RPC reply."
  (declare (type (unsigned-byte 32) target-pid)
           (type (unsigned-byte 64) call-id))
  (let* ((buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (context (make-marshal-context :buffer buffer :position 0)))
    (handler-case
        (progn
          (let ((header (make-rpc-header
                         :type +rpc-type-reply+
                         :call-id call-id)))
            (marshal-rpc-header header context))
          (marshal-u8 +rpc-status-success+ context)
          (marshal-value result context)
          (ipc-send-message target-pid +rpc-type-reply+
                           (subseq buffer 0 (marshal-context-position context))))
      (error (e)
        (format t "Send reply error: ~A~%" e)
        nil))))

(defun send-rpc-error (target-pid call-id status message)
  "Send RPC error response."
  (declare (type (unsigned-byte 32) target-pid)
           (type (unsigned-byte 64) call-id)
           (type (unsigned-byte 8) status)
           (type string message))
  (let* ((buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (context (make-marshal-context :buffer buffer :position 0)))
    (let ((header (make-rpc-header
                   :type +rpc-type-error+
                   :call-id call-id)))
      (marshal-rpc-header header context))
    (marshal-u8 status context)
    (marshal-string message context)
    (ipc-send-message target-pid +rpc-type-error+
                     (subseq buffer 0 (marshal-context-position context)))))

;;; Helper Functions

(defun encode-ieee754-double (significand exponent sign)
  "Encode double float to IEEE 754 format."
  (declare (type double-float significand)
           (type integer exponent sign))
  ;; Simplified - production would handle denormals, infinity, NaN
  (let* ((exp-bias 1023)
         (biased-exp (+ exponent exp-bias))
         (frac (floor (* (abs significand) (expt 2 52)))))
    (logior (if (minusp sign) (ash 1 63) 0)
            (ash (logand biased-exp #x7FF) 52)
            (logand frac #xFFFFFFFFFFFFF))))

(defun decode-ieee754-double (bits)
  "Decode IEEE 754 format to double float."
  (declare (type (unsigned-byte 64) bits))
  (let* ((sign-bit (logbitp 63 bits))
         (exponent (logand (ash bits -52) #x7FF))
         (fraction (logand bits #xFFFFFFFFFFFFF)))
    (cond
      ((= exponent #x7FF)
       (if (= fraction 0)
           (if sign-bit -1.0d0 1.0d0)  ; Infinity
           0.0d0))  ; NaN simplified
      ((= exponent 0)
       ;; Denormalized
       (* (if sign-bit -1.0d0 1.0d0)
          (expt 2.0d0 -1022)
          (/ fraction (expt 2.0d0 52))))
      (t
       ;; Normalized
       (* (if sign-bit -1.0d0 1.0d0)
          (expt 2.0d0 (- exponent 1023))
          (+ 1.0d0 (/ fraction (expt 2.0d0 52))))))))

(defun string-to-utf8 (string)
  "Convert string to UTF-8 byte array."
  (declare (type string string))
  ;; Simplified UTF-8 encoding
  (let ((bytes (make-array (* (length string) 4) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop for char across string do
      (let ((code (char-code char)))
        (cond
          ((< code #x80)
           (vector-push code bytes))
          ((< code #x800)
           (vector-push (logior #xC0 (ash code -6)) bytes)
           (vector-push (logior #x80 (logand code #x3F)) bytes))
          ((< code #x10000)
           (vector-push (logior #xE0 (ash code -12)) bytes)
           (vector-push (logior #x80 (logand (ash code -6) #x3F)) bytes)
           (vector-push (logior #x80 (logand code #x3F)) bytes))
          (t
           (vector-push (logior #xF0 (ash code -18)) bytes)
           (vector-push (logior #x80 (logand (ash code -12) #x3F)) bytes)
           (vector-push (logior #x80 (logand (ash code -6) #x3F)) bytes)
           (vector-push (logior #x80 (logand code #x3F)) bytes)))))
    bytes))

(defun utf8-to-string (bytes)
  "Convert UTF-8 byte array to string."
  (declare (type (vector (unsigned-byte 8)) bytes))
  ;; Simplified UTF-8 decoding
  (let ((chars nil)
        (i 0)
        (len (length bytes)))
    (loop while (< i len) do
      (let ((b1 (aref bytes i)))
        (cond
          ((< b1 #x80)
           (push (code-char b1) chars)
           (incf i))
          ((< b1 #xE0)
           (when (< (+ i 1) len)
             (let ((b2 (aref bytes (+ i 1))))
               (push (code-char (logior (ash (logand b1 #x1F) 6)
                                       (logand b2 #x3F)))
                     chars)
               (incf i 2))))
          ((< b1 #xF0)
           (when (< (+ i 2) len)
             (let ((b2 (aref bytes (+ i 1)))
                   (b3 (aref bytes (+ i 2))))
               (push (code-char (logior (ash (logand b1 #x0F) 12)
                                       (ash (logand b2 #x3F) 6)
                                       (logand b3 #x3F)))
                     chars)
               (incf i 3))))
          (t
           (when (< (+ i 3) len)
             (incf i 4))))))
    (coerce (nreverse chars) 'string)))

(defun structurep (value)
  "Check if value is a structure."
  (and (not (null value))
       (not (consp value))
       (not (arrayp value))
       (not (hash-table-p value))
       (not (symbolp value))
       (not (numberp value))
       (not (stringp value))
       (not (characterp value))
       (not (functionp value))))

;;; Forward Declarations and Stubs

(defun make-mutex ()
  "Create mutex (forward declaration)."
  nil)

(defmacro with-mutex ((mutex) &body body)
  "Execute body with mutex held (forward declaration)."
  `(progn ,@body))

(defun make-semaphore (initial max)
  "Create semaphore (forward declaration)."
  (declare (ignore initial max))
  nil)

(defun semaphore-wait (sem &key timeout)
  "Wait on semaphore (forward declaration)."
  (declare (ignore sem timeout))
  t)

(defun semaphore-post (sem)
  "Post to semaphore (forward declaration)."
  (declare (ignore sem))
  t)

(defun atomic-incf (place)
  "Atomic increment (forward declaration)."
  (incf place))

(defun get-current-pid ()
  "Get current process ID (forward declaration)."
  1)

(defun get-tick-count ()
  "Get system tick count (forward declaration)."
  0)

(defun thread-yield ()
  "Yield CPU (forward declaration)."
  nil)

(defun ipc-send-message (pid type buffer)
  "Send IPC message (forward declaration)."
  (declare (ignore pid type buffer))
  0)

(defun check-rpc-capabilities (pid required-caps)
  "Check if process has required capabilities (forward declaration)."
  (declare (ignore pid required-caps))
  t)

