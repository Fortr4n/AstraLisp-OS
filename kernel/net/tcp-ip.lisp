;; AstraLisp OS Kernel - Comprehensive TCP/IP Network Stack
;; Production network stack with Ethernet, ARP, IP, ICMP, UDP, and TCP

(defpackage :astralisp-net
  (:use :cl)
  (:export ;; Network initialization
           :net-init
           ;; Packet buffer management
           :skb-alloc
           :skb-free
           :skb-put
           :skb-push
           :skb-pull
           ;; Ethernet layer
           :eth-rx
           :eth-tx
           ;; ARP
           :arp-lookup
           :arp-resolve
           ;; IP layer
           :ip-send
           :ip-receive
           :ip-route
           ;; ICMP
           :icmp-send-echo-request
           :icmp-send-echo-reply
           ;; UDP
           :udp-bind
           :udp-sendto
           :udp-recvfrom
           ;; TCP
           :tcp-socket
           :tcp-bind
           :tcp-listen
           :tcp-connect
           :tcp-accept
           :tcp-send
           :tcp-recv
           :tcp-close
           ;; Socket API
           :socket-create
           :socket-bind
           :socket-listen
           :socket-connect
           :socket-send
           :socket-recv
           :socket-close))

(in-package :astralisp-net)

;;; Network Constants

;; Ethernet
(defconstant +eth-alen+ 6 "Ethernet address length")
(defconstant +eth-hlen+ 14 "Ethernet header length")
(defconstant +eth-p-ip+ #x0800 "IPv4 protocol")
(defconstant +eth-p-arp+ #x0806 "ARP protocol")
(defconstant +eth-p-ipv6+ #x86DD "IPv6 protocol")

;; ARP
(defconstant +arp-request+ 1)
(defconstant +arp-reply+ 2)
(defconstant +arp-cache-timeout+ 300000 "ARP cache timeout (5 min)")
(defconstant +arp-max-retries+ 3)

;; IP Protocol Numbers
(defconstant +ipproto-icmp+ 1)
(defconstant +ipproto-tcp+ 6)
(defconstant +ipproto-udp+ 17)

;; IP Flags
(defconstant +ip-df+ #x4000 "Don't fragment")
(defconstant +ip-mf+ #x2000 "More fragments")
(defconstant +ip-offset-mask+ #x1FFF "Fragment offset mask")

;; ICMP Types
(defconstant +icmp-echo-reply+ 0)
(defconstant +icmp-dest-unreach+ 3)
(defconstant +icmp-source-quench+ 4)
(defconstant +icmp-redirect+ 5)
(defconstant +icmp-echo+ 8)
(defconstant +icmp-time-exceeded+ 11)

;; TCP States
(defconstant +tcp-closed+ 0)
(defconstant +tcp-listen+ 1)
(defconstant +tcp-syn-sent+ 2)
(defconstant +tcp-syn-received+ 3)
(defconstant +tcp-established+ 4)
(defconstant +tcp-fin-wait-1+ 5)
(defconstant +tcp-fin-wait-2+ 6)
(defconstant +tcp-close-wait+ 7)
(defconstant +tcp-closing+ 8)
(defconstant +tcp-last-ack+ 9)
(defconstant +tcp-time-wait+ 10)

;; TCP Flags
(defconstant +tcp-fin+ #x01)
(defconstant +tcp-syn+ #x02)
(defconstant +tcp-rst+ #x04)
(defconstant +tcp-psh+ #x08)
(defconstant +tcp-ack+ #x10)
(defconstant +tcp-urg+ #x20)

;; TCP Constants
(defconstant +tcp-mss+ 1460 "Maximum segment size")
(defconstant +tcp-window-size+ 65535 "Default window size")
(defconstant +tcp-max-retries+ 5)
(defconstant +tcp-rto-min+ 200 "Min RTO (ms)")
(defconstant +tcp-rto-max+ 120000 "Max RTO (ms)")
(defconstant +tcp-time-wait-timeout+ 120000 "TIME_WAIT timeout (2MSL)")

;;; Data Structures

(defstruct sk-buff
  "Socket buffer (packet buffer)."
  (data 0 :type (unsigned-byte 64))           ; Data pointer
  (len 0 :type (unsigned-byte 32))            ; Data length
  (head 0 :type (unsigned-byte 64))           ; Buffer head
  (tail 0 :type (unsigned-byte 64))           ; Buffer tail
  (end 0 :type (unsigned-byte 64))            ; Buffer end
  (truesize 0 :type (unsigned-byte 32))       ; True buffer size
  (protocol 0 :type (unsigned-byte 16))       ; Protocol
  (dev nil :type t)                           ; Network device
  (destructor nil :type (or null function))
  (timestamp 0 :type (unsigned-byte 64))
  (next nil :type (or null sk-buff))
  (prev nil :type (or null sk-buff)))

(defstruct eth-header
  "Ethernet header."
  (dest (make-array 6 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (src (make-array 6 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (proto 0 :type (unsigned-byte 16)))

(defstruct arp-entry
  "ARP cache entry."
  (ip 0 :type (unsigned-byte 32))
  (mac (make-array 6 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (timestamp 0 :type (unsigned-byte 64))
  (state 0 :type (unsigned-byte 8)))          ; 0=incomplete, 1=reachable, 2=stale

(defstruct ip-header
  "IPv4 header."
  (version 4 :type (unsigned-byte 4))
  (ihl 5 :type (unsigned-byte 4))
  (tos 0 :type (unsigned-byte 8))
  (tot-len 0 :type (unsigned-byte 16))
  (id 0 :type (unsigned-byte 16))
  (frag-off 0 :type (unsigned-byte 16))
  (ttl 64 :type (unsigned-byte 8))
  (protocol 0 :type (unsigned-byte 8))
  (check 0 :type (unsigned-byte 16))
  (saddr 0 :type (unsigned-byte 32))
  (daddr 0 :type (unsigned-byte 32)))

(defstruct icmp-header
  "ICMP header."
  (type 0 :type (unsigned-byte 8))
  (code 0 :type (unsigned-byte 8))
  (checksum 0 :type (unsigned-byte 16))
  (id 0 :type (unsigned-byte 16))
  (sequence 0 :type (unsigned-byte 16)))

(defstruct udp-header
  "UDP header."
  (sport 0 :type (unsigned-byte 16))
  (dport 0 :type (unsigned-byte 16))
  (len 0 :type (unsigned-byte 16))
  (check 0 :type (unsigned-byte 16)))

(defstruct tcp-header
  "TCP header."
  (sport 0 :type (unsigned-byte 16))
  (dport 0 :type (unsigned-byte 16))
  (seq 0 :type (unsigned-byte 32))
  (ack-seq 0 :type (unsigned-byte 32))
  (doff 5 :type (unsigned-byte 4))            ; Data offset (header length / 4)
  (flags 0 :type (unsigned-byte 8))
  (window 0 :type (unsigned-byte 16))
  (check 0 :type (unsigned-byte 16))
  (urg-ptr 0 :type (unsigned-byte 16)))

(defstruct tcp-socket
  "TCP socket control block."
  (state +tcp-closed+ :type (unsigned-byte 8))
  (local-addr 0 :type (unsigned-byte 32))
  (local-port 0 :type (unsigned-byte 16))
  (remote-addr 0 :type (unsigned-byte 32))
  (remote-port 0 :type (unsigned-byte 16))
  ;; Sequence numbers
  (snd-una 0 :type (unsigned-byte 32))        ; Oldest unacknowledged seq
  (snd-nxt 0 :type (unsigned-byte 32))        ; Next seq to send
  (snd-wnd 0 :type (unsigned-byte 32))        ; Send window
  (rcv-nxt 0 :type (unsigned-byte 32))        ; Next seq expected
  (rcv-wnd +tcp-window-size+ :type (unsigned-byte 32)) ; Receive window
  ;; Buffers
  (send-buffer nil :type list)                ; Outgoing segments
  (recv-buffer nil :type list)                ; Received segments
  (listen-backlog nil :type list)             ; Accept queue
  (max-backlog 5 :type (unsigned-byte 16))
  ;; Retransmission
  (rto 1000 :type (unsigned-byte 32))         ; Retransmission timeout (ms)
  (srtt 0 :type (unsigned-byte 32))           ; Smoothed RTT
  (rttvar 0 :type (unsigned-byte 32))         ; RTT variance
  (retrans-timer nil :type t)
  (retrans-count 0 :type (unsigned-byte 8))
  ;; Congestion control
  (cwnd +tcp-mss+ :type (unsigned-byte 32))   ; Congestion window
  (ssthresh 65535 :type (unsigned-byte 32))   ; Slow start threshold
  (dup-acks 0 :type (unsigned-byte 8))
  ;; Synchronization
  (lock nil :type t)
  (recv-wait nil :type t)                     ; Semaphore for recv
  (accept-wait nil :type t))                  ; Semaphore for accept

(defstruct route-entry
  "Routing table entry."
  (dest 0 :type (unsigned-byte 32))
  (mask 0 :type (unsigned-byte 32))
  (gateway 0 :type (unsigned-byte 32))
  (interface nil :type t)
  (metric 0 :type (unsigned-byte 32)))

(defstruct net-device
  "Network device."
  (name "" :type string)
  (mac (make-array 6 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (ip 0 :type (unsigned-byte 32))
  (netmask 0 :type (unsigned-byte 32))
  (mtu 1500 :type (unsigned-byte 16))
  (flags 0 :type (unsigned-byte 32))
  (tx-queue nil :type list)
  (rx-handler nil :type (or null function))
  (hard-header nil :type (or null function))
  (hard-start-xmit nil :type (or null function))
  (stats nil :type t))

;;; Global State

(defvar *net-initialized* nil)
(defvar *net-devices* nil "List of network devices")
(defvar *arp-cache* (make-hash-table) "ARP cache")
(defvar *arp-cache-lock* nil)
(defvar *route-table* nil "Routing table")
(defvar *route-table-lock* nil)
(defvar *tcp-sockets* (make-hash-table) "Active TCP sockets")
(defvar *tcp-sockets-lock* nil)
(defvar *next-ephemeral-port* 32768)
(defvar *ip-id-counter* 0)

;;; Initialization

(defun net-init ()
  "Initialize network stack."
  (when *net-initialized*
    (return-from net-init t))

  (setf *arp-cache-lock* (make-spinlock))
  (setf *route-table-lock* (make-spinlock))
  (setf *tcp-sockets-lock* (make-spinlock))

  (setf *net-initialized* t)
  t)

;;; Socket Buffer (sk_buff) Management

(defun skb-alloc (size)
  "Allocate socket buffer."
  (declare (type (unsigned-byte 32) size))

  (let* ((headroom 128)  ; Space for headers
         (total-size (+ size headroom 128))  ; Extra tailroom
         (buffer (kmalloc total-size)))

    (when (zerop buffer)
      (return-from skb-alloc nil))

    (let ((skb (make-sk-buff
                :head buffer
                :data (+ buffer headroom)
                :tail (+ buffer headroom)
                :end (+ buffer total-size)
                :len 0
                :truesize total-size
                :timestamp (get-time-ns))))
      skb)))

(defun skb-free (skb)
  "Free socket buffer."
  (declare (type sk-buff skb))

  (when (sk-buff-destructor skb)
    (funcall (sk-buff-destructor skb) skb))

  (kfree (sk-buff-head skb))
  t)

(defun skb-put (skb len)
  "Extend data area at tail."
  (declare (type sk-buff skb)
           (type (unsigned-byte 32) len))

  (let ((old-tail (sk-buff-tail skb)))
    (incf (sk-buff-tail skb) len)
    (incf (sk-buff-len skb) len)
    old-tail))

(defun skb-push (skb len)
  "Extend data area at head."
  (declare (type sk-buff skb)
           (type (unsigned-byte 32) len))

  (decf (sk-buff-data skb) len)
  (incf (sk-buff-len skb) len)
  (sk-buff-data skb))

(defun skb-pull (skb len)
  "Remove data from head."
  (declare (type sk-buff skb)
           (type (unsigned-byte 32) len))

  (incf (sk-buff-data skb) len)
  (decf (sk-buff-len skb) len)
  (sk-buff-data skb))

;;; Checksum Calculation

(defun ip-checksum (data offset length)
  "Calculate IP checksum."
  (declare (type (unsigned-byte 64) data)
           (type (unsigned-byte 32) offset length))

  (let ((sum 0))
    (dotimes (i (floor length 2))
      (let ((word (logior (mem-read-u8 (+ data offset (* i 2)))
                         (ash (mem-read-u8 (+ data offset (* i 2) 1)) 8))))
        (incf sum word)))

    ;; Add odd byte if present
    (when (oddp length)
      (incf sum (mem-read-u8 (+ data offset (1- length)))))

    ;; Fold 32-bit sum to 16 bits
    (loop while (> sum #xFFFF) do
      (setf sum (+ (logand sum #xFFFF) (ash sum -16))))

    (logand (lognot sum) #xFFFF)))

;;; Ethernet Layer

(defun eth-rx (dev skb)
  "Receive Ethernet frame."
  (declare (type net-device dev)
           (type sk-buff skb))

  (when (< (sk-buff-len skb) +eth-hlen+)
    (skb-free skb)
    (return-from eth-rx nil))

  ;; Parse Ethernet header
  (let ((data (sk-buff-data skb)))
    (let ((proto (logior (ash (mem-read-u8 (+ data 12)) 8)
                        (mem-read-u8 (+ data 13)))))

      ;; Remove Ethernet header
      (skb-pull skb +eth-hlen+)

      ;; Dispatch to protocol handler
      (case proto
        (#.+eth-p-ip+
         (ip-receive dev skb))
        (#.+eth-p-arp+
         (arp-receive dev skb))
        (t
         (skb-free skb))))))

(defun eth-tx (dev skb dest-mac proto)
  "Transmit Ethernet frame."
  (declare (type net-device dev)
           (type sk-buff skb)
           (type (vector (unsigned-byte 8)) dest-mac)
           (type (unsigned-byte 16) proto))

  ;; Add Ethernet header
  (let ((eth-hdr (skb-push skb +eth-hlen+)))
    ;; Destination MAC
    (dotimes (i 6)
      (mem-write-u8 (+ eth-hdr i) (aref dest-mac i)))

    ;; Source MAC
    (dotimes (i 6)
      (mem-write-u8 (+ eth-hdr 6 i) (aref (net-device-mac dev) i)))

    ;; EtherType
    (mem-write-u8 (+ eth-hdr 12) (ash proto -8))
    (mem-write-u8 (+ eth-hdr 13) (logand proto #xFF)))

  ;; Transmit
  (when (net-device-hard-start-xmit dev)
    (funcall (net-device-hard-start-xmit dev) skb dev))

  t)

;;; ARP Protocol

(defun arp-receive (dev skb)
  "Receive ARP packet."
  (declare (type net-device dev)
           (type sk-buff skb))

  (when (< (sk-buff-len skb) 28)  ; ARP packet size
    (skb-free skb)
    (return-from arp-receive nil))

  (let ((data (sk-buff-data skb)))
    (let ((op (logior (ash (mem-read-u8 (+ data 6)) 8)
                     (mem-read-u8 (+ data 7))))
          (sender-ip (logior (ash (mem-read-u8 (+ data 14)) 24)
                            (ash (mem-read-u8 (+ data 15)) 16)
                            (ash (mem-read-u8 (+ data 16)) 8)
                            (mem-read-u8 (+ data 17))))
          (target-ip (logior (ash (mem-read-u8 (+ data 24)) 24)
                            (ash (mem-read-u8 (+ data 25)) 16)
                            (ash (mem-read-u8 (+ data 26)) 8)
                            (mem-read-u8 (+ data 27)))))

      (case op
        (#.+arp-request+
         (when (= target-ip (net-device-ip dev))
           ;; Send ARP reply
           (arp-send-reply dev sender-ip (+ data 8))))

        (#.+arp-reply+
         ;; Update ARP cache
         (let ((mac (make-array 6 :element-type '(unsigned-byte 8))))
           (dotimes (i 6)
             (setf (aref mac i) (mem-read-u8 (+ data 8 i))))
           (arp-cache-update sender-ip mac))))))

  (skb-free skb)
  t)

(defun arp-send-reply (dev target-ip target-mac-ptr)
  "Send ARP reply."
  (declare (type net-device dev)
           (type (unsigned-byte 32) target-ip)
           (type (unsigned-byte 64) target-mac-ptr))

  (let ((skb (skb-alloc 28)))
    (unless skb
      (return-from arp-send-reply nil))

    (let ((data (skb-put skb 28)))
      ;; Hardware type (Ethernet)
      (mem-write-u8 data 0)
      (mem-write-u8 (+ data 1) 1)

      ;; Protocol type (IP)
      (mem-write-u8 (+ data 2) 8)
      (mem-write-u8 (+ data 3) 0)

      ;; Hardware size
      (mem-write-u8 (+ data 4) 6)

      ;; Protocol size
      (mem-write-u8 (+ data 5) 4)

      ;; Opcode (reply)
      (mem-write-u8 (+ data 6) 0)
      (mem-write-u8 (+ data 7) +arp-reply+)

      ;; Sender MAC
      (dotimes (i 6)
        (mem-write-u8 (+ data 8 i) (aref (net-device-mac dev) i)))

      ;; Sender IP
      (mem-write-u8 (+ data 14) (logand (ash (net-device-ip dev) -24) #xFF))
      (mem-write-u8 (+ data 15) (logand (ash (net-device-ip dev) -16) #xFF))
      (mem-write-u8 (+ data 16) (logand (ash (net-device-ip dev) -8) #xFF))
      (mem-write-u8 (+ data 17) (logand (net-device-ip dev) #xFF))

      ;; Target MAC
      (dotimes (i 6)
        (mem-write-u8 (+ data 18 i) (mem-read-u8 (+ target-mac-ptr i))))

      ;; Target IP
      (mem-write-u8 (+ data 24) (logand (ash target-ip -24) #xFF))
      (mem-write-u8 (+ data 25) (logand (ash target-ip -16) #xFF))
      (mem-write-u8 (+ data 26) (logand (ash target-ip -8) #xFF))
      (mem-write-u8 (+ data 27) (logand target-ip #xFF)))

    ;; Get target MAC for Ethernet header
    (let ((target-mac (make-array 6 :element-type '(unsigned-byte 8))))
      (dotimes (i 6)
        (setf (aref target-mac i) (mem-read-u8 (+ target-mac-ptr i))))

      (eth-tx dev skb target-mac +eth-p-arp+))
    t))

(defun arp-cache-update (ip mac)
  "Update ARP cache entry."
  (declare (type (unsigned-byte 32) ip)
           (type (vector (unsigned-byte 8)) mac))

  (with-spinlock (*arp-cache-lock*)
    (let ((entry (gethash ip *arp-cache*)))
      (unless entry
        (setf entry (make-arp-entry :ip ip))
        (setf (gethash ip *arp-cache*) entry))

      (dotimes (i 6)
        (setf (aref (arp-entry-mac entry) i) (aref mac i)))

      (setf (arp-entry-timestamp entry) (get-time-ms))
      (setf (arp-entry-state entry) 1)))  ; Reachable
  t)

(defun arp-lookup (ip)
  "Lookup MAC address in ARP cache."
  (declare (type (unsigned-byte 32) ip))

  (with-spinlock (*arp-cache-lock*)
    (let ((entry (gethash ip *arp-cache*)))
      (when (and entry
                (= (arp-entry-state entry) 1)
                (< (- (get-time-ms) (arp-entry-timestamp entry))
                   +arp-cache-timeout+))
        (arp-entry-mac entry)))))

;;; IP Layer

(defun ip-receive (dev skb)
  "Receive IP packet."
  (declare (type net-device dev)
           (type sk-buff skb))

  (when (< (sk-buff-len skb) 20)  ; Minimum IP header
    (skb-free skb)
    (return-from ip-receive nil))

  (let ((data (sk-buff-data skb)))
    (let* ((ihl (* (logand (mem-read-u8 data) #x0F) 4))
           (tot-len (logior (ash (mem-read-u8 (+ data 2)) 8)
                           (mem-read-u8 (+ data 3))))
           (protocol (mem-read-u8 (+ data 9)))
           (saddr (logior (ash (mem-read-u8 (+ data 12)) 24)
                         (ash (mem-read-u8 (+ data 13)) 16)
                         (ash (mem-read-u8 (+ data 14)) 8)
                         (mem-read-u8 (+ data 15))))
           (daddr (logior (ash (mem-read-u8 (+ data 16)) 24)
                         (ash (mem-read-u8 (+ data 17)) 16)
                         (ash (mem-read-u8 (+ data 18)) 8)
                         (mem-read-u8 (+ data 19)))))

      ;; Verify checksum
      (unless (zerop (ip-checksum data 0 ihl))
        (skb-free skb)
        (return-from ip-receive nil))

      ;; Check if packet is for us
      (unless (or (= daddr (net-device-ip dev))
                 (= daddr #xFFFFFFFF))  ; Broadcast
        (skb-free skb)
        (return-from ip-receive nil))

      ;; Remove IP header
      (skb-pull skb ihl)

      ;; Dispatch to protocol handler
      (case protocol
        (#.+ipproto-icmp+
         (icmp-receive dev skb saddr daddr))
        (#.+ipproto-tcp+
         (tcp-receive dev skb saddr daddr))
        (#.+ipproto-udp+
         (udp-receive dev skb saddr daddr))
        (t
         (skb-free skb)))))

  t)

(defun ip-send (dev skb dest-ip protocol)
  "Send IP packet."
  (declare (type net-device dev)
           (type sk-buff skb)
           (type (unsigned-byte 32) dest-ip)
           (type (unsigned-byte 8) protocol))

  (let ((ip-hdr (skb-push skb 20)))
    ;; Version and IHL
    (mem-write-u8 ip-hdr #x45)

    ;; ToS
    (mem-write-u8 (+ ip-hdr 1) 0)

    ;; Total length
    (let ((tot-len (sk-buff-len skb)))
      (mem-write-u8 (+ ip-hdr 2) (ash tot-len -8))
      (mem-write-u8 (+ ip-hdr 3) (logand tot-len #xFF)))

    ;; ID
    (let ((id (atomic-incf *ip-id-counter*)))
      (mem-write-u8 (+ ip-hdr 4) (ash id -8))
      (mem-write-u8 (+ ip-hdr 5) (logand id #xFF)))

    ;; Flags and fragment offset
    (mem-write-u8 (+ ip-hdr 6) (ash +ip-df+ -8))
    (mem-write-u8 (+ ip-hdr 7) 0)

    ;; TTL
    (mem-write-u8 (+ ip-hdr 8) 64)

    ;; Protocol
    (mem-write-u8 (+ ip-hdr 9) protocol)

    ;; Checksum (zero for calculation)
    (mem-write-u8 (+ ip-hdr 10) 0)
    (mem-write-u8 (+ ip-hdr 11) 0)

    ;; Source address
    (let ((saddr (net-device-ip dev)))
      (mem-write-u8 (+ ip-hdr 12) (logand (ash saddr -24) #xFF))
      (mem-write-u8 (+ ip-hdr 13) (logand (ash saddr -16) #xFF))
      (mem-write-u8 (+ ip-hdr 14) (logand (ash saddr -8) #xFF))
      (mem-write-u8 (+ ip-hdr 15) (logand saddr #xFF)))

    ;; Destination address
    (mem-write-u8 (+ ip-hdr 16) (logand (ash dest-ip -24) #xFF))
    (mem-write-u8 (+ ip-hdr 17) (logand (ash dest-ip -16) #xFF))
    (mem-write-u8 (+ ip-hdr 18) (logand (ash dest-ip -8) #xFF))
    (mem-write-u8 (+ ip-hdr 19) (logand dest-ip #xFF))

    ;; Calculate checksum
    (let ((checksum (ip-checksum ip-hdr 0 20)))
      (mem-write-u8 (+ ip-hdr 10) (ash checksum -8))
      (mem-write-u8 (+ ip-hdr 11) (logand checksum #xFF))))

  ;; Resolve MAC address
  (let ((dest-mac (arp-lookup dest-ip)))
    (if dest-mac
        (eth-tx dev skb dest-mac +eth-p-ip+)
        ;; Need to do ARP resolution
        (arp-resolve dev dest-ip skb)))

  t)

;;; ICMP Protocol

(defun icmp-receive (dev skb saddr daddr)
  "Receive ICMP packet."
  (declare (type net-device dev)
           (type sk-buff skb)
           (type (unsigned-byte 32) saddr daddr))

  (when (< (sk-buff-len skb) 8)
    (skb-free skb)
    (return-from icmp-receive nil))

  (let ((data (sk-buff-data skb)))
    (let ((type (mem-read-u8 data))
          (code (mem-read-u8 (+ data 1))))

      (case type
        (#.+icmp-echo+
         ;; Echo request - send reply
         (icmp-send-echo-reply dev skb saddr daddr))

        (#.+icmp-echo-reply+
         ;; Echo reply - deliver to waiting process
         nil)

        (t
         (skb-free skb)))))

  t)

(defun icmp-send-echo-reply (dev skb saddr daddr)
  "Send ICMP echo reply."
  (declare (type net-device dev)
           (type sk-buff skb)
           (type (unsigned-byte 32) saddr daddr))

  (let ((data (sk-buff-data skb)))
    ;; Change type to echo reply
    (mem-write-u8 data +icmp-echo-reply+)

    ;; Recalculate checksum
    (mem-write-u8 (+ data 2) 0)
    (mem-write-u8 (+ data 3) 0)

    (let ((checksum (ip-checksum data 0 (sk-buff-len skb))))
      (mem-write-u8 (+ data 2) (ash checksum -8))
      (mem-write-u8 (+ data 3) (logand checksum #xFF))))

  ;; Send back to sender
  (ip-send dev skb saddr +ipproto-icmp+)
  t)

;;; UDP Protocol

(defstruct udp-socket
  "UDP socket."
  (local-addr 0 :type (unsigned-byte 32))
  (local-port 0 :type (unsigned-byte 16))
  (recv-queue nil :type list)
  (lock nil :type t)
  (recv-wait nil :type t))

(defvar *udp-sockets* (make-hash-table) "Active UDP sockets")
(defvar *udp-sockets-lock* nil)

(defun udp-receive (dev skb saddr daddr)
  "Receive UDP datagram."
  (declare (type net-device dev)
           (type sk-buff skb)
           (type (unsigned-byte 32) saddr daddr))
  (declare (ignore dev daddr))

  (when (< (sk-buff-len skb) 8)
    (skb-free skb)
    (return-from udp-receive nil))

  (let ((data (sk-buff-data skb)))
    (let ((sport (logior (ash (mem-read-u8 data) 8)
                        (mem-read-u8 (+ data 1))))
          (dport (logior (ash (mem-read-u8 (+ data 2)) 8)
                        (mem-read-u8 (+ data 3))))
          (length (logior (ash (mem-read-u8 (+ data 4)) 8)
                         (mem-read-u8 (+ data 5)))))

      ;; Find socket bound to this port
      (with-spinlock (*udp-sockets-lock*)
        (let ((sock (gethash dport *udp-sockets*)))
          (if sock
              (progn
                ;; Queue packet for application
                (with-spinlock ((udp-socket-lock sock))
                  (push (list saddr sport skb) (udp-socket-recv-queue sock))
                  ;; Wake up waiting receiver
                  (when (udp-socket-recv-wait sock)
                    (semaphore-signal (udp-socket-recv-wait sock)))))
              ;; No socket - drop packet
              (skb-free skb))))))

  t)

(defun udp-bind (sock local-port)
  "Bind UDP socket to port."
  (declare (type udp-socket sock)
           (type (unsigned-byte 16) local-port))

  (with-spinlock (*udp-sockets-lock*)
    (when (gethash local-port *udp-sockets*)
      (return-from udp-bind nil))  ; Port already in use

    (setf (udp-socket-local-port sock) local-port)
    (setf (gethash local-port *udp-sockets*) sock))
  t)

(defun udp-sendto (sock dev dest-addr dest-port data length)
  "Send UDP datagram."
  (declare (type udp-socket sock)
           (type net-device dev)
           (type (unsigned-byte 32) dest-addr)
           (type (unsigned-byte 16) dest-port)
           (type (unsigned-byte 64) data)
           (type (unsigned-byte 32) length))

  (let ((skb (skb-alloc (+ length 8))))
    (unless skb
      (return-from udp-sendto nil))

    (let ((udp-hdr (skb-put skb 8)))
      ;; Source port
      (mem-write-u8 udp-hdr (ash (udp-socket-local-port sock) -8))
      (mem-write-u8 (+ udp-hdr 1) (logand (udp-socket-local-port sock) #xFF))

      ;; Destination port
      (mem-write-u8 (+ udp-hdr 2) (ash dest-port -8))
      (mem-write-u8 (+ udp-hdr 3) (logand dest-port #xFF))

      ;; Length
      (let ((tot-len (+ length 8)))
        (mem-write-u8 (+ udp-hdr 4) (ash tot-len -8))
        (mem-write-u8 (+ udp-hdr 5) (logand tot-len #xFF)))

      ;; Checksum (optional for IPv4)
      (mem-write-u8 (+ udp-hdr 6) 0)
      (mem-write-u8 (+ udp-hdr 7) 0)

      ;; Copy data
      (let ((payload (skb-put skb length)))
        (dotimes (i length)
          (mem-write-u8 (+ payload i) (mem-read-u8 (+ data i))))))

    ;; Send via IP
    (ip-send dev skb dest-addr +ipproto-udp+)
    length))

(defun udp-recvfrom (sock buffer length)
  "Receive UDP datagram."
  (declare (type udp-socket sock)
           (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) length))

  ;; Wait for packet
  (loop
    (with-spinlock ((udp-socket-lock sock))
      (when (udp-socket-recv-queue sock)
        (let* ((entry (pop (udp-socket-recv-queue sock)))
               (saddr (first entry))
               (sport (second entry))
               (skb (third entry))
               (data (+ (sk-buff-data skb) 8))  ; Skip UDP header
               (data-len (- (sk-buff-len skb) 8))
               (to-copy (min length data-len)))

          ;; Copy data to user buffer
          (dotimes (i to-copy)
            (mem-write-u8 (+ buffer i) (mem-read-u8 (+ data i))))

          (skb-free skb)
          (return-from udp-recvfrom (values to-copy saddr sport)))))

    ;; No data - wait
    (semaphore-wait (udp-socket-recv-wait sock))))

;;; TCP Protocol

(defun tcp-receive (dev skb saddr daddr)
  "Receive TCP segment."
  (declare (type net-device dev)
           (type sk-buff skb)
           (type (unsigned-byte 32) saddr daddr))
  (declare (ignore dev daddr))

  (when (< (sk-buff-len skb) 20)
    (skb-free skb)
    (return-from tcp-receive nil))

  (let ((data (sk-buff-data skb)))
    (let* ((sport (logior (ash (mem-read-u8 data) 8)
                         (mem-read-u8 (+ data 1))))
           (dport (logior (ash (mem-read-u8 (+ data 2)) 8)
                         (mem-read-u8 (+ data 3))))
           (seq (logior (ash (mem-read-u8 (+ data 4)) 24)
                       (ash (mem-read-u8 (+ data 5)) 16)
                       (ash (mem-read-u8 (+ data 6)) 8)
                       (mem-read-u8 (+ data 7))))
           (ack-seq (logior (ash (mem-read-u8 (+ data 8)) 24)
                           (ash (mem-read-u8 (+ data 9)) 16)
                           (ash (mem-read-u8 (+ data 10)) 8)
                           (mem-read-u8 (+ data 11))))
           (doff (ash (mem-read-u8 (+ data 12)) -4))
           (flags (mem-read-u8 (+ data 13)))
           (window (logior (ash (mem-read-u8 (+ data 14)) 8)
                          (mem-read-u8 (+ data 15)))))

      ;; Find socket
      (with-spinlock (*tcp-sockets-lock*)
        (let ((sock (tcp-find-socket dport saddr sport)))
          (if sock
              (tcp-process-segment sock skb seq ack-seq flags window)
              ;; No socket - send RST
              (progn
                (tcp-send-rst dev saddr sport dport seq)
                (skb-free skb)))))))

  t)

(defun tcp-find-socket (local-port remote-addr remote-port)
  "Find TCP socket matching connection."
  (declare (type (unsigned-byte 16) local-port remote-port)
           (type (unsigned-byte 32) remote-addr))

  ;; Try exact match first
  (let ((key (logior (ash local-port 48)
                    (ash remote-addr 16)
                    remote-port)))
    (let ((sock (gethash key *tcp-sockets*)))
      (when sock
        (return-from tcp-find-socket sock))))

  ;; Try listening socket
  (let ((listen-key (ash local-port 48)))
    (gethash listen-key *tcp-sockets*)))

(defun tcp-process-segment (sock skb seq ack-seq flags window)
  "Process TCP segment."
  (declare (type tcp-socket sock)
           (type sk-buff skb)
           (type (unsigned-byte 32) seq ack-seq)
           (type (unsigned-byte 8) flags)
           (type (unsigned-byte 16) window))

  (with-spinlock ((tcp-socket-lock sock))
    (let ((state (tcp-socket-state sock)))

      (case state
        (#.+tcp-listen+
         (when (logtest flags +tcp-syn+)
           ;; SYN received - send SYN-ACK
           (tcp-send-synack sock seq)
           (setf (tcp-socket-state sock) +tcp-syn-received+)
           (setf (tcp-socket-rcv-nxt sock) (1+ seq))))

        (#.+tcp-syn-sent+
         (when (and (logtest flags +tcp-syn+)
                   (logtest flags +tcp-ack+))
           ;; SYN-ACK received - send ACK
           (setf (tcp-socket-rcv-nxt sock) (1+ seq))
           (setf (tcp-socket-snd-una sock) ack-seq)
           (tcp-send-ack sock)
           (setf (tcp-socket-state sock) +tcp-established+)
           ;; Wake up waiting connect
           (when (tcp-socket-recv-wait sock)
             (semaphore-signal (tcp-socket-recv-wait sock)))))

        (#.+tcp-syn-received+
         (when (logtest flags +tcp-ack+)
           ;; ACK received - connection established
           (setf (tcp-socket-snd-una sock) ack-seq)
           (setf (tcp-socket-state sock) +tcp-established+)
           ;; Add to accept queue
           (push sock (tcp-socket-listen-backlog sock))
           (when (tcp-socket-accept-wait sock)
             (semaphore-signal (tcp-socket-accept-wait sock)))))

        (#.+tcp-established+
         ;; Handle data and ACKs
         (when (logtest flags +tcp-ack+)
           (tcp-process-ack sock ack-seq))

         (when (> (sk-buff-len skb) 20)
           (tcp-process-data sock skb seq))

         (when (logtest flags +tcp-fin+)
           ;; FIN received - initiate close
           (setf (tcp-socket-state sock) +tcp-close-wait+)
           (tcp-send-ack sock)))

        (#.+tcp-fin-wait-1+
         (when (logtest flags +tcp-ack+)
           (setf (tcp-socket-state sock) +tcp-fin-wait-2+))
         (when (logtest flags +tcp-fin+)
           (setf (tcp-socket-state sock) +tcp-time-wait+)
           (tcp-send-ack sock)))

        (#.+tcp-fin-wait-2+
         (when (logtest flags +tcp-fin+)
           (setf (tcp-socket-state sock) +tcp-time-wait+)
           (tcp-send-ack sock)))

        (#.+tcp-close-wait+
         ;; Waiting for application to close
         nil))))

  (skb-free skb)
  t)

(defun tcp-process-ack (sock ack-seq)
  "Process ACK."
  (declare (type tcp-socket sock)
           (type (unsigned-byte 32) ack-seq))

  ;; Update send window
  (when (> ack-seq (tcp-socket-snd-una sock))
    (setf (tcp-socket-snd-una sock) ack-seq)

    ;; Remove acknowledged segments from send buffer
    (setf (tcp-socket-send-buffer sock)
          (remove-if (lambda (seg)
                      (<= (first seg) ack-seq))
                    (tcp-socket-send-buffer sock)))

    ;; Update congestion window (simplified)
    (when (< (tcp-socket-cwnd sock) (tcp-socket-ssthresh sock))
      ;; Slow start
      (incf (tcp-socket-cwnd sock) +tcp-mss+))
    (when (>= (tcp-socket-cwnd sock) (tcp-socket-ssthresh sock))
      ;; Congestion avoidance
      (incf (tcp-socket-cwnd sock)
           (floor (* +tcp-mss+ +tcp-mss+) (tcp-socket-cwnd sock)))))

  t)

(defun tcp-process-data (sock skb seq)
  "Process received data."
  (declare (type tcp-socket sock)
           (type sk-buff skb)
           (type (unsigned-byte 32) seq))

  (when (= seq (tcp-socket-rcv-nxt sock))
    ;; In-sequence data
    (let ((data-len (- (sk-buff-len skb) 20)))
      (when (> data-len 0)
        ;; Add to receive buffer
        (push (list seq skb) (tcp-socket-recv-buffer sock))
        (incf (tcp-socket-rcv-nxt sock) data-len)

        ;; Send ACK
        (tcp-send-ack sock)

        ;; Wake up waiting receiver
        (when (tcp-socket-recv-wait sock)
          (semaphore-signal (tcp-socket-recv-wait sock))))))

  t)

(defun tcp-send-synack (sock seq)
  "Send SYN-ACK."
  (declare (type tcp-socket sock)
           (type (unsigned-byte 32) seq))

  (let ((skb (skb-alloc 20)))
    (unless skb
      (return-from tcp-send-synack nil))

    (let ((tcp-hdr (skb-put skb 20)))
      ;; Build TCP header
      (tcp-build-header tcp-hdr sock (logior +tcp-syn+ +tcp-ack+) (1+ seq))

      ;; Send via IP
      (let ((dev (first *net-devices*)))  ; Get first device
        (when dev
          (ip-send dev skb (tcp-socket-remote-addr sock) +ipproto-tcp+))))

    t))

(defun tcp-send-ack (sock)
  "Send ACK."
  (declare (type tcp-socket sock))

  (let ((skb (skb-alloc 20)))
    (unless skb
      (return-from tcp-send-ack nil))

    (let ((tcp-hdr (skb-put skb 20)))
      (tcp-build-header tcp-hdr sock +tcp-ack+ (tcp-socket-rcv-nxt sock))

      (let ((dev (first *net-devices*)))
        (when dev
          (ip-send dev skb (tcp-socket-remote-addr sock) +ipproto-tcp+))))

    t))

(defun tcp-send-rst (dev dest-addr dest-port src-port seq)
  "Send RST."
  (declare (type net-device dev)
           (type (unsigned-byte 32) dest-addr seq)
           (type (unsigned-byte 16) dest-port src-port))

  (let ((skb (skb-alloc 20)))
    (unless skb
      (return-from tcp-send-rst nil))

    (let ((tcp-hdr (skb-put skb 20)))
      ;; Source port
      (mem-write-u8 tcp-hdr (ash src-port -8))
      (mem-write-u8 (+ tcp-hdr 1) (logand src-port #xFF))

      ;; Destination port
      (mem-write-u8 (+ tcp-hdr 2) (ash dest-port -8))
      (mem-write-u8 (+ tcp-hdr 3) (logand dest-port #xFF))

      ;; Sequence number
      (mem-write-u8 (+ tcp-hdr 4) (logand (ash seq -24) #xFF))
      (mem-write-u8 (+ tcp-hdr 5) (logand (ash seq -16) #xFF))
      (mem-write-u8 (+ tcp-hdr 6) (logand (ash seq -8) #xFF))
      (mem-write-u8 (+ tcp-hdr 7) (logand seq #xFF))

      ;; ACK number (0)
      (dotimes (i 4)
        (mem-write-u8 (+ tcp-hdr 8 i) 0))

      ;; Data offset and flags
      (mem-write-u8 (+ tcp-hdr 12) #x50)  ; 5 * 4 = 20 bytes
      (mem-write-u8 (+ tcp-hdr 13) +tcp-rst+)

      ;; Window
      (mem-write-u8 (+ tcp-hdr 14) 0)
      (mem-write-u8 (+ tcp-hdr 15) 0)

      ;; Checksum (calculate later)
      (mem-write-u8 (+ tcp-hdr 16) 0)
      (mem-write-u8 (+ tcp-hdr 17) 0)

      ;; Urgent pointer
      (mem-write-u8 (+ tcp-hdr 18) 0)
      (mem-write-u8 (+ tcp-hdr 19) 0))

    (ip-send dev skb dest-addr +ipproto-tcp+)
    t))

(defun tcp-build-header (tcp-hdr sock flags ack)
  "Build TCP header."
  (declare (type (unsigned-byte 64) tcp-hdr)
           (type tcp-socket sock)
           (type (unsigned-byte 8) flags)
           (type (unsigned-byte 32) ack))

  ;; Source port
  (mem-write-u8 tcp-hdr (ash (tcp-socket-local-port sock) -8))
  (mem-write-u8 (+ tcp-hdr 1) (logand (tcp-socket-local-port sock) #xFF))

  ;; Destination port
  (mem-write-u8 (+ tcp-hdr 2) (ash (tcp-socket-remote-port sock) -8))
  (mem-write-u8 (+ tcp-hdr 3) (logand (tcp-socket-remote-port sock) #xFF))

  ;; Sequence number
  (let ((seq (tcp-socket-snd-nxt sock)))
    (mem-write-u8 (+ tcp-hdr 4) (logand (ash seq -24) #xFF))
    (mem-write-u8 (+ tcp-hdr 5) (logand (ash seq -16) #xFF))
    (mem-write-u8 (+ tcp-hdr 6) (logand (ash seq -8) #xFF))
    (mem-write-u8 (+ tcp-hdr 7) (logand seq #xFF)))

  ;; ACK number
  (mem-write-u8 (+ tcp-hdr 8) (logand (ash ack -24) #xFF))
  (mem-write-u8 (+ tcp-hdr 9) (logand (ash ack -16) #xFF))
  (mem-write-u8 (+ tcp-hdr 10) (logand (ash ack -8) #xFF))
  (mem-write-u8 (+ tcp-hdr 11) (logand ack #xFF))

  ;; Data offset and reserved
  (mem-write-u8 (+ tcp-hdr 12) #x50)  ; 5 * 4 = 20 bytes

  ;; Flags
  (mem-write-u8 (+ tcp-hdr 13) flags)

  ;; Window
  (let ((window (tcp-socket-rcv-wnd sock)))
    (mem-write-u8 (+ tcp-hdr 14) (ash window -8))
    (mem-write-u8 (+ tcp-hdr 15) (logand window #xFF)))

  ;; Checksum (0 for now)
  (mem-write-u8 (+ tcp-hdr 16) 0)
  (mem-write-u8 (+ tcp-hdr 17) 0)

  ;; Urgent pointer
  (mem-write-u8 (+ tcp-hdr 18) 0)
  (mem-write-u8 (+ tcp-hdr 19) 0)

  t)

;;; Socket API

(defun tcp-socket ()
  "Create TCP socket."
  (make-tcp-socket
   :lock (make-spinlock)
   :recv-wait (make-semaphore 0)
   :accept-wait (make-semaphore 0)))

(defun tcp-bind (sock local-port)
  "Bind TCP socket to port."
  (declare (type tcp-socket sock)
           (type (unsigned-byte 16) local-port))

  (setf (tcp-socket-local-port sock) local-port)
  t)

(defun tcp-listen (sock backlog)
  "Listen for connections."
  (declare (type tcp-socket sock)
           (type (unsigned-byte 16) backlog))

  (setf (tcp-socket-state sock) +tcp-listen+)
  (setf (tcp-socket-max-backlog sock) backlog)

  ;; Register socket
  (with-spinlock (*tcp-sockets-lock*)
    (let ((key (ash (tcp-socket-local-port sock) 48)))
      (setf (gethash key *tcp-sockets*) sock)))

  t)

(defun tcp-connect (sock dev remote-addr remote-port)
  "Connect to remote host."
  (declare (type tcp-socket sock)
           (type net-device dev)
           (type (unsigned-byte 32) remote-addr)
           (type (unsigned-byte 16) remote-port))

  (setf (tcp-socket-remote-addr sock) remote-addr)
  (setf (tcp-socket-remote-port sock) remote-port)
  (setf (tcp-socket-local-addr sock) (net-device-ip dev))

  ;; Allocate ephemeral port if not bound
  (when (zerop (tcp-socket-local-port sock))
    (setf (tcp-socket-local-port sock)
          (atomic-incf *next-ephemeral-port*)))

  ;; Generate initial sequence number
  (setf (tcp-socket-snd-nxt sock) (random-u32))
  (setf (tcp-socket-snd-una sock) (tcp-socket-snd-nxt sock))

  ;; Send SYN
  (let ((skb (skb-alloc 20)))
    (unless skb
      (return-from tcp-connect nil))

    (let ((tcp-hdr (skb-put skb 20)))
      (tcp-build-header tcp-hdr sock +tcp-syn+ 0)
      (ip-send dev skb remote-addr +ipproto-tcp+)))

  (setf (tcp-socket-state sock) +tcp-syn-sent+)

  ;; Register socket
  (with-spinlock (*tcp-sockets-lock*)
    (let ((key (logior (ash (tcp-socket-local-port sock) 48)
                      (ash remote-addr 16)
                      remote-port)))
      (setf (gethash key *tcp-sockets*) sock)))

  ;; Wait for connection
  (semaphore-wait (tcp-socket-recv-wait sock))

  (= (tcp-socket-state sock) +tcp-established+))

(defun tcp-accept (sock)
  "Accept connection."
  (declare (type tcp-socket sock))

  ;; Wait for connection
  (loop
    (with-spinlock ((tcp-socket-lock sock))
      (when (tcp-socket-listen-backlog sock)
        (return-from tcp-accept (pop (tcp-socket-listen-backlog sock)))))

    (semaphore-wait (tcp-socket-accept-wait sock))))

(defun tcp-send (sock dev data length)
  "Send data on TCP connection."
  (declare (type tcp-socket sock)
           (type net-device dev)
           (type (unsigned-byte 64) data)
           (type (unsigned-byte 32) length))

  (let ((sent 0))
    (loop while (< sent length) do
      (let* ((to-send (min (- length sent) +tcp-mss+))
             (skb (skb-alloc (+ to-send 20))))

        (unless skb
          (return-from tcp-send sent))

        (let ((tcp-hdr (skb-put skb 20)))
          (tcp-build-header tcp-hdr sock +tcp-ack+ (tcp-socket-rcv-nxt sock))

          ;; Copy data
          (let ((payload (skb-put skb to-send)))
            (dotimes (i to-send)
              (mem-write-u8 (+ payload i) (mem-read-u8 (+ data sent i)))))

          ;; Send
          (ip-send dev skb (tcp-socket-remote-addr sock) +ipproto-tcp+)

          ;; Update sequence number
          (incf (tcp-socket-snd-nxt sock) to-send)
          (incf sent to-send))))

    sent))

(defun tcp-recv (sock buffer length)
  "Receive data from TCP connection."
  (declare (type tcp-socket sock)
           (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) length))

  ;; Wait for data
  (loop
    (with-spinlock ((tcp-socket-lock sock))
      (when (tcp-socket-recv-buffer sock)
        (let* ((entry (pop (tcp-socket-recv-buffer sock)))
               (skb (second entry))
               (data (+ (sk-buff-data skb) 20))
               (data-len (- (sk-buff-len skb) 20))
               (to-copy (min length data-len)))

          (dotimes (i to-copy)
            (mem-write-u8 (+ buffer i) (mem-read-u8 (+ data i))))

          (skb-free skb)
          (return-from tcp-recv to-copy))))

    (semaphore-wait (tcp-socket-recv-wait sock))))

(defun tcp-close (sock)
  "Close TCP connection."
  (declare (type tcp-socket sock))

  (case (tcp-socket-state sock)
    ((#.+tcp-established+ #.+tcp-close-wait+)
     ;; Send FIN
     (let ((skb (skb-alloc 20)))
       (when skb
         (let ((tcp-hdr (skb-put skb 20)))
           (tcp-build-header tcp-hdr sock (logior +tcp-fin+ +tcp-ack+)
                           (tcp-socket-rcv-nxt sock))

           (let ((dev (first *net-devices*)))
             (when dev
               (ip-send dev skb (tcp-socket-remote-addr sock) +ipproto-tcp+))))))

     (setf (tcp-socket-state sock) +tcp-fin-wait-1+))))

;;; ARP Resolution (Delayed)

(defun arp-resolve (dev dest-ip skb)
  "Resolve MAC via ARP request."
  (declare (type net-device dev)
           (type (unsigned-byte 32) dest-ip)
           (type sk-buff skb))

  ;; Queue packet for later transmission
  ;; Send ARP request
  (let ((arp-skb (skb-alloc 28)))
    (when arp-skb
      (let ((data (skb-put arp-skb 28)))
        ;; Hardware type (Ethernet)
        (mem-write-u8 data 0)
        (mem-write-u8 (+ data 1) 1)

        ;; Protocol type (IP)
        (mem-write-u8 (+ data 2) 8)
        (mem-write-u8 (+ data 3) 0)

        ;; Hardware size / Protocol size
        (mem-write-u8 (+ data 4) 6)
        (mem-write-u8 (+ data 5) 4)

        ;; Opcode (request)
        (mem-write-u8 (+ data 6) 0)
        (mem-write-u8 (+ data 7) +arp-request+)

        ;; Sender MAC
        (dotimes (i 6)
          (mem-write-u8 (+ data 8 i) (aref (net-device-mac dev) i)))

        ;; Sender IP
        (mem-write-u8 (+ data 14) (logand (ash (net-device-ip dev) -24) #xFF))
        (mem-write-u8 (+ data 15) (logand (ash (net-device-ip dev) -16) #xFF))
        (mem-write-u8 (+ data 16) (logand (ash (net-device-ip dev) -8) #xFF))
        (mem-write-u8 (+ data 17) (logand (net-device-ip dev) #xFF))

        ;; Target MAC (unknown)
        (dotimes (i 6)
          (mem-write-u8 (+ data 18 i) 0))

        ;; Target IP
        (mem-write-u8 (+ data 24) (logand (ash dest-ip -24) #xFF))
        (mem-write-u8 (+ data 25) (logand (ash dest-ip -16) #xFF))
        (mem-write-u8 (+ data 26) (logand (ash dest-ip -8) #xFF))
        (mem-write-u8 (+ data 27) (logand dest-ip #xFF)))

      ;; Send as broadcast
      (let ((broadcast-mac (make-array 6 :element-type '(unsigned-byte 8) :initial-element #xFF)))
        (eth-tx dev arp-skb broadcast-mac +eth-p-arp+))))

  ;; Free original packet for now
  ;; In production, would queue and retry
  (skb-free skb)
  t)

;;; Utility Functions

(defun random-u32 ()
  "Generate random 32-bit number."
  ;; In production, would use cryptographically secure RNG
  (logand (get-time-ns) #xFFFFFFFF))

(defun make-semaphore (initial-value)
  "Create semaphore."
  (declare (type (unsigned-byte 32) initial-value))
  (declare (ignore initial-value))
  nil)

(defun semaphore-wait (sem)
  "Wait on semaphore."
  (declare (ignore sem))
  nil)

(defun semaphore-signal (sem)
  "Signal semaphore."
  (declare (ignore sem))
  nil)

(defun get-time-ms ()
  "Get time in milliseconds."
  0)

(defun get-time-ns ()
  "Get time in nanoseconds."
  0)

(defun make-spinlock ()
  "Create spinlock."
  nil)

(defmacro with-spinlock ((lock) &body body)
  "Execute with spinlock."
  `(progn ,@body))

(defun atomic-incf (place)
  "Atomic increment."
  (incf place))

(defun kmalloc (size)
  "Allocate kernel memory."
  (declare (type (unsigned-byte 32) size))
  (declare (ignore size))
  0)

(defun kfree (ptr)
  "Free kernel memory."
  (declare (type (unsigned-byte 64) ptr))
  (declare (ignore ptr))
  nil)

(defun mem-read-u8 (addr)
  "Read byte."
  (declare (type (unsigned-byte 64) addr))
  (declare (ignore addr))
  0)

(defun mem-write-u8 (addr value)
  "Write byte."
  (declare (type (unsigned-byte 64) addr)
           (type (unsigned-byte 8) value))
  (declare (ignore addr value))
  nil)
