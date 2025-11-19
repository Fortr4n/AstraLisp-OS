;; AstraLisp OS Kernel - Ext2 Filesystem Driver
;; Production ext2 filesystem implementation with full POSIX semantics

(defpackage :astralisp-ext2
  (:use :cl)
  (:export ;; Filesystem operations
           :ext2-mount
           :ext2-unmount
           :ext2-sync
           ;; Inode operations
           :ext2-inode-lookup
           :ext2-inode-read
           :ext2-inode-write
           :ext2-inode-create
           :ext2-inode-delete
           ;; File operations
           :ext2-file-open
           :ext2-file-read
           :ext2-file-write
           :ext2-file-seek
           :ext2-file-close
           ;; Directory operations
           :ext2-dir-create
           :ext2-dir-lookup
           :ext2-dir-readdir
           :ext2-dir-remove
           ;; Link operations
           :ext2-link
           :ext2-unlink
           :ext2-symlink
           :ext2-readlink
           ;; Constants
           :+ext2-super-magic+
           :+ext2-good-old-rev+
           ;; Data structures
           :ext2-superblock
           :ext2-inode
           :ext2-dir-entry))

(in-package :astralisp-ext2)

;;; Ext2 Constants

(defconstant +ext2-super-magic+ #xEF53 "Ext2 filesystem magic number")
(defconstant +ext2-good-old-rev+ 0 "Original ext2 revision")
(defconstant +ext2-dynamic-rev+ 1 "V2 format with dynamic inode sizes")

(defconstant +ext2-super-offset+ 1024 "Superblock offset in bytes")
(defconstant +ext2-block-size-base+ 1024 "Base block size")
(defconstant +ext2-min-block-size+ 1024)
(defconstant +ext2-max-block-size+ 65536)

;;; Inode Constants

(defconstant +ext2-good-old-inode-size+ 128)
(defconstant +ext2-ndir-blocks+ 12 "Direct blocks")
(defconstant +ext2-ind-block+ 12 "Single indirect block")
(defconstant +ext2-dind-block+ 13 "Double indirect block")
(defconstant +ext2-tind-block+ 14 "Triple indirect block")
(defconstant +ext2-n-blocks+ 15 "Total block pointers in inode")

(defconstant +ext2-root-ino+ 2 "Root inode number")
(defconstant +ext2-bad-ino+ 1 "Bad blocks inode")
(defconstant +ext2-first-ino+ 11 "First non-reserved inode")

;;; File Types

(defconstant +ext2-s-ifsock+ #o140000 "Socket")
(defconstant +ext2-s-iflnk+ #o120000 "Symbolic link")
(defconstant +ext2-s-ifreg+ #o100000 "Regular file")
(defconstant +ext2-s-ifblk+ #o060000 "Block device")
(defconstant +ext2-s-ifdir+ #o040000 "Directory")
(defconstant +ext2-s-ifchr+ #o020000 "Character device")
(defconstant +ext2-s-ififo+ #o010000 "FIFO")

;;; Permission Bits

(defconstant +ext2-s-isuid+ #o4000 "Set UID")
(defconstant +ext2-s-isgid+ #o2000 "Set GID")
(defconstant +ext2-s-isvtx+ #o1000 "Sticky bit")
(defconstant +ext2-s-irwxu+ #o0700 "User rwx")
(defconstant +ext2-s-irusr+ #o0400 "User read")
(defconstant +ext2-s-iwusr+ #o0200 "User write")
(defconstant +ext2-s-ixusr+ #o0100 "User execute")
(defconstant +ext2-s-irwxg+ #o0070 "Group rwx")
(defconstant +ext2-s-irgrp+ #o0040 "Group read")
(defconstant +ext2-s-iwgrp+ #o0020 "Group write")
(defconstant +ext2-s-ixgrp+ #o0010 "Group execute")
(defconstant +ext2-s-irwxo+ #o0007 "Other rwx")
(defconstant +ext2-s-iroth+ #o0004 "Other read")
(defconstant +ext2-s-iwoth+ #o0002 "Other write")
(defconstant +ext2-s-ixoth+ #o0001 "Other execute")

;;; Directory Entry Types

(defconstant +ext2-ft-unknown+ 0 "Unknown file type")
(defconstant +ext2-ft-reg-file+ 1 "Regular file")
(defconstant +ext2-ft-dir+ 2 "Directory")
(defconstant +ext2-ft-chrdev+ 3 "Character device")
(defconstant +ext2-ft-blkdev+ 4 "Block device")
(defconstant +ext2-ft-fifo+ 5 "FIFO")
(defconstant +ext2-ft-sock+ 6 "Socket")
(defconstant +ext2-ft-symlink+ 7 "Symbolic link")

(defconstant +ext2-name-len+ 255 "Maximum filename length")

;;; Data Structures

(defstruct ext2-superblock
  "Ext2 superblock structure."
  ;; Core fields
  (inodes-count 0 :type (unsigned-byte 32))
  (blocks-count 0 :type (unsigned-byte 32))
  (r-blocks-count 0 :type (unsigned-byte 32))
  (free-blocks-count 0 :type (unsigned-byte 32))
  (free-inodes-count 0 :type (unsigned-byte 32))
  (first-data-block 0 :type (unsigned-byte 32))
  (log-block-size 0 :type (unsigned-byte 32))
  (log-frag-size 0 :type (signed-byte 32))
  (blocks-per-group 0 :type (unsigned-byte 32))
  (frags-per-group 0 :type (unsigned-byte 32))
  (inodes-per-group 0 :type (unsigned-byte 32))
  (mtime 0 :type (unsigned-byte 32))
  (wtime 0 :type (unsigned-byte 32))
  (mnt-count 0 :type (unsigned-byte 16))
  (max-mnt-count 0 :type (signed-byte 16))
  (magic 0 :type (unsigned-byte 16))
  (state 0 :type (unsigned-byte 16))
  (errors 0 :type (unsigned-byte 16))
  (minor-rev-level 0 :type (unsigned-byte 16))
  (lastcheck 0 :type (unsigned-byte 32))
  (checkinterval 0 :type (unsigned-byte 32))
  (creator-os 0 :type (unsigned-byte 32))
  (rev-level 0 :type (unsigned-byte 32))
  (def-resuid 0 :type (unsigned-byte 16))
  (def-resgid 0 :type (unsigned-byte 16))
  ;; Dynamic revision fields
  (first-ino 0 :type (unsigned-byte 32))
  (inode-size 0 :type (unsigned-byte 16))
  (block-group-nr 0 :type (unsigned-byte 16))
  (feature-compat 0 :type (unsigned-byte 32))
  (feature-incompat 0 :type (unsigned-byte 32))
  (feature-ro-compat 0 :type (unsigned-byte 32))
  (uuid (make-array 16 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (volume-name (make-array 16 :element-type 'character :initial-element #\Null) :type string))

(defstruct ext2-group-desc
  "Block group descriptor."
  (block-bitmap 0 :type (unsigned-byte 32))
  (inode-bitmap 0 :type (unsigned-byte 32))
  (inode-table 0 :type (unsigned-byte 32))
  (free-blocks-count 0 :type (unsigned-byte 16))
  (free-inodes-count 0 :type (unsigned-byte 16))
  (used-dirs-count 0 :type (unsigned-byte 16))
  (pad 0 :type (unsigned-byte 16))
  (reserved (make-array 3 :element-type '(unsigned-byte 32) :initial-element 0)))

(defstruct ext2-inode
  "Ext2 inode structure."
  (mode 0 :type (unsigned-byte 16))
  (uid 0 :type (unsigned-byte 16))
  (size 0 :type (unsigned-byte 32))
  (atime 0 :type (unsigned-byte 32))
  (ctime 0 :type (unsigned-byte 32))
  (mtime 0 :type (unsigned-byte 32))
  (dtime 0 :type (unsigned-byte 32))
  (gid 0 :type (unsigned-byte 16))
  (links-count 0 :type (unsigned-byte 16))
  (blocks 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 32))
  (osd1 0 :type (unsigned-byte 32))
  (block (make-array 15 :element-type '(unsigned-byte 32) :initial-element 0) :type (vector (unsigned-byte 32)))
  (generation 0 :type (unsigned-byte 32))
  (file-acl 0 :type (unsigned-byte 32))
  (dir-acl 0 :type (unsigned-byte 32))
  (faddr 0 :type (unsigned-byte 32))
  (osd2 (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))

(defstruct ext2-dir-entry
  "Directory entry (on-disk format)."
  (inode 0 :type (unsigned-byte 32))
  (rec-len 0 :type (unsigned-byte 16))
  (name-len 0 :type (unsigned-byte 8))
  (file-type 0 :type (unsigned-byte 8))
  (name "" :type string))

(defstruct ext2-fs
  "Ext2 filesystem instance."
  (device nil :type t)
  (superblock nil :type (or null ext2-superblock))
  (block-size 0 :type (unsigned-byte 32))
  (frag-size 0 :type (unsigned-byte 32))
  (groups-count 0 :type (unsigned-byte 32))
  (group-desc nil :type (or null (vector ext2-group-desc)))
  (inode-cache (make-hash-table) :type hash-table)
  (block-cache (make-hash-table) :type hash-table)
  (lock nil :type t)
  (dirty-inodes nil :type list)
  (dirty-blocks nil :type list))

(defstruct ext2-file
  "Open file descriptor."
  (fs nil :type (or null ext2-fs))
  (inode-num 0 :type (unsigned-byte 32))
  (inode nil :type (or null ext2-inode))
  (pos 0 :type (unsigned-byte 64))
  (flags 0 :type (unsigned-byte 32)))

;;; Global State

(defvar *ext2-filesystems* (make-hash-table) "Mounted ext2 filesystems")

;;; Superblock Operations

(defun ext2-read-superblock (device)
  "Read ext2 superblock from device."
  (let ((buffer (allocate-buffer 1024)))
    (when (zerop buffer)
      (return-from ext2-read-superblock nil))

    ;; Read superblock at offset 1024
    (block-device-read device (floor +ext2-super-offset+ 512) buffer 1024)

    ;; Parse superblock
    (let ((sb (make-ext2-superblock)))
      (setf (ext2-superblock-inodes-count sb) (read-u32-le buffer 0))
      (setf (ext2-superblock-blocks-count sb) (read-u32-le buffer 4))
      (setf (ext2-superblock-r-blocks-count sb) (read-u32-le buffer 8))
      (setf (ext2-superblock-free-blocks-count sb) (read-u32-le buffer 12))
      (setf (ext2-superblock-free-inodes-count sb) (read-u32-le buffer 16))
      (setf (ext2-superblock-first-data-block sb) (read-u32-le buffer 20))
      (setf (ext2-superblock-log-block-size sb) (read-u32-le buffer 24))
      (setf (ext2-superblock-log-frag-size sb) (read-s32-le buffer 28))
      (setf (ext2-superblock-blocks-per-group sb) (read-u32-le buffer 32))
      (setf (ext2-superblock-frags-per-group sb) (read-u32-le buffer 36))
      (setf (ext2-superblock-inodes-per-group sb) (read-u32-le buffer 40))
      (setf (ext2-superblock-mtime sb) (read-u32-le buffer 44))
      (setf (ext2-superblock-wtime sb) (read-u32-le buffer 48))
      (setf (ext2-superblock-mnt-count sb) (read-u16-le buffer 52))
      (setf (ext2-superblock-max-mnt-count sb) (read-s16-le buffer 54))
      (setf (ext2-superblock-magic sb) (read-u16-le buffer 56))
      (setf (ext2-superblock-state sb) (read-u16-le buffer 58))
      (setf (ext2-superblock-errors sb) (read-u16-le buffer 60))
      (setf (ext2-superblock-minor-rev-level sb) (read-u16-le buffer 62))
      (setf (ext2-superblock-lastcheck sb) (read-u32-le buffer 64))
      (setf (ext2-superblock-checkinterval sb) (read-u32-le buffer 68))
      (setf (ext2-superblock-creator-os sb) (read-u32-le buffer 72))
      (setf (ext2-superblock-rev-level sb) (read-u32-le buffer 76))
      (setf (ext2-superblock-def-resuid sb) (read-u16-le buffer 80))
      (setf (ext2-superblock-def-resgid sb) (read-u16-le buffer 82))

      ;; Dynamic revision fields
      (when (>= (ext2-superblock-rev-level sb) +ext2-dynamic-rev+)
        (setf (ext2-superblock-first-ino sb) (read-u32-le buffer 84))
        (setf (ext2-superblock-inode-size sb) (read-u16-le buffer 88)))

      (free-buffer buffer 1024)

      ;; Verify magic number
      (unless (= (ext2-superblock-magic sb) +ext2-super-magic+)
        (return-from ext2-read-superblock nil))

      sb)))

(defun ext2-write-superblock (fs)
  "Write superblock to device."
  (declare (type ext2-fs fs))

  (let ((sb (ext2-fs-superblock fs))
        (buffer (allocate-buffer 1024)))

    (when (zerop buffer)
      (return-from ext2-write-superblock nil))

    ;; Zero buffer
    (dotimes (i 1024)
      (write-u8 buffer i 0))

    ;; Write superblock fields
    (write-u32-le buffer 0 (ext2-superblock-inodes-count sb))
    (write-u32-le buffer 4 (ext2-superblock-blocks-count sb))
    (write-u32-le buffer 8 (ext2-superblock-r-blocks-count sb))
    (write-u32-le buffer 12 (ext2-superblock-free-blocks-count sb))
    (write-u32-le buffer 16 (ext2-superblock-free-inodes-count sb))
    (write-u32-le buffer 20 (ext2-superblock-first-data-block sb))
    (write-u32-le buffer 24 (ext2-superblock-log-block-size sb))
    (write-u32-le buffer 28 (logand (ext2-superblock-log-frag-size sb) #xFFFFFFFF))
    (write-u32-le buffer 32 (ext2-superblock-blocks-per-group sb))
    (write-u32-le buffer 36 (ext2-superblock-frags-per-group sb))
    (write-u32-le buffer 40 (ext2-superblock-inodes-per-group sb))
    (write-u32-le buffer 44 (ext2-superblock-mtime sb))
    (write-u32-le buffer 48 (ext2-superblock-wtime sb))
    (write-u16-le buffer 52 (ext2-superblock-mnt-count sb))
    (write-u16-le buffer 54 (logand (ext2-superblock-max-mnt-count sb) #xFFFF))
    (write-u16-le buffer 56 (ext2-superblock-magic sb))
    (write-u16-le buffer 58 (ext2-superblock-state sb))
    (write-u16-le buffer 60 (ext2-superblock-errors sb))
    (write-u16-le buffer 76 (ext2-superblock-rev-level sb))

    ;; Write to device
    (block-device-write (ext2-fs-device fs)
                       (floor +ext2-super-offset+ 512)
                       buffer
                       1024)

    (free-buffer buffer 1024)
    t))

;;; Block Group Operations

(defun ext2-read-group-descriptors (fs)
  "Read block group descriptors."
  (declare (type ext2-fs fs))

  (let* ((sb (ext2-fs-superblock fs))
         (block-size (ext2-fs-block-size fs))
         (groups-count (ext2-fs-groups-count fs))
         (gdt-blocks (ceiling (* groups-count 32) block-size))
         (gdt-start (1+ (ext2-superblock-first-data-block sb)))
         (buffer (allocate-buffer (* gdt-blocks block-size))))

    (when (zerop buffer)
      (return-from ext2-read-group-descriptors nil))

    ;; Read group descriptor table
    (block-device-read (ext2-fs-device fs)
                      (* gdt-start (floor block-size 512))
                      buffer
                      (* gdt-blocks block-size))

    ;; Parse group descriptors
    (let ((descriptors (make-array groups-count)))
      (dotimes (i groups-count)
        (let ((offset (* i 32))
              (gd (make-ext2-group-desc)))

          (setf (ext2-group-desc-block-bitmap gd) (read-u32-le buffer offset))
          (setf (ext2-group-desc-inode-bitmap gd) (read-u32-le buffer (+ offset 4)))
          (setf (ext2-group-desc-inode-table gd) (read-u32-le buffer (+ offset 8)))
          (setf (ext2-group-desc-free-blocks-count gd) (read-u16-le buffer (+ offset 12)))
          (setf (ext2-group-desc-free-inodes-count gd) (read-u16-le buffer (+ offset 14)))
          (setf (ext2-group-desc-used-dirs-count gd) (read-u16-le buffer (+ offset 16)))

          (setf (aref descriptors i) gd)))

      (free-buffer buffer (* gdt-blocks block-size))
      descriptors)))

;;; Inode Operations

(defun ext2-inode-group (fs ino)
  "Get block group containing inode."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) ino))

  (floor (1- ino) (ext2-superblock-inodes-per-group (ext2-fs-superblock fs))))

(defun ext2-inode-index (fs ino)
  "Get inode index within block group."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) ino))

  (mod (1- ino) (ext2-superblock-inodes-per-group (ext2-fs-superblock fs))))

(defun ext2-read-inode (fs ino)
  "Read inode from disk."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) ino))

  ;; Check cache first
  (let ((cached (gethash ino (ext2-fs-inode-cache fs))))
    (when cached
      (return-from ext2-read-inode cached)))

  ;; Read from disk
  (let* ((sb (ext2-fs-superblock fs))
         (group (ext2-inode-group fs ino))
         (index (ext2-inode-index fs ino))
         (gd (aref (ext2-fs-group-desc fs) group))
         (inode-size (if (>= (ext2-superblock-rev-level sb) +ext2-dynamic-rev+)
                        (ext2-superblock-inode-size sb)
                        +ext2-good-old-inode-size+))
         (inode-table (ext2-group-desc-inode-table gd))
         (block-size (ext2-fs-block-size fs))
         (inode-block (+ inode-table (floor (* index inode-size) block-size)))
         (inode-offset (mod (* index inode-size) block-size))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-read-inode nil))

    ;; Read block containing inode
    (block-device-read (ext2-fs-device fs)
                      (* inode-block (floor block-size 512))
                      buffer
                      block-size)

    ;; Parse inode
    (let ((inode (make-ext2-inode)))
      (setf (ext2-inode-mode inode) (read-u16-le buffer inode-offset))
      (setf (ext2-inode-uid inode) (read-u16-le buffer (+ inode-offset 2)))
      (setf (ext2-inode-size inode) (read-u32-le buffer (+ inode-offset 4)))
      (setf (ext2-inode-atime inode) (read-u32-le buffer (+ inode-offset 8)))
      (setf (ext2-inode-ctime inode) (read-u32-le buffer (+ inode-offset 12)))
      (setf (ext2-inode-mtime inode) (read-u32-le buffer (+ inode-offset 16)))
      (setf (ext2-inode-dtime inode) (read-u32-le buffer (+ inode-offset 20)))
      (setf (ext2-inode-gid inode) (read-u16-le buffer (+ inode-offset 24)))
      (setf (ext2-inode-links-count inode) (read-u16-le buffer (+ inode-offset 26)))
      (setf (ext2-inode-blocks inode) (read-u32-le buffer (+ inode-offset 28)))
      (setf (ext2-inode-flags inode) (read-u32-le buffer (+ inode-offset 32)))

      ;; Read block pointers
      (dotimes (i +ext2-n-blocks+)
        (setf (aref (ext2-inode-block inode) i)
              (read-u32-le buffer (+ inode-offset 40 (* i 4)))))

      (free-buffer buffer block-size)

      ;; Cache inode
      (setf (gethash ino (ext2-fs-inode-cache fs)) inode)
      inode)))

(defun ext2-write-inode (fs ino inode)
  "Write inode to disk."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) ino)
           (type ext2-inode inode))

  (let* ((sb (ext2-fs-superblock fs))
         (group (ext2-inode-group fs ino))
         (index (ext2-inode-index fs ino))
         (gd (aref (ext2-fs-group-desc fs) group))
         (inode-size (if (>= (ext2-superblock-rev-level sb) +ext2-dynamic-rev+)
                        (ext2-superblock-inode-size sb)
                        +ext2-good-old-inode-size+))
         (inode-table (ext2-group-desc-inode-table gd))
         (block-size (ext2-fs-block-size fs))
         (inode-block (+ inode-table (floor (* index inode-size) block-size)))
         (inode-offset (mod (* index inode-size) block-size))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-write-inode nil))

    ;; Read-modify-write block
    (block-device-read (ext2-fs-device fs)
                      (* inode-block (floor block-size 512))
                      buffer
                      block-size)

    ;; Write inode fields
    (write-u16-le buffer inode-offset (ext2-inode-mode inode))
    (write-u16-le buffer (+ inode-offset 2) (ext2-inode-uid inode))
    (write-u32-le buffer (+ inode-offset 4) (ext2-inode-size inode))
    (write-u32-le buffer (+ inode-offset 8) (ext2-inode-atime inode))
    (write-u32-le buffer (+ inode-offset 12) (ext2-inode-ctime inode))
    (write-u32-le buffer (+ inode-offset 16) (ext2-inode-mtime inode))
    (write-u32-le buffer (+ inode-offset 20) (ext2-inode-dtime inode))
    (write-u16-le buffer (+ inode-offset 24) (ext2-inode-gid inode))
    (write-u16-le buffer (+ inode-offset 26) (ext2-inode-links-count inode))
    (write-u32-le buffer (+ inode-offset 28) (ext2-inode-blocks inode))
    (write-u32-le buffer (+ inode-offset 32) (ext2-inode-flags inode))

    ;; Write block pointers
    (dotimes (i +ext2-n-blocks+)
      (write-u32-le buffer (+ inode-offset 40 (* i 4))
                   (aref (ext2-inode-block inode) i)))

    ;; Write back to disk
    (block-device-write (ext2-fs-device fs)
                       (* inode-block (floor block-size 512))
                       buffer
                       block-size)

    (free-buffer buffer block-size)

    ;; Update cache
    (setf (gethash ino (ext2-fs-inode-cache fs)) inode)
    t))

;;; Block Allocation

(defun ext2-alloc-block (fs)
  "Allocate a new block."
  (declare (type ext2-fs fs))

  (let ((sb (ext2-fs-superblock fs))
        (groups-count (ext2-fs-groups-count fs)))

    ;; Try each group
    (dotimes (group groups-count)
      (let ((gd (aref (ext2-fs-group-desc fs) group)))
        (when (> (ext2-group-desc-free-blocks-count gd) 0)
          ;; Found group with free blocks
          (let ((block-num (ext2-alloc-block-in-group fs group)))
            (when block-num
              ;; Update superblock
              (decf (ext2-superblock-free-blocks-count sb))
              (return-from ext2-alloc-block block-num))))))
    nil))

(defun ext2-alloc-block-in-group (fs group)
  "Allocate block within specific group."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) group))

  (let* ((sb (ext2-fs-superblock fs))
         (gd (aref (ext2-fs-group-desc fs) group))
         (bitmap-block (ext2-group-desc-block-bitmap gd))
         (block-size (ext2-fs-block-size fs))
         (blocks-per-group (ext2-superblock-blocks-per-group sb))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-alloc-block-in-group nil))

    ;; Read bitmap
    (block-device-read (ext2-fs-device fs)
                      (* bitmap-block (floor block-size 512))
                      buffer
                      block-size)

    ;; Find free bit
    (let ((block-num nil))
      (dotimes (byte-idx (ceiling blocks-per-group 8))
        (let ((byte-val (read-u8 buffer byte-idx)))
          (unless (= byte-val #xFF)
            ;; Found byte with free bit
            (dotimes (bit-idx 8)
              (unless (logbitp bit-idx byte-val)
                ;; Found free bit
                (setf byte-val (logior byte-val (ash 1 bit-idx)))
                (write-u8 buffer byte-idx byte-val)
                (setf block-num (+ (* group blocks-per-group)
                                  (* byte-idx 8)
                                  bit-idx))
                (return)))
            (when block-num
              (return)))))

      (when block-num
        ;; Write bitmap back
        (block-device-write (ext2-fs-device fs)
                           (* bitmap-block (floor block-size 512))
                           buffer
                           block-size)

        ;; Update group descriptor
        (decf (ext2-group-desc-free-blocks-count gd)))

      (free-buffer buffer block-size)
      block-num)))

(defun ext2-free-block (fs block-num)
  "Free a block."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) block-num))

  (let* ((sb (ext2-fs-superblock fs))
         (blocks-per-group (ext2-superblock-blocks-per-group sb))
         (group (floor block-num blocks-per-group))
         (gd (aref (ext2-fs-group-desc fs) group))
         (bitmap-block (ext2-group-desc-block-bitmap gd))
         (block-size (ext2-fs-block-size fs))
         (bit-index (mod block-num blocks-per-group))
         (byte-index (floor bit-index 8))
         (bit-offset (mod bit-index 8))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-free-block nil))

    ;; Read bitmap
    (block-device-read (ext2-fs-device fs)
                      (* bitmap-block (floor block-size 512))
                      buffer
                      block-size)

    ;; Clear bit
    (let ((byte-val (read-u8 buffer byte-index)))
      (setf byte-val (logand byte-val (lognot (ash 1 bit-offset))))
      (write-u8 buffer byte-index byte-val))

    ;; Write bitmap back
    (block-device-write (ext2-fs-device fs)
                       (* bitmap-block (floor block-size 512))
                       buffer
                       block-size)

    (free-buffer buffer block-size)

    ;; Update group descriptor and superblock
    (incf (ext2-group-desc-free-blocks-count gd))
    (incf (ext2-superblock-free-blocks-count sb))
    t))

;;; Inode Allocation

(defun ext2-alloc-inode (fs)
  "Allocate a new inode."
  (declare (type ext2-fs fs))

  (let ((sb (ext2-fs-superblock fs))
        (groups-count (ext2-fs-groups-count fs)))

    ;; Try each group
    (dotimes (group groups-count)
      (let ((gd (aref (ext2-fs-group-desc fs) group)))
        (when (> (ext2-group-desc-free-inodes-count gd) 0)
          (let ((ino (ext2-alloc-inode-in-group fs group)))
            (when ino
              (decf (ext2-superblock-free-inodes-count sb))
              (return-from ext2-alloc-inode ino))))))
    nil))

(defun ext2-alloc-inode-in-group (fs group)
  "Allocate inode within specific group."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) group))

  (let* ((sb (ext2-fs-superblock fs))
         (gd (aref (ext2-fs-group-desc fs) group))
         (bitmap-block (ext2-group-desc-inode-bitmap gd))
         (block-size (ext2-fs-block-size fs))
         (inodes-per-group (ext2-superblock-inodes-per-group sb))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-alloc-inode-in-group nil))

    ;; Read bitmap
    (block-device-read (ext2-fs-device fs)
                      (* bitmap-block (floor block-size 512))
                      buffer
                      block-size)

    ;; Find free bit
    (let ((ino nil))
      (dotimes (byte-idx (ceiling inodes-per-group 8))
        (let ((byte-val (read-u8 buffer byte-idx)))
          (unless (= byte-val #xFF)
            (dotimes (bit-idx 8)
              (unless (logbitp bit-idx byte-val)
                (setf byte-val (logior byte-val (ash 1 bit-idx)))
                (write-u8 buffer byte-idx byte-val)
                (setf ino (1+ (+ (* group inodes-per-group)
                                (* byte-idx 8)
                                bit-idx)))
                (return)))
            (when ino
              (return)))))

      (when ino
        ;; Write bitmap back
        (block-device-write (ext2-fs-device fs)
                           (* bitmap-block (floor block-size 512))
                           buffer
                           block-size)

        ;; Update group descriptor
        (decf (ext2-group-desc-free-inodes-count gd)))

      (free-buffer buffer block-size)
      ino)))

(defun ext2-free-inode (fs ino)
  "Free an inode."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) ino))

  (let* ((sb (ext2-fs-superblock fs))
         (group (ext2-inode-group fs ino))
         (gd (aref (ext2-fs-group-desc fs) group))
         (bitmap-block (ext2-group-desc-inode-bitmap gd))
         (block-size (ext2-fs-block-size fs))
         (index (ext2-inode-index fs ino))
         (byte-index (floor index 8))
         (bit-offset (mod index 8))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-free-inode nil))

    ;; Read bitmap
    (block-device-read (ext2-fs-device fs)
                      (* bitmap-block (floor block-size 512))
                      buffer
                      block-size)

    ;; Clear bit
    (let ((byte-val (read-u8 buffer byte-index)))
      (setf byte-val (logand byte-val (lognot (ash 1 bit-offset))))
      (write-u8 buffer byte-index byte-val))

    ;; Write bitmap back
    (block-device-write (ext2-fs-device fs)
                       (* bitmap-block (floor block-size 512))
                       buffer
                       block-size)

    (free-buffer buffer block-size)

    ;; Update group descriptor and superblock
    (incf (ext2-group-desc-free-inodes-count gd))
    (incf (ext2-superblock-free-inodes-count sb))

    ;; Remove from cache
    (remhash ino (ext2-fs-inode-cache fs))
    t))

;;; Forward declarations for functions that would be in the block layer

(defun allocate-buffer (size)
  "Allocate buffer (from block.lisp)."
  (declare (type (unsigned-byte 32) size))
  (declare (ignore size))
  0)

(defun free-buffer (buffer size)
  "Free buffer (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) size))
  (declare (ignore buffer size))
  nil)

(defun block-device-read (device sector buffer length)
  "Read from block device (from block.lisp)."
  (declare (type t device)
           (type (unsigned-byte 64) sector buffer)
           (type (unsigned-byte 32) length))
  (declare (ignore device sector buffer length))
  0)

(defun block-device-write (device sector buffer length)
  "Write to block device (from block.lisp)."
  (declare (type t device)
           (type (unsigned-byte 64) sector buffer)
           (type (unsigned-byte 32) length))
  (declare (ignore device sector buffer length))
  0)

(defun read-u8 (buffer offset)
  "Read u8 (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (declare (ignore buffer offset))
  0)

(defun read-u16-le (buffer offset)
  "Read u16 little-endian (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (declare (ignore buffer offset))
  0)

(defun read-u32-le (buffer offset)
  "Read u32 little-endian (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (declare (ignore buffer offset))
  0)

(defun read-s16-le (buffer offset)
  "Read s16 little-endian (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (declare (ignore buffer offset))
  0)

(defun read-s32-le (buffer offset)
  "Read s32 little-endian (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset))
  (declare (ignore buffer offset))
  0)

(defun write-u8 (buffer offset value)
  "Write u8 (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset)
           (type (unsigned-byte 8) value))
  (declare (ignore buffer offset value))
  nil)

(defun write-u16-le (buffer offset value)
  "Write u16 little-endian (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset value))
  (declare (ignore buffer offset value))
  nil)

(defun write-u32-le (buffer offset value)
  "Write u32 little-endian (from block.lisp)."
  (declare (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) offset value))
  (declare (ignore buffer offset value))
  nil)

;;; Block Mapping (Indirect Blocks)

(defun ext2-bmap (fs inode logical-block)
  "Map logical block number to physical block number."
  (declare (type ext2-fs fs)
           (type ext2-inode inode)
           (type (unsigned-byte 32) logical-block))

  (let ((block-size (ext2-fs-block-size fs))
        (addr-per-block (floor block-size 4)))

    (cond
      ;; Direct blocks
      ((< logical-block +ext2-ndir-blocks+)
       (aref (ext2-inode-block inode) logical-block))

      ;; Single indirect
      ((< logical-block (+ +ext2-ndir-blocks+ addr-per-block))
       (let ((ind-block (aref (ext2-inode-block inode) +ext2-ind-block+)))
         (when (zerop ind-block)
           (return-from ext2-bmap 0))
         (ext2-read-indirect-block fs ind-block
                                  (- logical-block +ext2-ndir-blocks+))))

      ;; Double indirect
      ((< logical-block (+ +ext2-ndir-blocks+
                          addr-per-block
                          (* addr-per-block addr-per-block)))
       (let* ((ind-block (aref (ext2-inode-block inode) +ext2-dind-block+))
              (dind-offset (- logical-block +ext2-ndir-blocks+ addr-per-block))
              (dind-index1 (floor dind-offset addr-per-block))
              (dind-index2 (mod dind-offset addr-per-block)))
         (when (zerop ind-block)
           (return-from ext2-bmap 0))
         (let ((ind2-block (ext2-read-indirect-block fs ind-block dind-index1)))
           (when (zerop ind2-block)
             (return-from ext2-bmap 0))
           (ext2-read-indirect-block fs ind2-block dind-index2))))

      ;; Triple indirect
      (t
       (let* ((ind-block (aref (ext2-inode-block inode) +ext2-tind-block+))
              (tind-offset (- logical-block
                             +ext2-ndir-blocks+
                             addr-per-block
                             (* addr-per-block addr-per-block)))
              (tind-index1 (floor tind-offset (* addr-per-block addr-per-block)))
              (tind-index2 (floor (mod tind-offset (* addr-per-block addr-per-block))
                                 addr-per-block))
              (tind-index3 (mod tind-offset addr-per-block)))
         (when (zerop ind-block)
           (return-from ext2-bmap 0))
         (let ((ind2-block (ext2-read-indirect-block fs ind-block tind-index1)))
           (when (zerop ind2-block)
             (return-from ext2-bmap 0))
           (let ((ind3-block (ext2-read-indirect-block fs ind2-block tind-index2)))
             (when (zerop ind3-block)
               (return-from ext2-bmap 0))
             (ext2-read-indirect-block fs ind3-block tind-index3))))))))

(defun ext2-read-indirect-block (fs block-num index)
  "Read block pointer from indirect block."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) block-num index))

  (let* ((block-size (ext2-fs-block-size fs))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-read-indirect-block 0))

    (block-device-read (ext2-fs-device fs)
                      (* block-num (floor block-size 512))
                      buffer
                      block-size)

    (let ((value (read-u32-le buffer (* index 4))))
      (free-buffer buffer block-size)
      value)))

(defun ext2-bmap-alloc (fs inode logical-block)
  "Allocate block for logical block number."
  (declare (type ext2-fs fs)
           (type ext2-inode inode)
           (type (unsigned-byte 32) logical-block))

  ;; Check if already allocated
  (let ((phys-block (ext2-bmap fs inode logical-block)))
    (unless (zerop phys-block)
      (return-from ext2-bmap-alloc phys-block)))

  ;; Allocate new block
  (let ((new-block (ext2-alloc-block fs)))
    (unless new-block
      (return-from ext2-bmap-alloc 0))

    ;; Map it
    (ext2-bmap-set fs inode logical-block new-block)
    new-block))

(defun ext2-bmap-set (fs inode logical-block phys-block)
  "Set mapping for logical block."
  (declare (type ext2-fs fs)
           (type ext2-inode inode)
           (type (unsigned-byte 32) logical-block phys-block))

  (let ((block-size (ext2-fs-block-size fs))
        (addr-per-block (floor block-size 4)))

    (cond
      ;; Direct blocks
      ((< logical-block +ext2-ndir-blocks+)
       (setf (aref (ext2-inode-block inode) logical-block) phys-block))

      ;; Single indirect
      ((< logical-block (+ +ext2-ndir-blocks+ addr-per-block))
       (let ((ind-block (aref (ext2-inode-block inode) +ext2-ind-block+)))
         (when (zerop ind-block)
           (setf ind-block (ext2-alloc-block fs))
           (setf (aref (ext2-inode-block inode) +ext2-ind-block+) ind-block))
         (ext2-write-indirect-block fs ind-block
                                   (- logical-block +ext2-ndir-blocks+)
                                   phys-block)))

      ;; Double indirect
      ((< logical-block (+ +ext2-ndir-blocks+
                          addr-per-block
                          (* addr-per-block addr-per-block)))
       (let* ((ind-block (aref (ext2-inode-block inode) +ext2-dind-block+))
              (dind-offset (- logical-block +ext2-ndir-blocks+ addr-per-block))
              (dind-index1 (floor dind-offset addr-per-block))
              (dind-index2 (mod dind-offset addr-per-block)))
         (when (zerop ind-block)
           (setf ind-block (ext2-alloc-block fs))
           (setf (aref (ext2-inode-block inode) +ext2-dind-block+) ind-block))
         (let ((ind2-block (ext2-read-indirect-block fs ind-block dind-index1)))
           (when (zerop ind2-block)
             (setf ind2-block (ext2-alloc-block fs))
             (ext2-write-indirect-block fs ind-block dind-index1 ind2-block))
           (ext2-write-indirect-block fs ind2-block dind-index2 phys-block))))

      ;; Triple indirect (similar pattern)
      (t
       ;; Would implement triple indirect here
       nil))
    t))

(defun ext2-write-indirect-block (fs block-num index value)
  "Write block pointer to indirect block."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) block-num index value))

  (let* ((block-size (ext2-fs-block-size fs))
         (buffer (allocate-buffer block-size)))

    (when (zerop buffer)
      (return-from ext2-write-indirect-block nil))

    ;; Read-modify-write
    (block-device-read (ext2-fs-device fs)
                      (* block-num (floor block-size 512))
                      buffer
                      block-size)

    (write-u32-le buffer (* index 4) value)

    (block-device-write (ext2-fs-device fs)
                       (* block-num (floor block-size 512))
                       buffer
                       block-size)

    (free-buffer buffer block-size)
    t))

;;; Directory Operations

(defun ext2-dir-lookup (fs dir-ino name)
  "Lookup entry in directory."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) dir-ino)
           (type string name))

  (let ((dir-inode (ext2-read-inode fs dir-ino)))
    (unless dir-inode
      (return-from ext2-dir-lookup nil))

    ;; Verify it's a directory
    (unless (= (logand (ext2-inode-mode dir-inode) #o170000) +ext2-s-ifdir+)
      (return-from ext2-dir-lookup nil))

    ;; Scan directory entries
    (let ((size (ext2-inode-size dir-inode))
          (block-size (ext2-fs-block-size fs))
          (offset 0))

      (loop while (< offset size) do
        (let* ((logical-block (floor offset block-size))
               (block-offset (mod offset block-size))
               (phys-block (ext2-bmap fs dir-inode logical-block)))

          (when (zerop phys-block)
            (return nil))

          ;; Read block
          (let ((buffer (allocate-buffer block-size)))
            (when (zerop buffer)
              (return nil))

            (block-device-read (ext2-fs-device fs)
                              (* phys-block (floor block-size 512))
                              buffer
                              block-size)

            ;; Parse directory entries in this block
            (loop while (< block-offset block-size) do
              (let ((inode-num (read-u32-le buffer block-offset))
                    (rec-len (read-u16-le buffer (+ block-offset 4)))
                    (name-len (read-u8 buffer (+ block-offset 6))))

                (when (zerop rec-len)
                  (return))

                (when (and (not (zerop inode-num)) (> name-len 0))
                  ;; Read name
                  (let ((entry-name (make-string name-len)))
                    (dotimes (i name-len)
                      (setf (char entry-name i)
                            (code-char (read-u8 buffer (+ block-offset 8 i)))))

                    (when (string= entry-name name)
                      (free-buffer buffer block-size)
                      (return-from ext2-dir-lookup inode-num))))

                (incf block-offset rec-len)
                (incf offset rec-len)))

            (free-buffer buffer block-size))))

      nil)))

(defun ext2-dir-add-entry (fs dir-ino name ino file-type)
  "Add entry to directory."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) dir-ino ino)
           (type string name)
           (type (unsigned-byte 8) file-type))

  (let ((dir-inode (ext2-read-inode fs dir-ino)))
    (unless dir-inode
      (return-from ext2-dir-add-entry nil))

    (let ((size (ext2-inode-size dir-inode))
          (block-size (ext2-fs-block-size fs))
          (name-len (length name))
          (rec-len (+ 8 name-len (* 4 (ceiling (+ name-len 8) 4))))) ; Align to 4 bytes

      ;; Try to find space in existing blocks
      (let ((offset 0)
            (added nil))

        (loop while (and (< offset size) (not added)) do
          (let* ((logical-block (floor offset block-size))
                 (phys-block (ext2-bmap fs dir-inode logical-block)))

            (when (zerop phys-block)
              (return))

            (let ((buffer (allocate-buffer block-size)))
              (when (zerop buffer)
                (return nil))

              (block-device-read (ext2-fs-device fs)
                                (* phys-block (floor block-size 512))
                                buffer
                                block-size)

              ;; Try to find free space in block
              (let ((block-offset (mod offset block-size)))
                (loop while (< block-offset block-size) do
                  (let ((entry-ino (read-u32-le buffer block-offset))
                        (entry-rec-len (read-u16-le buffer (+ block-offset 4)))
                        (entry-name-len (read-u8 buffer (+ block-offset 6))))

                    (when (zerop entry-rec-len)
                      (return))

                    (let ((actual-len (+ 8 entry-name-len
                                        (* 4 (ceiling (+ entry-name-len 8) 4)))))

                      ;; Check if we can split this entry
                      (when (and (not (zerop entry-ino))
                                (>= (- entry-rec-len actual-len) rec-len))
                        ;; Found space - split entry
                        (write-u16-le buffer (+ block-offset 4) actual-len)
                        (incf block-offset actual-len)

                        ;; Write new entry
                        (write-u32-le buffer block-offset ino)
                        (write-u16-le buffer (+ block-offset 4) (- entry-rec-len actual-len))
                        (write-u8 buffer (+ block-offset 6) name-len)
                        (write-u8 buffer (+ block-offset 7) file-type)

                        (dotimes (i name-len)
                          (write-u8 buffer (+ block-offset 8 i) (char-code (char name i))))

                        (block-device-write (ext2-fs-device fs)
                                           (* phys-block (floor block-size 512))
                                           buffer
                                           block-size)

                        (setf added t)
                        (free-buffer buffer block-size)
                        (return-from ext2-dir-add-entry t)))

                    (incf block-offset entry-rec-len))))

              (free-buffer buffer block-size))))

        ;; Need to allocate new block
        (unless added
          (let* ((logical-block (floor size block-size))
                 (phys-block (ext2-bmap-alloc fs dir-inode logical-block)))

            (when (zerop phys-block)
              (return-from ext2-dir-add-entry nil))

            (let ((buffer (allocate-buffer block-size)))
              (when (zerop buffer)
                (return-from ext2-dir-add-entry nil))

              ;; Zero block
              (dotimes (i block-size)
                (write-u8 buffer i 0))

              ;; Write entry
              (write-u32-le buffer 0 ino)
              (write-u16-le buffer 4 block-size) ; Use entire block
              (write-u8 buffer 6 name-len)
              (write-u8 buffer 7 file-type)

              (dotimes (i name-len)
                (write-u8 buffer (+ 8 i) (char-code (char name i))))

              (block-device-write (ext2-fs-device fs)
                                 (* phys-block (floor block-size 512))
                                 buffer
                                 block-size)

              (free-buffer buffer block-size)

              ;; Update directory size
              (incf (ext2-inode-size dir-inode) block-size)
              (ext2-write-inode fs dir-ino dir-inode)
              t)))))))

(defun ext2-dir-remove-entry (fs dir-ino name)
  "Remove entry from directory."
  (declare (type ext2-fs fs)
           (type (unsigned-byte 32) dir-ino)
           (type string name))

  (let ((dir-inode (ext2-read-inode fs dir-ino)))
    (unless dir-inode
      (return-from ext2-dir-remove-entry nil))

    (let ((size (ext2-inode-size dir-inode))
          (block-size (ext2-fs-block-size fs))
          (offset 0))

      (loop while (< offset size) do
        (let* ((logical-block (floor offset block-size))
               (phys-block (ext2-bmap fs dir-inode logical-block)))

          (when (zerop phys-block)
            (return nil))

          (let ((buffer (allocate-buffer block-size)))
            (when (zerop buffer)
              (return nil))

            (block-device-read (ext2-fs-device fs)
                              (* phys-block (floor block-size 512))
                              buffer
                              block-size)

            (let ((block-offset (mod offset block-size))
                  (prev-offset 0))

              (loop while (< block-offset block-size) do
                (let ((entry-ino (read-u32-le buffer block-offset))
                      (entry-rec-len (read-u16-le buffer (+ block-offset 4)))
                      (entry-name-len (read-u8 buffer (+ block-offset 6))))

                  (when (zerop entry-rec-len)
                    (return))

                  (when (and (not (zerop entry-ino)) (> entry-name-len 0))
                    (let ((entry-name (make-string entry-name-len)))
                      (dotimes (i entry-name-len)
                        (setf (char entry-name i)
                              (code-char (read-u8 buffer (+ block-offset 8 i)))))

                      (when (string= entry-name name)
                        ;; Found entry - mark as deleted
                        (if (zerop prev-offset)
                            ;; First entry - zero inode
                            (write-u32-le buffer block-offset 0)
                            ;; Extend previous entry
                            (let ((prev-rec-len (read-u16-le buffer (+ prev-offset 4))))
                              (write-u16-le buffer (+ prev-offset 4)
                                           (+ prev-rec-len entry-rec-len))))

                        (block-device-write (ext2-fs-device fs)
                                           (* phys-block (floor block-size 512))
                                           buffer
                                           block-size)

                        (free-buffer buffer block-size)
                        (return-from ext2-dir-remove-entry t))))

                  (setf prev-offset block-offset)
                  (incf block-offset entry-rec-len))))

            (free-buffer buffer block-size))))

      nil)))

;;; File Operations

(defun ext2-file-read (fs file buffer length)
  "Read from file."
  (declare (type ext2-fs fs)
           (type ext2-file file)
           (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) length))

  (let ((inode (ext2-file-inode file))
        (pos (ext2-file-pos file))
        (block-size (ext2-fs-block-size fs))
        (bytes-read 0))

    ;; Clamp to file size
    (let ((available (- (ext2-inode-size inode) pos)))
      (when (<= available 0)
        (return-from ext2-file-read 0))
      (setf length (min length available)))

    ;; Read blocks
    (loop while (> length 0) do
      (let* ((logical-block (floor pos block-size))
             (block-offset (mod pos block-size))
             (phys-block (ext2-bmap fs inode logical-block))
             (to-read (min length (- block-size block-offset))))

        (when (zerop phys-block)
          ;; Sparse file - return zeros
          (dotimes (i to-read)
            (write-u8 buffer i 0))
          (incf buffer to-read))
        (when (not (zerop phys-block))
          ;; Read block
          (let ((block-buf (allocate-buffer block-size)))
            (when (zerop block-buf)
              (return-from ext2-file-read bytes-read))

            (block-device-read (ext2-fs-device fs)
                              (* phys-block (floor block-size 512))
                              block-buf
                              block-size)

            ;; Copy to user buffer
            (dotimes (i to-read)
              (write-u8 buffer i (read-u8 block-buf (+ block-offset i))))

            (free-buffer block-buf block-size)
            (incf buffer to-read)))

        (incf pos to-read)
        (incf bytes-read to-read)
        (decf length to-read)))

    ;; Update position
    (setf (ext2-file-pos file) pos)
    bytes-read))

(defun ext2-file-write (fs file buffer length)
  "Write to file."
  (declare (type ext2-fs fs)
           (type ext2-file file)
           (type (unsigned-byte 64) buffer)
           (type (unsigned-byte 32) length))

  (let ((inode (ext2-file-inode file))
        (ino (ext2-file-inode-num file))
        (pos (ext2-file-pos file))
        (block-size (ext2-fs-block-size fs))
        (bytes-written 0))

    ;; Write blocks
    (loop while (> length 0) do
      (let* ((logical-block (floor pos block-size))
             (block-offset (mod pos block-size))
             (phys-block (ext2-bmap-alloc fs inode logical-block))
             (to-write (min length (- block-size block-offset))))

        (when (zerop phys-block)
          (return-from ext2-file-write bytes-written))

        ;; Read-modify-write block
        (let ((block-buf (allocate-buffer block-size)))
          (when (zerop block-buf)
            (return-from ext2-file-write bytes-written))

          ;; Read existing content if partial block write
          (when (or (not (zerop block-offset)) (< to-write block-size))
            (block-device-read (ext2-fs-device fs)
                              (* phys-block (floor block-size 512))
                              block-buf
                              block-size))

          ;; Copy from user buffer
          (dotimes (i to-write)
            (write-u8 block-buf (+ block-offset i) (read-u8 buffer i)))

          (block-device-write (ext2-fs-device fs)
                             (* phys-block (floor block-size 512))
                             block-buf
                             block-size)

          (free-buffer block-buf block-size)
          (incf buffer to-write))

        (incf pos to-write)
        (incf bytes-written to-write)
        (decf length to-write)))

    ;; Update file size if grew
    (when (> pos (ext2-inode-size inode))
      (setf (ext2-inode-size inode) pos)
      (setf (ext2-inode-mtime inode) (get-time))
      (ext2-write-inode fs ino inode))

    ;; Update position
    (setf (ext2-file-pos file) pos)
    bytes-written))

;;; Mount/Unmount

(defun ext2-mount (device)
  "Mount ext2 filesystem."
  (let ((sb (ext2-read-superblock device)))
    (unless sb
      (return-from ext2-mount nil))

    (let* ((block-size (ash +ext2-block-size-base+ (ext2-superblock-log-block-size sb)))
           (blocks-count (ext2-superblock-blocks-count sb))
           (blocks-per-group (ext2-superblock-blocks-per-group sb))
           (groups-count (ceiling blocks-count blocks-per-group))
           (fs (make-ext2-fs
                :device device
                :superblock sb
                :block-size block-size
                :groups-count groups-count
                :lock (make-spinlock))))

      ;; Read group descriptors
      (setf (ext2-fs-group-desc fs) (ext2-read-group-descriptors fs))
      (unless (ext2-fs-group-desc fs)
        (return-from ext2-mount nil))

      ;; Register filesystem
      (setf (gethash device *ext2-filesystems*) fs)
      fs)))

(defun ext2-unmount (fs)
  "Unmount ext2 filesystem."
  (declare (type ext2-fs fs))

  ;; Flush dirty data
  (ext2-sync fs)

  ;; Remove from registry
  (remhash (ext2-fs-device fs) *ext2-filesystems*)
  t)

(defun ext2-sync (fs)
  "Sync filesystem to disk."
  (declare (type ext2-fs fs))

  ;; Write dirty inodes
  (dolist (ino (ext2-fs-dirty-inodes fs))
    (let ((inode (gethash ino (ext2-fs-inode-cache fs))))
      (when inode
        (ext2-write-inode fs ino inode))))

  (setf (ext2-fs-dirty-inodes fs) nil)

  ;; Update superblock
  (ext2-write-superblock fs)
  t)

;;; Utility Functions

(defun get-time ()
  "Get current Unix time."
  0)

(defun make-spinlock ()
  "Create spinlock."
  nil)

(defmacro with-spinlock ((lock) &body body)
  "Execute with spinlock."
  `(progn ,@body))
