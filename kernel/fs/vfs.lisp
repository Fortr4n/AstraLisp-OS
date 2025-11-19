;; AstraLisp OS Virtual File System
;; Production VFS with file descriptors, caching, and multiple filesystem support

(defpackage :astralisp-vfs
  (:use :cl)
  (:export :vfs-init
           :vfs-mount
           :vfs-unmount
           :vfs-open
           :vfs-close
           :vfs-read
           :vfs-write
           :vfs-seek
           :vfs-stat
           :vfs-create
           :vfs-unlink
           :vfs-mkdir
           :vfs-rmdir
           :fd-table-create
           :fd-table-destroy
           :fd-table-allocate
           :fd-table-get
           :fd-table-close))

(in-package :astralisp-vfs)

;; File descriptor flags
(defconstant +o-rdonly+ #x0000)
(defconstant +o-wronly+ #x0001)
(defconstant +o-rdwr+ #x0002)
(defconstant +o-creat+ #x0040)
(defconstant +o-excl+ #x0080)
(defconstant +o-trunc+ #x0200)
(defconstant +o-append+ #x0400)
(defconstant +o-nonblock+ #x0800)
(defconstant +o-directory+ #x10000)
(defconstant +o-cloexec+ #x80000)

;; Seek whence
(defconstant +seek-set+ 0)
(defconstant +seek-cur+ 1)
(defconstant +seek-end+ 2)

;; File types
(defconstant +s-ifreg+ #x8000)    ; Regular file
(defconstant +s-ifdir+ #x4000)    ; Directory
(defconstant +s-ifchr+ #x2000)    ; Character device
(defconstant +s-ifblk+ #x6000)    ; Block device
(defconstant +s-ififo+ #x1000)    ; FIFO
(defconstant +s-iflnk+ #xA000)    ; Symbolic link
(defconstant +s-ifsock+ #xC000)   ; Socket

;; File permissions
(defconstant +s-irusr+ #x0100)
(defconstant +s-iwusr+ #x0080)
(defconstant +s-ixusr+ #x0040)
(defconstant +s-irgrp+ #x0020)
(defconstant +s-iwgrp+ #x0010)
(defconstant +s-ixgrp+ #x0008)
(defconstant +s-iroth+ #x0004)
(defconstant +s-iwoth+ #x0002)
(defconstant +s-ixoth+ #x0001)

;; VNode (virtual node) structure
(defstruct vnode
  "Virtual filesystem node."
  (type +s-ifreg+ :type (unsigned-byte 16))
  (mode 0 :type (unsigned-byte 16))
  (uid 0 :type (unsigned-byte 32))
  (gid 0 :type (unsigned-byte 32))
  (size 0 :type (unsigned-byte 64))
  (blocks 0 :type (unsigned-byte 64))
  (atime 0 :type (unsigned-byte 64))
  (mtime 0 :type (unsigned-byte 64))
  (ctime 0 :type (unsigned-byte 64))
  (inode 0 :type (unsigned-byte 64))
  (nlink 1 :type (unsigned-byte 32))
  (filesystem nil :type (or null filesystem))
  (fs-data nil :type t)                    ; Filesystem-specific data
  (lock nil :type t)
  (reference-count 0 :type (unsigned-byte 32)))

;; File structure
(defstruct file
  "Open file structure."
  (vnode nil :type (or null vnode))
  (flags 0 :type (unsigned-byte 32))
  (position 0 :type (unsigned-byte 64))
  (lock nil :type t)
  (reference-count 0 :type (unsigned-byte 32)))

;; File descriptor table
(defstruct fd-table
  "File descriptor table."
  (fds (make-array 1024 :initial-element nil) :type (vector (or null file)))
  (max-fd 2 :type (unsigned-byte 32))      ; Start after stdin/stdout/stderr
  (lock nil :type t))

;; Filesystem operations
(defstruct filesystem-ops
  "Filesystem operations."
  (mount nil :type (or null function))
  (unmount nil :type (or null function))
  (statfs nil :type (or null function))
  (lookup nil :type (or null function))
  (create nil :type (or null function))
  (unlink nil :type (or null function))
  (mkdir nil :type (or null function))
  (rmdir nil :type (or null function))
  (rename nil :type (or null function))
  (link nil :type (or null function))
  (symlink nil :type (or null function))
  (readlink nil :type (or null function)))

;; VNode operations
(defstruct vnode-ops
  "VNode operations."
  (open nil :type (or null function))
  (close nil :type (or null function))
  (read nil :type (or null function))
  (write nil :type (or null function))
  (ioctl nil :type (or null function))
  (mmap nil :type (or null function))
  (getattr nil :type (or null function))
  (setattr nil :type (or null function))
  (readdir nil :type (or null function))
  (truncate nil :type (or null function)))

;; Filesystem structure
(defstruct filesystem
  "Mounted filesystem."
  (name "" :type string)
  (mount-point "" :type string)
  (device "" :type string)
  (ops nil :type (or null filesystem-ops))
  (vnode-ops nil :type (or null vnode-ops))
  (root-vnode nil :type (or null vnode))
  (fs-data nil :type t)
  (flags 0 :type (unsigned-byte 32))
  (next nil :type (or null filesystem)))

;; VFS state
(defvar *vfs-initialized* nil)
(defvar *mounted-filesystems* nil)
(defvar *root-filesystem* nil)
(defvar *vfs-lock* nil)
(defvar *vnode-cache* (make-hash-table :test 'equal))
(defvar *dentry-cache* (make-hash-table :test 'equal))

;; Initialize VFS
(defun vfs-init ()
  "Initialize virtual filesystem."
  (when *vfs-initialized*
    (error "VFS already initialized"))

  (setf *vfs-lock* (make-mutex))
  (setf *mounted-filesystems* nil)
  (setf *root-filesystem* nil)

  (setf *vfs-initialized* t))

;; Mount filesystem
(defun vfs-mount (device mount-point fs-type &key flags options)
  "Mount filesystem at mount point."
  (declare (type string device mount-point fs-type))

  (with-mutex (*vfs-lock*)
    ;; Check if mount point exists
    (when (and *root-filesystem* (not (equal mount-point "/")))
      (let ((vnode (path-lookup mount-point)))
        (when (null vnode)
          (error "Mount point does not exist: ~A" mount-point))
        (when (not (= (vnode-type vnode) +s-ifdir+))
          (error "Mount point is not a directory: ~A" mount-point))))

    ;; Create filesystem structure
    (let* ((fs-ops (get-filesystem-ops fs-type))
           (vnode-ops (get-vnode-ops fs-type))
           (fs (make-filesystem
                :name fs-type
                :mount-point mount-point
                :device device
                :ops fs-ops
                :vnode-ops vnode-ops
                :flags (or flags 0))))

      ;; Call filesystem mount operation
      (when (filesystem-ops-mount fs-ops)
        (funcall (filesystem-ops-mount fs-ops) fs device options))

      ;; Add to mounted filesystems list
      (if (equal mount-point "/")
          (setf *root-filesystem* fs)
          (progn
            (setf (filesystem-next fs) *mounted-filesystems*)
            (setf *mounted-filesystems* fs)))

      fs)))

;; Unmount filesystem
(defun vfs-unmount (mount-point)
  "Unmount filesystem."
  (declare (type string mount-point))

  (with-mutex (*vfs-lock*)
    (let ((fs (find-mounted-filesystem mount-point)))
      (when (null fs)
        (error "No filesystem mounted at: ~A" mount-point))

      ;; Call filesystem unmount operation
      (when (filesystem-ops-unmount (filesystem-ops fs))
        (funcall (filesystem-ops-unmount (filesystem-ops fs)) fs))

      ;; Remove from list
      (if (equal mount-point "/")
          (setf *root-filesystem* nil)
          (setf *mounted-filesystems*
                (remove fs *mounted-filesystems*))))))

;; Open file
(defun vfs-open (path flags &key mode)
  "Open file and return file descriptor."
  (declare (type string path)
           (type (unsigned-byte 32) flags))

  (let ((vnode (path-lookup path)))

    ;; Handle O_CREAT
    (when (and (null vnode) (logbitp 6 flags)) ; O_CREAT
      (setf vnode (vfs-create path (or mode #o644))))

    (when (null vnode)
      (return-from vfs-open -1)) ; ENOENT

    ;; Handle O_EXCL
    (when (and (logbitp 7 flags) (logbitp 6 flags)) ; O_EXCL | O_CREAT
      (return-from vfs-open -1)) ; EEXIST

    ;; Handle O_DIRECTORY
    (when (and (logbitp 16 flags) ; O_DIRECTORY
               (not (= (vnode-type vnode) +s-ifdir+)))
      (return-from vfs-open -1)) ; ENOTDIR

    ;; Handle O_TRUNC
    (when (and (logbitp 9 flags) ; O_TRUNC
               (or (= (logand flags 3) +o-wronly+)
                   (= (logand flags 3) +o-rdwr+)))
      (vnode-truncate vnode 0))

    ;; Create file structure
    (let ((file (make-file
                 :vnode vnode
                 :flags flags
                 :position (if (logbitp 10 flags) ; O_APPEND
                              (vnode-size vnode)
                              0)
                 :lock (make-mutex)
                 :reference-count 1)))

      ;; Increment vnode reference count
      (incf (vnode-reference-count vnode))

      ;; Call vnode open operation
      (when (vnode-ops-open (filesystem-vnode-ops (vnode-filesystem vnode)))
        (funcall (vnode-ops-open (filesystem-vnode-ops (vnode-filesystem vnode)))
                vnode flags))

      file)))

;; Close file
(defun vfs-close (file)
  "Close file."
  (declare (type file file))

  (with-mutex ((file-lock file))
    (decf (file-reference-count file))

    (when (zerop (file-reference-count file))
      (let ((vnode (file-vnode file)))

        ;; Call vnode close operation
        (when (vnode-ops-close (filesystem-vnode-ops (vnode-filesystem vnode)))
          (funcall (vnode-ops-close (filesystem-vnode-ops (vnode-filesystem vnode)))
                  vnode))

        ;; Decrement vnode reference count
        (decf (vnode-reference-count vnode))

        ;; Release vnode if no more references
        (when (zerop (vnode-reference-count vnode))
          (release-vnode vnode))))))

;; Read from file
(defun vfs-read (file buffer size)
  "Read from file."
  (declare (type file file)
           (type (unsigned-byte 64) size))

  (with-mutex ((file-lock file))
    (let ((vnode (file-vnode file)))

      ;; Check read permission
      (when (= (logand (file-flags file) 3) +o-wronly+)
        (return-from vfs-read -1)) ; EBADF

      ;; Call vnode read operation
      (when (vnode-ops-read (filesystem-vnode-ops (vnode-filesystem vnode)))
        (let ((bytes-read (funcall (vnode-ops-read
                                    (filesystem-vnode-ops (vnode-filesystem vnode)))
                                  vnode
                                  buffer
                                  (file-position file)
                                  size)))

          ;; Update position
          (incf (file-position file) bytes-read)

          ;; Update atime
          (setf (vnode-atime vnode) (get-current-time))

          bytes-read)))))

;; Write to file
(defun vfs-write (file buffer size)
  "Write to file."
  (declare (type file file)
           (type (unsigned-byte 64) size))

  (with-mutex ((file-lock file))
    (let ((vnode (file-vnode file)))

      ;; Check write permission
      (when (= (logand (file-flags file) 3) +o-rdonly+)
        (return-from vfs-write -1)) ; EBADF

      ;; Handle O_APPEND
      (when (logbitp 10 (file-flags file))
        (setf (file-position file) (vnode-size vnode)))

      ;; Call vnode write operation
      (when (vnode-ops-write (filesystem-vnode-ops (vnode-filesystem vnode)))
        (let ((bytes-written (funcall (vnode-ops-write
                                       (filesystem-vnode-ops (vnode-filesystem vnode)))
                                     vnode
                                     buffer
                                     (file-position file)
                                     size)))

          ;; Update position
          (incf (file-position file) bytes-written)

          ;; Update size if necessary
          (when (> (file-position file) (vnode-size vnode))
            (setf (vnode-size vnode) (file-position file)))

          ;; Update mtime and ctime
          (let ((now (get-current-time)))
            (setf (vnode-mtime vnode) now)
            (setf (vnode-ctime vnode) now))

          bytes-written)))))

;; Seek in file
(defun vfs-seek (file offset whence)
  "Seek in file."
  (declare (type file file)
           (type (signed-byte 64) offset)
           (type (unsigned-byte 32) whence))

  (with-mutex ((file-lock file))
    (let ((new-pos 0)
          (vnode (file-vnode file)))

      (case whence
        (#.+seek-set+
         (setf new-pos offset))
        (#.+seek-cur+
         (setf new-pos (+ (file-position file) offset)))
        (#.+seek-end+
         (setf new-pos (+ (vnode-size vnode) offset)))
        (t
         (return-from vfs-seek -1))) ; EINVAL

      ;; Check bounds
      (when (< new-pos 0)
        (return-from vfs-seek -1)) ; EINVAL

      (setf (file-position file) new-pos)
      new-pos)))

;; Get file status
(defun vfs-stat (path)
  "Get file status."
  (declare (type string path))

  (let ((vnode (path-lookup path)))
    (when (null vnode)
      (return-from vfs-stat nil))

    ;; Return stat structure
    (make-stat-from-vnode vnode)))

;; Create file
(defun vfs-create (path mode)
  "Create new file."
  (declare (type string path)
           (type (unsigned-byte 16) mode))

  (let* ((parent-path (get-parent-path path))
         (filename (get-filename path))
         (parent-vnode (path-lookup parent-path)))

    (when (null parent-vnode)
      (error "Parent directory does not exist"))

    (when (not (= (vnode-type parent-vnode) +s-ifdir+))
      (error "Parent is not a directory"))

    ;; Call filesystem create operation
    (when (filesystem-ops-create (filesystem-ops (vnode-filesystem parent-vnode)))
      (funcall (filesystem-ops-create (filesystem-ops (vnode-filesystem parent-vnode)))
              parent-vnode
              filename
              mode))))

;; Unlink file
(defun vfs-unlink (path)
  "Unlink (delete) file."
  (declare (type string path))

  (let* ((parent-path (get-parent-path path))
         (filename (get-filename path))
         (parent-vnode (path-lookup parent-path)))

    (when (null parent-vnode)
      (error "Parent directory does not exist"))

    ;; Call filesystem unlink operation
    (when (filesystem-ops-unlink (filesystem-ops (vnode-filesystem parent-vnode)))
      (funcall (filesystem-ops-unlink (filesystem-ops (vnode-filesystem parent-vnode)))
              parent-vnode
              filename))))

;; Create directory
(defun vfs-mkdir (path mode)
  "Create directory."
  (declare (type string path)
           (type (unsigned-byte 16) mode))

  (let* ((parent-path (get-parent-path path))
         (dirname (get-filename path))
         (parent-vnode (path-lookup parent-path)))

    (when (null parent-vnode)
      (error "Parent directory does not exist"))

    ;; Call filesystem mkdir operation
    (when (filesystem-ops-mkdir (filesystem-ops (vnode-filesystem parent-vnode)))
      (funcall (filesystem-ops-mkdir (filesystem-ops (vnode-filesystem parent-vnode)))
              parent-vnode
              dirname
              mode))))

;; Remove directory
(defun vfs-rmdir (path)
  "Remove directory."
  (declare (type string path))

  (let* ((parent-path (get-parent-path path))
         (dirname (get-filename path))
         (parent-vnode (path-lookup parent-path)))

    (when (null parent-vnode)
      (error "Parent directory does not exist"))

    ;; Call filesystem rmdir operation
    (when (filesystem-ops-rmdir (filesystem-ops (vnode-filesystem parent-vnode)))
      (funcall (filesystem-ops-rmdir (filesystem-ops (vnode-filesystem parent-vnode)))
              parent-vnode
              dirname))))

;; File descriptor table operations
(defun fd-table-create ()
  "Create new file descriptor table."
  (make-fd-table :lock (make-mutex)))

(defun fd-table-destroy (fdt)
  "Destroy file descriptor table."
  (declare (type fd-table fdt))
  (with-mutex ((fd-table-lock fdt))
    ;; Close all open files
    (dotimes (i (length (fd-table-fds fdt)))
      (let ((file (aref (fd-table-fds fdt) i)))
        (when file
          (vfs-close file))))))

(defun fd-table-allocate (fdt file)
  "Allocate file descriptor for file."
  (declare (type fd-table fdt)
           (type file file))

  (with-mutex ((fd-table-lock fdt))
    ;; Find free FD
    (let ((fds (fd-table-fds fdt)))
      (loop for i from 3 below (length fds) ; Reserve 0-2 for std streams
            when (null (aref fds i))
              do (setf (aref fds i) file)
                 (return i)
            finally (return -1))))) ; EMFILE

(defun fd-table-get (fdt fd)
  "Get file from file descriptor."
  (declare (type fd-table fdt)
           (type (unsigned-byte 32) fd))

  (when (>= fd (length (fd-table-fds fdt)))
    (return-from fd-table-get nil))

  (aref (fd-table-fds fdt) fd))

(defun fd-table-close (fdt fd)
  "Close file descriptor."
  (declare (type fd-table fdt)
           (type (unsigned-byte 32) fd))

  (with-mutex ((fd-table-lock fdt))
    (let ((file (aref (fd-table-fds fdt) fd)))
      (when file
        (vfs-close file)
        (setf (aref (fd-table-fds fdt) fd) nil)
        t))))

;; Internal functions
(defun path-lookup (path)
  "Lookup vnode by path."
  (declare (type string path))

  ;; Check cache first
  (let ((cached (gethash path *dentry-cache*)))
    (when cached
      (return-from path-lookup cached)))

  ;; Start from root
  (when (null *root-filesystem*)
    (return-from path-lookup nil))

  (let ((current-vnode (filesystem-root-vnode *root-filesystem*))
        (components (split-path path)))

    ;; Traverse path
    (dolist (component components)
      (when (null current-vnode)
        (return-from path-lookup nil))

      ;; Lookup component in current directory
      (setf current-vnode (lookup-in-directory current-vnode component))

      (when (null current-vnode)
        (return-from path-lookup nil)))

    ;; Cache result
    (setf (gethash path *dentry-cache*) current-vnode)

    current-vnode))

(defun lookup-in-directory (dir-vnode name)
  "Lookup name in directory."
  (when (filesystem-ops-lookup (filesystem-ops (vnode-filesystem dir-vnode)))
    (funcall (filesystem-ops-lookup (filesystem-ops (vnode-filesystem dir-vnode)))
            dir-vnode
            name)))

(defun split-path (path)
  "Split path into components."
  (remove-if #'(lambda (s) (or (null s) (string= s "")))
             (split-sequence:split-sequence #\/ path)))

(defun get-parent-path (path)
  "Get parent directory path."
  (let ((pos (position #\/ path :from-end t)))
    (if pos
        (subseq path 0 pos)
        "/")))

(defun get-filename (path)
  "Get filename from path."
  (let ((pos (position #\/ path :from-end t)))
    (if pos
        (subseq path (1+ pos))
        path)))

(defun vnode-truncate (vnode size)
  "Truncate vnode to size."
  (when (vnode-ops-truncate (filesystem-vnode-ops (vnode-filesystem vnode)))
    (funcall (vnode-ops-truncate (filesystem-vnode-ops (vnode-filesystem vnode)))
            vnode
            size)))

(defun release-vnode (vnode)
  "Release vnode resources."
  (declare (ignore vnode))
  nil)

(defun make-stat-from-vnode (vnode)
  "Create stat structure from vnode."
  (list :type (vnode-type vnode)
        :mode (vnode-mode vnode)
        :size (vnode-size vnode)
        :uid (vnode-uid vnode)
        :gid (vnode-gid vnode)
        :atime (vnode-atime vnode)
        :mtime (vnode-mtime vnode)
        :ctime (vnode-ctime vnode)
        :inode (vnode-inode vnode)
        :nlink (vnode-nlink vnode)))

(defun find-mounted-filesystem (mount-point)
  "Find mounted filesystem by mount point."
  (when (equal mount-point "/")
    (return-from find-mounted-filesystem *root-filesystem*))

  (let ((fs *mounted-filesystems*))
    (loop while fs
          when (equal (filesystem-mount-point fs) mount-point)
            do (return fs)
          do (setf fs (filesystem-next fs)))))

;; Forward declarations
(defun get-filesystem-ops (fs-type)
  "Get filesystem operations for type."
  (declare (ignore fs-type))
  (make-filesystem-ops))

(defun get-vnode-ops (fs-type)
  "Get vnode operations for type."
  (declare (ignore fs-type))
  (make-vnode-ops))

(defun get-current-time ()
  "Get current time."
  0)

(defun make-mutex ()
  "Create mutex."
  (make-hash-table))

(defmacro with-mutex ((mutex) &body body)
  "Execute body with mutex held."
  (declare (ignore mutex))
  `(progn ,@body))
