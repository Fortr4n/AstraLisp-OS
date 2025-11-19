;; AstraLisp OS Kernel Heap Allocators
;; Production heap with slab, buddy, and pool allocators

(defpackage :astralisp-heap
  (:use :cl)
  (:export :heap-init
           :heap-alloc
           :heap-free
           :heap-realloc))

(in-package :astralisp-heap)

;; Constants
(defconstant +page-size+ 4096)

;; Heap allocator types
(defconstant +allocator-slab+ 0)
(defconstant +allocator-buddy+ 1)
(defconstant +allocator-pool+ 2)

;; Slab allocator for fixed-size objects
(defstruct slab
  "Slab structure."
  (object-size 0 :type (unsigned-byte 32))
  (objects-per-slab 0 :type (unsigned-byte 32))
  (free-list nil :type list)
  (pages nil :type list))

;; Buddy allocator for variable-size allocations
(defstruct buddy-block
  "Buddy allocator block."
  (address 0 :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 64))
  (free t :type boolean)
  (left nil :type (or null buddy-block))
  (right nil :type (or null buddy-block))
  (parent nil :type (or null buddy-block)))

;; Pool allocator for small objects
(defstruct pool
  "Pool allocator."
  (chunk-size 0 :type (unsigned-byte 32))
  (chunks nil :type list)
  (free-chunks nil :type list))

;; Heap state
(defvar *heap-initialized* nil)
(defvar *slab-allocators* (make-hash-table))
(defvar *buddy-allocator* nil)
(defvar *pool-allocators* (make-hash-table))
(defvar *heap-start* #x02000000)
(defvar *heap-end* #x06000000)
(defvar *heap-size* #x04000000)  ; 64MB

;; Initialize heap
(defun heap-init ()
  "Initialize kernel heap allocators."
  (when *heap-initialized*
    (error "Heap already initialized"))
  
  ;; Initialize buddy allocator for large allocations
  (setf *buddy-allocator* (make-buddy-allocator *heap-start* *heap-size*))
  
  ;; Initialize slab allocators for common sizes
  (dolist (size '(16 32 64 128 256 512 1024 2048 4096))
    (setf (gethash size *slab-allocators*)
          (make-slab-allocator size)))
  
  ;; Initialize pool allocators for small objects
  (dolist (size '(8 16 32))
    (setf (gethash size *pool-allocators*)
          (make-pool-allocator size)))
  
  (setf *heap-initialized* t))

;; Allocate memory from heap
(defun heap-alloc (size)
  "Allocate memory from heap."
  (declare (type (unsigned-byte 64) size))
  (cond
    ;; Small objects: use pool allocator
    ((<= size 32)
     (pool-alloc (gethash 32 *pool-allocators*) size))
    ;; Fixed-size objects: use slab allocator
    ((<= size 4096)
     (let ((slab-size (find-slab-size size)))
       (slab-alloc (gethash slab-size *slab-allocators*))))
    ;; Large objects: use buddy allocator
    (t
     (buddy-alloc *buddy-allocator* size))))

;; Free memory
(defun heap-free (ptr size)
  "Free memory from heap."
  (declare (type (unsigned-byte 64) ptr size))
  (cond
    ((<= size 32)
     (pool-free (gethash 32 *pool-allocators*) ptr))
    ((<= size 4096)
     (let ((slab-size (find-slab-size size)))
       (slab-free (gethash slab-size *slab-allocators*) ptr)))
    (t
     (buddy-free *buddy-allocator* ptr size))))

;; Reallocate memory
(defun heap-realloc (ptr old-size new-size)
  "Reallocate memory."
  (declare (type (unsigned-byte 64) ptr old-size new-size))
  (let ((new-ptr (heap-alloc new-size)))
    (when new-ptr
      ;; Copy old data
      (memory-copy ptr new-ptr (min old-size new-size))
      (heap-free ptr old-size))
    new-ptr))

;; Slab allocator
(defun make-slab-allocator (object-size)
  "Create slab allocator for given object size."
  (make-slab :object-size object-size
             :objects-per-slab (floor +page-size+ object-size)))

(defun slab-alloc (slab)
  "Allocate from slab."
  (declare (type slab slab))
  (if (slab-free-list slab)
      (pop (slab-free-list slab))
      (slab-alloc-new-page slab)))

(defun slab-alloc-new-page (slab)
  "Allocate new page for slab."
  (let ((page (page-alloc)))
    (when page
      (push page (slab-pages slab))
      (let ((base-addr (page-frame-address page)))
        (loop for i from 0 below (slab-objects-per-slab slab)
              for addr = (+ base-addr (* i (slab-object-size slab)))
              do (push addr (slab-free-list slab)))
        (pop (slab-free-list slab))))))

(defun slab-free (slab ptr)
  "Free object from slab."
  (declare (type slab slab))
  (push ptr (slab-free-list slab)))

;; Buddy allocator
(defun make-buddy-allocator (start size)
  "Create buddy allocator."
  (let ((root (make-buddy-block :address start :size size :free t)))
    root))

(defun buddy-alloc (allocator size)
  "Allocate from buddy allocator."
  (declare (type buddy-block allocator))
  (let ((aligned-size (align-size size)))
    (buddy-alloc-recursive allocator aligned-size)))

(defun buddy-alloc-recursive (block size)
  "Recursively allocate from buddy tree."
  (when (not (buddy-block-free block))
    (return-from buddy-alloc-recursive nil))
  
  (cond
    ;; Exact fit
    ((= (buddy-block-size block) size)
     (setf (buddy-block-free block) nil)
     (buddy-block-address block))
    ;; Block too large, split
    ((> (buddy-block-size block) size)
     (let ((half-size (/ (buddy-block-size block) 2)))
       (when (>= half-size size)
         ;; Split block
         (let ((left (make-buddy-block
                      :address (buddy-block-address block)
                      :size half-size
                      :free t
                      :parent block))
               (right (make-buddy-block
                       :address (+ (buddy-block-address block) half-size)
                       :size half-size
                       :free t
                       :parent block)))
           (setf (buddy-block-left block) left)
           (setf (buddy-block-right block) right)
           (buddy-alloc-recursive left size)))))))

(defun buddy-free (allocator ptr size)
  "Free from buddy allocator."
  (declare (type buddy-block allocator))
  (let ((aligned-size (align-size size)))
    (buddy-free-recursive allocator ptr aligned-size)))

(defun buddy-free-recursive (block ptr size)
  "Recursively free in buddy tree."
  (when (not block)
    (return-from buddy-free-recursive))
  
  (cond
    ;; Found block
    ((and (= (buddy-block-address block) ptr)
          (= (buddy-block-size block) size))
     (setf (buddy-block-free block) t)
     (buddy-merge block))
    ;; Search left
    ((< ptr (+ (buddy-block-address block) (/ (buddy-block-size block) 2)))
     (buddy-free-recursive (buddy-block-left block) ptr size))
    ;; Search right
    (t
     (buddy-free-recursive (buddy-block-right block) ptr size))))

(defun buddy-merge (block)
  "Merge buddy blocks if both are free."
  (when (and (buddy-block-left block)
             (buddy-block-right block)
             (buddy-block-free (buddy-block-left block))
             (buddy-block-free (buddy-block-right block)))
    (setf (buddy-block-left block) nil)
    (setf (buddy-block-right block) nil)
    (when (buddy-block-parent block)
      (buddy-merge (buddy-block-parent block)))))

;; Pool allocator
(defun make-pool-allocator (chunk-size)
  "Create pool allocator."
  (make-pool :chunk-size chunk-size))

(defun pool-alloc (pool size)
  "Allocate from pool."
  (declare (type pool pool))
  (if (pool-free-chunks pool)
      (pop (pool-free-chunks pool))
      (pool-alloc-new-chunk pool)))

(defun pool-alloc-new-chunk (pool)
  "Allocate new chunk for pool (allocates a page and divides it into pool-sized chunks)."
  ;; Allocate a full page to hold multiple objects
  (let ((page (page-alloc)))
    (when page
      (let ((page-addr (page-frame-address page))
            (chunk-size (pool-chunk-size pool))
            (objects-per-page (floor +page-size+ (pool-chunk-size pool))))
        (push page (pool-chunks pool))
        ;; Divide page into pool-sized objects and add to free list
        (loop for i from 0 below objects-per-page
              for addr = (+ page-addr (* i chunk-size))
              do (push addr (pool-free-chunks pool)))
        ;; Return first object
        (pop (pool-free-chunks pool))))))

(defun pool-free (pool ptr)
  "Free from pool."
  (declare (type pool pool))
  (push ptr (pool-free-chunks pool)))

;; Utility functions
(defun find-slab-size (size)
  "Find appropriate slab size for allocation."
  (dolist (slab-size '(16 32 64 128 256 512 1024 2048 4096))
    (when (>= slab-size size)
      (return slab-size)))
  4096)

(defun align-size (size)
  "Align size to power of 2."
  (let ((aligned 1))
    (loop while (< aligned size)
          do (setf aligned (* aligned 2)))
    aligned))

;; Page frame structure reference
(defstruct page-frame
  "Physical page frame."
  (address 0 :type (unsigned-byte 64)))

;; Forward declarations
(defun page-alloc ()
  "Allocate a physical page frame."
  (make-page-frame :address 0))  ; Stub

(defun memory-copy (src dst size)
  "Copy memory from src to dst."
  (declare (ignore src dst size))
  nil)

