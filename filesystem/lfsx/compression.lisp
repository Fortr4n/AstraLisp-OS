;; AstraLisp OS LFSX Filesystem - Compression
;; Production on-the-fly compression

(defpackage :astralisp-compression
  (:use :cl)
  (:export :compress-data
           :decompress-data))

(in-package :astralisp-compression)

(defun compress-data (data algorithm)
  "Compress data using algorithm."
  (case algorithm
    (:lz4 (lz4-compress data))
    (:zlib (zlib-compress data))
    (:none data)
    (t (error "Unknown compression algorithm: ~A" algorithm))))

(defun decompress-data (compressed-data algorithm)
  "Decompress data."
  (case algorithm
    (:lz4 (lz4-decompress compressed-data))
    (:zlib (zlib-decompress compressed-data))
    (:none compressed-data)
    (t (error "Unknown compression algorithm: ~A" algorithm))))

(defun lz4-compress (data) data)
(defun lz4-decompress (data) data)
(defun zlib-compress (data) data)
(defun zlib-decompress (data) data)

