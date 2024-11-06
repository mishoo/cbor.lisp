(in-package #:cbor)

;;; stringref utilities
;;; http://cbor.schmorp.de/stringref

(defparameter *stringref-cache* nil)

(defmacro with-stringrefs (encoding? &body body)
  `(let ((*stringref-cache*
           ,(if encoding?
                '(make-hash-table :test #'equal)
                '(make-array 0 :adjustable t :fill-pointer 0))))
     ,@body))

(defun stringref-should-cache (len)
  (declare (type fixnum len)
           #.*optimize*)
  (when *stringref-cache*
    (let ((index (if (hash-table-p *stringref-cache*)
                     (hash-table-count *stringref-cache*)
                     (length *stringref-cache*))))
      (when (or (>= len 11)
                (and (<=     0 index         23) (>= len 3))
                (and (<=    24 index        255) (>= len 4))
                (and (<=   256 index      65535) (>= len 5))
                (and (<= 65536 index 4294967295) (>= len 7)))
        index))))

(defun stringref-assign (str &optional
                               (len (trivial-utf-8:utf-8-byte-length str)))
  (declare (type (or string (vector (unsigned-byte 8))) str)
           (type fixnum len)
           #.*optimize*)
  (let ((index (stringref-should-cache len)))
    (when index
      (if (hash-table-p *stringref-cache*)
          (setf (gethash str *stringref-cache*) index)
          (vector-push-extend str *stringref-cache*)))))

(defun stringref-get (thing)
  (when *stringref-cache*
    (if (hash-table-p *stringref-cache*)
        (gethash thing *stringref-cache*)
        (when (< thing (length *stringref-cache*))
          (aref *stringref-cache* thing)))))
