;;;; cbor.lisp

(in-package #:cbor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype raw-data () '(simple-array (unsigned-byte 8) 1))
  (defparameter *optimize* '(optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (defparameter *max-uint64* (1- (expt 2 64)))
  (defparameter *min-uint64* (- (expt 2 64))))

(defconstant +tag-string-datetime+ 0)
(defconstant +tag-float-datetime+ 1)
(defconstant +tag-positive-bignum+ 2)
(defconstant +tag-negative-bignum+ 3)

(defconstant +tag-ratio+ 30)
(defconstant +tag-complex+ 43000)

(defconstant +tag-stringref+ 25)
(defconstant +tag-stringref-namespace+ 256)

(defconstant +tag-embedded-cbor+ 55799)

;; our tags
(defconstant +tag-symbol+ 2000)
(defconstant +tag-cons+ 2001)
(defconstant +tag-list+ 2002)
(defconstant +tag-character+ 2003)
(defconstant +tag-object+ 2004)
(defconstant +tag-structure+ 2005)

(defparameter *strict* t
  "Strict mode: use the custom tags above for precise serialization of
various Common Lisp types that are not defined in the core CBOR spec.
A decoder must support these types for proper de-serialization.")

(defparameter *use-stringrefs* t
  "Wether to use the stringrefs extension for encoding
(http://cbor.schmorp.de/stringref). Decoding always supports
it, if it encounters the tag.")

(defparameter *jsown-semantics* nil
  "Bind this to T if you want encode/decode to use JSOWN's conventions.")

(defparameter *array-format* :array
  "Bind this to :list if you want arrays to decode as lists. Note that
the empty array will be NIL in this case.")

(defparameter *dictionary-format* :hash
  "How to decode dictionaries. Supports :HASH, :ALIST or :PLIST. When
:HASH (the default), dictionaries are decoded into hash tables using
#'EQ as test when *string-to-symbol* is bound, or #'EQUAL otherwise.")

(declaim (type (or null (function (symbol) string)) *symbol-to-string*))
(defparameter *symbol-to-string*
  (lambda (symbol)
    (declare #.*optimize*)
    (string-downcase
     (substitute #\_ #\- (symbol-name symbol))))
  "A function that converts symbols to strings. If bound to NIL while
encoding, `symbol-name' will be used instead.")

(defparameter *symbols-package* (find-package "KEYWORD")
  "Package to intern symbols into. Used by the default implementation of
`*string-to-symbol*'.")

(declaim (type (or null (function (string) t)) *string-to-symbol*))
(defparameter *string-to-symbol*
  (lambda (string)
    (declare #.*optimize*)
    (intern (string-upcase
             (substitute #\- #\_ string))
            *symbols-package*))
  "A function that converts strings to symbols. Applied for dictionary
name and keys. If bound to NIL while decoding, strings will be left
alone.")

(declaim (type (or null (function ((integer 0 #.*max-uint64*) t) t)) *custom-tag-reader*))
(defparameter *custom-tag-reader* nil
  "Custom tag reader function, for tags not supported by
`read-tagged'. Receives tag number and the value.")

(declaim (inline encode-float16 decode-float16))
(ieee-floats:make-float-converters encode-float16 decode-float16 5 10 nil)
(import '(ieee-floats:encode-float32 ieee-floats:decode-float32
          ieee-floats:encode-float64 ieee-floats:decode-float64))

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
        (if (< thing (length *stringref-cache*))
            (aref *stringref-cache* thing)
            (error "Index out of bounds in stringref cache (~A)" thing)))))
