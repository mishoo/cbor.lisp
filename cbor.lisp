;;;; cbor.lisp

(in-package #:cbor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype raw-data () '(simple-array (unsigned-byte 8) 1))
  (defparameter *optimize* '(optimize speed (safety 1) (space 0) (debug 0)))
  ;; (defparameter *optimize* '(optimize (speed 0) (safety 1) (space 0) (debug 3)))
  (defparameter *max-uint64* (1- (expt 2 64)))
  (defparameter *min-uint64* (- (expt 2 64))))

(defconstant +tag-string-datetime+ 0)
(defconstant +tag-float-datetime+ 1)
(defconstant +tag-positive-bignum+ 2)
(defconstant +tag-negative-bignum+ 3)
(defconstant +tag-decimal-fraction+ 4)
(defconstant +tag-bigfloat+ 5)
(defconstant +tag-encoded-cbor+ 24)

(defconstant +tag-ratio+ 30)
(defconstant +tag-complex+ 43000)

(defconstant +tag-stringref+ 25)
(defconstant +tag-shareable+ 28)
(defconstant +tag-sharedref+ 29)
(defconstant +tag-stringref-namespace+ 256)

(defconstant +tag-cbor+ 55799)

;; our tags
(defconstant +tag-symbol+ 2000)
(defconstant +tag-cons+ 2001)
(defconstant +tag-list+ 2002)
(defconstant +tag-character+ 2003)
(defconstant +tag-object+ 2004)

(defparameter *strict* t
  "Strict mode: use the custom tags above for precise serialization of
various Common Lisp types that are not defined in the core CBOR spec.
A decoder must support these types for proper de-serialization.")

(defparameter *use-stringrefs* t
  "Wether to use the stringrefs extension for encoding
(http://cbor.schmorp.de/stringref). Decoding always supports
it, if it encounters the tag.")

(defparameter *use-sharedrefs* t
  "Wether to use the shared value extension for
encoding (http://cbor.schmorp.de/value-sharing). Decoding always
supports it if it encounters the tags.")

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
