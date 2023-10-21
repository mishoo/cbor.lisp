;;;; package.lisp

(defpackage #:cbor
  (:use #:cl)
  (:export #:encode #:decode
           #:*jsown-semantics*
           #:*array-format*
           #:*dictionary-format*
           #:*symbol-to-string*
           #:*symbols-package*
           #:*string-to-symbol*
           #:*custom-tag-reader*))
