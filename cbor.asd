;;;; cbor.asd

(asdf:defsystem #:cbor
  :description "CBOR encoder/decoder"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-utf-8
               #:ieee-floats
               #:local-time
               #:closer-mop)
  :components ((:file "package")
               (:file "cbor")
               (:file "stringref")
               (:file "circular")
               (:file "memstream")
               (:file "encode")
               (:file "decode")))
