(asdf:defsystem #:cbor-tests
  :description "CBOR encoder/decoder (test suite)"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cbor #:parachute #:yason #:equals #:local-time)
  :components ((:file "test/package")
               (:file "test/utils")
               (:file "test/core")))
