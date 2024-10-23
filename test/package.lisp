(defpackage #:cbor-tests
  (:use #:cl #:cbor #:parachute)
  (:import-from #:cbor #:raw-data
                #:*strict* #:*dictionary-format* #:*use-stringrefs* #:*use-sharedrefs*))
