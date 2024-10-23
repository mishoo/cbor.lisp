(in-package #:cbor-tests)

(defun bytes (hexstr)
  (labels ((rec (hexstr)
             (if (> (length hexstr) 2)
                 (cons (parse-integer (subseq hexstr 0 2) :radix 16)
                       (rec (subseq hexstr 2)))
                 (cons (parse-integer hexstr :radix 16) nil))))
    (coerce (rec hexstr) 'raw-data)))

(set-dispatch-macro-character
 #\# #\$
 (lambda (input c n)
   (declare (ignore c n))
   `(quote ,(yason:parse input :object-as :hash-table
                               :json-arrays-as-vectors t))))

(defun equals (a b)
  (equals:equals a b :recursive t :check-properties nil))

(define-test core
  (setf *strict* t
        *string-to-symbol* nil
        *symbol-to-string* nil
        *dictionary-format* :hash
        *use-stringrefs* nil
        *use-sharedrefs* nil))

(defmacro cbor-test (hex roundtrip expected)
  `(let* ((bin (bytes ,hex))
          (val (cbor:decode bin)))
     (is equals ,expected val)
     ,(when roundtrip
        `(let ((data (cbor:encode val)))
           (is equals bin data ,hex)))))

(defun cbor-test-date (hex expected)
  (let* ((bin (bytes hex))
         (val (cbor:decode bin)))
    (setf expected (local-time:parse-rfc3339-timestring expected))
    (is local-time:timestamp= expected val)))
