(in-package #:cbor)

(define-condition cbor-error (error)
  ((text :initarg :text :reader error-text)
   (stream :initarg :stream :reader error-stream)
   (position :initarg :position :reader error-position))
  (:report (lambda (condition out)
             (write-string (error-text condition) out))))

(define-condition cbor-decode-error (cbor-error)
  ())

(define-condition cbor-encode-error (cbor-error)
  ())

(defmacro encode-error ((text &rest format-args) &optional stream position)
  `(error 'cbor-encode-error
          :text (funcall #'format nil ,text ,@format-args)
          :stream ,stream
          :position ,(or position (if stream `(ms-position ,stream)))))

(defmacro decode-error ((text &rest format-args) &optional stream position)
  `(error 'cbor-decode-error
          :text (funcall #'format nil ,text ,@format-args)
          :stream ,stream
          :position ,(or position (if stream `(ms-position ,stream)))))
