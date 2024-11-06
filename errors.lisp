(in-package #:cbor)

(define-condition cbor-error (error)
  ((text :initarg :text :reader error-text)
   (stream :initarg :stream :reader error-stream)
   (position :initarg :position :reader error-position)))

(define-condition cbor-decode-error (cbor-error)
  ())

(define-condition cbor-encode-error (cbor-error)
  ())

(defmacro encode-error ((text &rest format-args) &optional stream position)
  `(make-condition 'cbor-encode-error
                   :text (funcall #'format nil ,text ,@format-args)
                   :stream ,stream
                   :position ,(or position (if stream `(ms-position ,stream)))))

(defmacro decode-error ((text &rest format-args) &optional stream position)
  `(make-condition 'cbor-decode-error
                   :text (funcall #'format nil ,text ,@format-args)
                   :stream ,stream
                   :position ,(or position (if stream `(ms-position ,stream)))))
