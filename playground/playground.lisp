(in-package #:cbor)

#-sbcl (error "This file requires SBCL")

(ql:quickload '("flexi-streams" "yason" "jsown" "cl-base64"))

(defun test-encode (value &rest args)
  (let ((data (coerce (apply #'encode value args) 'raw-data)))
    (loop for b across data
          for ch = (if (<= 32 b 127) (code-char b) "")
          do (format t "#x~2,'0X  ~8,'0B  ~3,D  ~A~%" b b b ch))
    data))

(defun test-decode (value &rest args)
  (let ((v2 (decode (apply #'test-encode value args))))
    ;; (format t "EQUAL? ~A" (equal value v2))
    v2))

(defun dumphash (hash)
  (loop for key being the hash-key of hash using (hash-value val)
        do (format t "~S : ~S~%" key val)))

(defparameter *timing-name* nil)
(defparameter *timing-footer* nil)

(defun print-time (&key real-time-ms user-run-time-us system-run-time-us
                     gc-run-time-ms gc-real-time-ms processor-cycles eval-calls
                     lambdas-converted page-faults bytes-consed
                     aborted)
  (format *trace-output*
          "~A: ~/sb-impl::format-milliseconds/, ~:D consed ~A~%"
          *timing-name*
          real-time-ms
          bytes-consed
          *timing-footer*))

(defmacro mytime (title foot &body body)
  `(progn
     (sb-ext:gc :full t)
     (let ((*timing-name* ,title)
           (*timing-footer* ,foot))
       (sb-impl::call-with-timing #'print-time (lambda () ,@body)))))

(defmacro with-binary-output ((output filename) &body body)
  (let ((vfilename (gensym)))
    `(let ((,vfilename ,filename))
       (with-open-file (,output ,vfilename
                                :element-type '(unsigned-byte 8)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
         ,@body))))

(defmacro %test-json (filename title decode-json encode-json)
  (let ((vfilename (gensym)))
    `(let ((,vfilename ,filename)
           data1)
       (with-open-file (input ,vfilename :element-type '(unsigned-byte 8))
         (let* ((seq (make-array (file-length input) :element-type '(unsigned-byte 8))))
           (read-sequence seq input)
           (mytime
               (format nil "~A-DECODE" ,title)
               (format nil "(~:D bytes JSON)" (length seq))
             (let ((input (trivial-utf-8:utf-8-bytes-to-string seq)))
               (setf data1 ,decode-json)))))
       (let ((seq (mytime "CBOR-ENCODE" "" (encode data1))))
         (with-binary-output (output (format nil "~A-~A.bin.cbor" filename ,title))
           (write-sequence seq output))
         (setf seq (coerce seq 'raw-data))
         (let* ((data (mytime "CBOR-DECODE"
                          (format nil "(~:D bytes BINARY)" (length seq))
                        (decode (make-memstream seq))))
                (seq (mytime
                         (format nil "~A-ENCODE" ,title)
                         ""
                       (trivial-utf-8:string-to-utf-8-bytes ,encode-json))))
           (format t "EQUALP? ~A~%" (equalp data data1))
           (with-binary-output (output (format nil "~A-~A.json" filename ,title))
             (write-sequence seq output))))
       nil)))

(defun test-json (&optional (filename "~/JSON/large-file.json"))
  (let ((*string-to-symbol* nil)
        (*symbol-to-string* nil)
        (*array-format* :list))
    (%test-json filename "YASON"
                (with-input-from-string (in input)
                  (yason:parse in))
                (with-output-to-string (out)
                  (let ((yason:*symbol-key-encoder* *symbol-to-string*))
                    (yason:encode data out)))))
  (let ((*jsown-semantics* t)
        (*string-to-symbol* nil)
        (*symbol-to-string* nil))
    (%test-json filename "JSOWN"
                (jsown:parse input)
                (jsown:to-json data))))

;;;

(defstruct (dude)
  (first-name "" :type string)
  (last-name "" :type string)
  (age 0 :type fixnum))

(defclass geometry ()
  ((x :type fixnum :initarg :x)
   (y :type fixnum :initarg :y)
   (width :type fixnum :initarg :width)
   (height :type fixnum :initarg :height)))

(defclass window ()
  ((title :type string :initarg :title)
   (geometry :type geometry :initarg :geometry)
   (buttons :initform '(:close :minimize :maximize))))

;; (test-decode (list (make-dude :first-name "John" :last-name "Doe" :age 44)
;;                    1
;;                    (make-dude :first-name "Jane" :last-name "Austin" :age 40)))



;;; appendix-a.json test data from: https://github.com/cbor/test-vectors/
(defparameter *json-file* (asdf:system-relative-pathname "cbor" "playground/appendix_a.json"))

(defun read-json (filename)
  (with-open-file (input filename :element-type '(unsigned-byte 8))
    (jsown:parse (trivial-utf-8:read-utf-8-string input :stop-at-eof t))))

(defun test-appendix-a-json (&optional (filename *json-file*))
  (let (;; (*jsown-semantics* t)
        (*symbol-to-string* nil)
        (*string-to-symbol* nil))
    (loop for test in (read-json filename)
          for base64 = (jsown:val test "cbor")
          for binary = (cl-base64:base64-string-to-usb8-array base64)
          for roundtrip = (jsown:val test "roundtrip")
          for decoded = (ignore-errors (jsown:val test "decoded"))
          for diagnostic = (ignore-errors (jsown:val test "diagnostic"))
          do (handler-case
                 (let ((data (cbor:decode binary)))
                   (format t "   ~{~2,'0X~} ~S~%" (coerce binary 'list) data)
                   (cond
                     (roundtrip
                      (let ((enc (cbor:encode data)))
                        (unless (equalp enc binary)
                          (format t "!! ~{~2,'0X~} ~S ~{~2,'0X~} (~A)~%"
                                  (coerce binary 'list) data
                                  (coerce enc 'list) (or diagnostic "")))))
                     (decoded
                      (format t "   ~S~%" decoded)
                      (let* ((*jsown-semantics* t)
                             (data (cbor:decode binary)))
                        (unless (equalp data decoded)
                          (format t "!! err: we got ~S~%" data))))))
               (floating-point-overflow ()
                 (format t "** FLOATING-POINT-OVERFLOW ~{~2,'0X~}~%" (coerce binary 'list)))
               (end-of-file (err)
                 (format t "** END-OF-FILE ~{~2,'0X~}~%" (coerce binary 'list))
                 (error err))
               (error (err)
                 (format t "** ERROR ~{~2,'0X~}~%" (coerce binary 'list))
                 (format t "         ~A~%" err))))))

(defun hex-to-bytes (hexstr)
  (labels ((rec (hexstr)
             (if (> (length hexstr) 2)
                 (cons (parse-integer (subseq hexstr 0 2) :radix 16)
                       (rec (subseq hexstr 2)))
                 (cons (parse-integer hexstr :radix 16) nil))))
    (coerce (rec hexstr) 'raw-data)))
