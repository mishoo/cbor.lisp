(in-package #:cbor)

#-sbcl (error "This file requires SBCL")

(ql:quickload '("flexi-streams" "yason" "jsown"))

(defun test-encode (value &rest args)
  (let ((data (coerce (apply #'encode value args) 'raw-data)))
    (loop for b across data
          for ch = (if (<= 32 b 127) (code-char b) "")
          do (format t "#x~2,'0X  ~8,'0B  ~3,D  ~A~%" b b b ch))
    data))

(defun test-decode (value &rest args)
  (decode (apply #'test-encode value args)))

(defun dumphash (hash)
  (loop for key being the hash-key using (hash-value val) of hash
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

;; (defstruct (dude)
;;   (first-name "" :type string)
;;   (last-name "" :type string)
;;   (age 0 :type fixnum))

;; (defgeneric create-object (name data)
;;   (:method ((name (eql :dude)) data)
;;     (apply #'make-dude data))
;;   (:method ((name symbol) data)
;;     data))

;; (let ((*create-object* #'create-object))
;;   (test-decode (list (make-dude :first-name "John" :last-name "Doe" :age 44)
;;                      1
;;                      (make-dude :first-name "Jane" :last-name "Austin" :age 44))))
