(in-package #:cbor)

(defun encode (value)
  (let ((output (make-memstream)))
    (%encode value output)
    (ms-whole-data output)))

(defun encode-false (output)
  (declare (type memstream output)
           #.*optimize*)
  (ms-write-byte 244 output))

(defun encode-true (output)
  (declare (type memstream output)
           #.*optimize*)
  (ms-write-byte 245 output))

(defun encode-null (output)
  (declare (type memstream output)
           #.*optimize*)
  (ms-write-byte 246 output))

(defmacro unroll-write-byte (size value output)
  `(progn
     ,@(loop for i from (* 8 (1- size)) downto 0 by 8
             collect `(ms-write-byte (ldb (byte 8 ,i) ,value) ,output))))

(defun write-tag (type argument output)
  (declare (type (unsigned-byte 3) type)
           (type (integer 0 #.*max-uint64*) argument)
           (type memstream output)
           #.*optimize*)
  (let ((tag (ash type 5)))
    (cond
      ((<= argument 23)
       (ms-write-byte (logior tag argument) output))
      ((<= argument #xFF)
       (ms-write-byte (logior tag 24) output)
       (unroll-write-byte 1 argument output))
      ((<= argument #xFFFF)
       (ms-write-byte (logior tag 25) output)
       (unroll-write-byte 2 argument output))
      ((<= argument #xFFFFFFFF)
       (ms-write-byte (logior tag 26) output)
       (unroll-write-byte 4 argument output))
      (t
       (ms-write-byte (logior tag 27) output)
       (unroll-write-byte 8 argument output)))))

(defun encode-positive-integer (value output)
  (declare (type (integer 0 #.*max-uint64*) value)
           (type memstream output)
           #.*optimize*)
  (write-tag 0 value output))

(defun encode-negative-integer (value output)
  (declare (type (integer #.*min-uint64* -1) value)
           (type memstream output)
           #.*optimize*)
  (write-tag 1 (1- (- value)) output))

(macrolet ((try (bytes encoder decoder)
             `(handler-case
                  (let ((v (,encoder value)))
                    (when (= value (,decoder v))
                      (ms-write-byte (logior #b11100000
                                             ,(ecase bytes
                                                (2 25)
                                                (4 26)
                                                (8 27)))
                                     output)
                      (unroll-write-byte ,bytes v output)
                      t))
                (error (c)
                  (declare (ignore c))
                  nil))))
  (defun encode-float (value output)
    (declare (type float value)
             (type memstream output)
             #.*optimize*)
    (or (try 2 encode-float16 decode-float16)
        (try 4 encode-float32 decode-float32)
        (try 8 encode-float64 decode-float64)
        (error "Can't encode float value: ~A" value))))

;; http://peteroupc.github.io/CBOR/rational.html appears to be
;; somewhat official.
(defun encode-ratio (ratio output)
  (write-tag 6 30 output)
  (write-tag 4 2 output)                ; array of two values
  (%encode (numerator ratio) output)
  (%encode (denominator ratio) output))

(defun encode-binary (seq output)
  (declare (type raw-data seq)
           (type memstream output)
           #.*optimize*)
  (write-tag 2 (length seq) output)
  (ms-write-sequence seq output))

(defun encode-string (str output)
  (declare (type string str)
           (type memstream output)
           #.*optimize*)
  (write-tag 3 (trivial-utf-8:utf-8-byte-length str) output)
  (ms-write-sequence (trivial-utf-8:string-to-utf-8-bytes str) output))

(defun encode-symbol (symbol output)
  (declare (type symbol symbol)
           (type memstream output)
           #.*optimize*)
  (cond
    (*strict*
     (write-tag 6 +tag-symbol+ output)
     (let ((pak (symbol-package symbol)))
       (write-tag 4 2 output) ;; array with two values
       ;; 1. package. Encode `t' for KEYWORD, as a shortcut
       (%encode (case pak
                  (#.(find-package "KEYWORD") t)
                  ((nil) nil)
                  (t (package-name pak)))
                output)
       ;; 2. symbol name (string)
       (encode-string (symbol-name symbol) output)))
    (t (let ((str (if *symbol-to-string*
                      (funcall *symbol-to-string* symbol)
                      (symbol-name symbol))))
         (encode-string str output)))))

(defun encode-array (value output)
  (declare (type array value)
           #.*optimize*)
  (write-tag 4 (length value) output)
  (loop for val across value do (%encode val output)))

(defmacro with-dictionary ((output length) &body body)
  (let ((vlength (gensym)))
    `(let ((,vlength ,length))
       (write-tag 5 ,vlength ,output)
       ,@body)))

(defun encode-hash (value output)
  (declare (type hash-table value)
           (type memstream output)
           #.*optimize*)
  (with-dictionary (output (hash-table-count value))
    (loop for key being the hash-key using (hash-value val) of value
          do (%encode key output)
             (%encode val output))))

(defun encode-alist (value output)
  (declare (type list value)
           (type memstream output)
           #.*optimize*)
  (with-dictionary (output (length value))
    (loop for (key . val) in value
          do (%encode key output)
             (%encode val output))))

(defun proper-list-p (list)
  (loop with p = list and q = (cdr list) do
    (cond
      ((null q)
       (return t))
      ((or (eq p q)
           (not (and (listp p)
                     (listp q))))
       (return nil)))
    (setf p (cdr p))
    (setf q (cdr q))
    (unless (listp q)
      (return nil))
    (setf q (cdr q))))

(defun encode-cons (value output)
  (declare (type list value)
           (type memstream output)
           #.*optimize*)
  (write-tag 6 +tag-cons+ output)
  (write-tag 4 2 output)                ; array of 2 elements
  (%encode (car value) output)
  (if (consp (cdr value))
      (encode-cons (cdr value) output)
      (%encode (cdr value) output)))

(defun encode-proper-list (list output)
  (declare (type list list)
           (type memstream output)
           #.*optimize*)
  (write-tag 6 +tag-list+ output)
  (write-tag 4 (length list) output)
  (loop for val in list do (%encode val output)))

(defun encode-list (value output)
  (declare (type list value)
           (type memstream output)
           #.*optimize*)
  (cond
    (*strict*
     (if (proper-list-p value)
         (encode-proper-list value output)
         (encode-cons value output)))
    ((eq 'simple (car value))
     (write-tag 7 (cdr value) output))
    ((and *jsown-semantics*
          (eq :obj (car value)))
     (encode-alist (cdr value) output))
    ((and (every #'consp value)
          (some (lambda (cell)
                  (not (listp (cdr cell))))
                value))
     (encode-alist value output))
    (t
     (write-tag 4 (length value) output)
     (loop for val in value do (%encode val output)))))

(macrolet ((%encode-object ()
             `(let* ((class (class-of object))
                     (slots (remove-if-not
                             (lambda (key) (slot-boundp object key))
                             (mapcar #'closer-mop:slot-definition-name
                                     (closer-mop:class-direct-slots class)))))
                (declare (type list slots))
                (with-dictionary (output (length slots))
                  (loop for key in slots
                        for val = (slot-value object key)
                        do (%encode key output)
                           (%encode val output))))))
  (defgeneric encode-object (object output)
    (:method ((object standard-object) (output memstream))
      (declare (type standard-object object)
               #.*optimize*)
      (%encode-object))
    (:method ((object structure-object) (output memstream))
      (declare #.*optimize*)
      (%encode-object))))

(defmethod encode-object ((ts local-time:timestamp) (output memstream))
  (let* ((seconds (local-time:timestamp-to-unix ts))
         (milliseconds (local-time:timestamp-millisecond ts)))
    (write-tag 6 +tag-float-datetime+ output)
    (%encode (if (zerop milliseconds)
                 seconds
                 (coerce (+ seconds (/ milliseconds 1000)) 'double-float))
             output)))

(flet ((write-num (value output)
         (multiple-value-bind (size rem) (round (integer-length value) 8)
           (unless (zerop rem)
             (incf size))
           (write-tag 2 size output)
           (loop for i from 0 below size
                 for j from (* 8 (1- size)) downto 0 by 8
                 for byte = (ldb (byte 8 j) value)
                 do (ms-write-byte byte output)))))
  (defun encode-bignum (value output)
    (declare (type integer value)
             (type memstream output)
             #.*optimize*)
    (cond
      ((>= value 0)
       (write-tag 6 +tag-positive-bignum+ output)
       (write-num value output))
      (t
       (write-tag 6 +tag-negative-bignum+ output)
       (write-num (1- (- value)) output)))))

(defun %encode (value output)
  (declare (type memstream output)
           #.*optimize*)
  (unless *strict*
    (cond
      ((or (eq value :t)
           (eq value :true))
       (setf value t))
      ((eq value :null)
       (setf value nil))))
  (case value
    ((t) (encode-true output))
    ((nil) (if *jsown-semantics*
               (write-tag 4 0 output)
               (encode-null output)))
    (otherwise
     (cond
       ((and (not *strict*)
             (or (eq value :f)
                 (eq value :false)))
        (encode-false output))
       (t
        (etypecase value
          ((integer 0 #.*max-uint64*) (encode-positive-integer value output))
          ((integer #.*min-uint64* -1) (encode-negative-integer value output))
          (integer (encode-bignum value output))
          (float (encode-float value output))
          (ratio (if *strict*
                     (encode-ratio value output)
                     (encode-float (float value) output)))
          (string (encode-string value output))
          (symbol (encode-symbol value output))
          (hash-table (encode-hash value output))
          ((vector (unsigned-byte 8)) (encode-binary (coerce value 'raw-data) output))
          (vector (encode-array value output))
          (list (encode-list value output))
          (standard-object (encode-object value output))
          (structure-object (encode-object value output)))))))
  output)
