(in-package #:cbor)

(defun decode (input)
  (declare (type (or memstream raw-data) input))
  (unless (memstream-p input)
    (setf input (make-memstream input)))
  (%decode input))

(defmacro unroll-read-byte (size input)
  `(progn
     (let ((value 0))
       (declare (type (integer 0 ,(1- (expt 2 (* 8 size)))) value))
       ,@(loop for i from (* 8 (1- size)) downto 0 by 8
               collect `(setf (ldb (byte 8 ,i) value)
                              (ms-read-byte ,input)))
       value)))

(defmacro with-tag ((_input _tag) &body body)
  (let ((input (gensym))
        (tag (gensym)))
    `(let* ((,input ,_input)
            (,tag ,_tag)
            (type (ldb (byte 3 5) ,tag))
            (argument (ldb (byte 5 0) ,tag))
            (special? (= argument 31))
            (simple? (= type 7)))
       (declare (type (unsigned-byte 3) type)
                (ignorable special? simple?))
       (case argument
         (24 (setf argument (unroll-read-byte 1 ,input)))
         (25 (setf argument (unroll-read-byte 2 ,input) simple? nil))
         (26 (setf argument (unroll-read-byte 4 ,input) simple? nil))
         (27 (setf argument (unroll-read-byte 8 ,input) simple? nil)))
       ,@body)))

(defun read-binary (input size &optional indefinite-size)
  (declare (type memstream input)
           (type (integer 0 #.*max-uint64*) size)
           (type boolean indefinite-size)
           #.*optimize*)
  (cond
    (indefinite-size
     (loop for tag = (ms-read-byte input)
           until (= tag 255)
           collect (with-tag (input tag)
                     (assert (= type 2)
                             (type)
                             "Invalid chunk type ~A when reading indefinite-length byte string"
                             type)
                     (assert (not special?)
                             (argument)
                             "Nested indefinite-length byte string (argument is 31)")
                     (read-binary input argument))
             into sequences
           finally (return (apply #'concatenate 'raw-data sequences))))
    (t
     (let ((seq (make-array size :element-type '(unsigned-byte 8))))
       (ms-read-sequence seq input)
       seq))))

(defun read-string (input size &optional indefinite-size)
  (declare (type memstream input)
           (type (integer 0 #.*max-uint64*) size)
           (type boolean indefinite-size)
           #.*optimize*)
  (cond
    (indefinite-size
     (loop for tag = (ms-read-byte input)
           until (= tag 255)
           collect (with-tag (input tag)
                     (assert (= type 3) (type)
                             "Invalid chunk type ~A when reading indefinite-length string"
                             type)
                     (assert (not special?) (argument)
                             "Nested indefinite-length string (argument is 31)")
                     (read-string input argument))
             into strings
           finally (return (apply #'concatenate 'simple-string strings))))
    (t
     ;; that's cheating a bit, accessing the underlying data
     ;; sequence (should have used ms-read-sequence), but oh
     ;; well.. why cons if we can not cons?
     (trivial-utf-8:utf-8-bytes-to-string (ms-data input)
                                          :start (ms-position input)
                                          :end (incf (ms-position input) size)))))

(defun read-array (input size &optional indefinite-size)
  (declare (type memstream input)
           (type (integer 0 #.*max-uint64*) size)
           (type boolean indefinite-size)
           #.*optimize*)
  (let ((wants-list (or *jsown-semantics* (eq *array-format* :list))))
    (cond
      (indefinite-size
       (loop for tag = (ms-peek-byte input)
             for length of-type fixnum from 0
             until (= tag 255)
             collect (%decode input) into entries
             finally (incf (ms-position input))
                     (return (if wants-list
                                 entries
                                 (make-array length :initial-contents entries)))))
      (wants-list
       (loop repeat size collect (%decode input)))
      (t
       (let ((seq (make-array size)))
         (loop for i below size
               do (setf (aref seq i) (%decode input)))
         seq)))))

(labels
    ((maybe-symbol (thing)
       (declare #.*optimize*)
       (if (and (stringp thing) *string-to-symbol*)
           (funcall *string-to-symbol* thing)
           thing))
     (read-alist (input size &optional indefinite-size)
       (declare #.*optimize*)
       (cond
         (indefinite-size
          (loop for tag = (ms-peek-byte input)
                until (= tag 255)
                collect (cons (maybe-symbol (%decode input))
                              (%decode input))
                finally (incf (ms-position input))))
         (t
          (loop repeat size
                collect (cons (maybe-symbol (%decode input))
                              (%decode input))))))
     (read-plist (input size &optional indefinite-size)
       (declare #.*optimize*)
       (cond
         (indefinite-size
          (loop for tag = (ms-peek-byte input)
                until (= tag 255)
                nconc (list (maybe-symbol (%decode input))
                            (%decode input))
                finally (incf (ms-position input))))
         (t
          (loop repeat size
                nconc (list (maybe-symbol (%decode input))
                            (%decode input))))))
     (read-hash (input size &optional indefinite-size)
       (declare #.*optimize*)
       (let ((hash (make-hash-table :test (if *string-to-symbol* #'eq #'equal))))
         (cond
           (indefinite-size
            (loop for tag = (ms-peek-byte input)
                  until (= tag 255)
                  for key = (maybe-symbol (%decode input))
                  for val = (%decode input)
                  do (setf (gethash key hash) val)
                  finally (incf (ms-position input))))
           (t
            (loop repeat size
                  for key = (maybe-symbol (%decode input))
                  for val = (%decode input)
                  do (setf (gethash key hash) val))))
         hash)))
  ;; (declare (inline maybe-symbol read-alist read-plist read-hash))
  (defun read-map (input size &optional indefinite-size)
    (declare (type memstream input)
             (type (integer 0 #.*max-uint64*) size)
             (type boolean indefinite-size)
             #.*optimize*)
    (if *jsown-semantics*
        (cons :obj (read-alist input size indefinite-size))
        (ecase *dictionary-format*
          (:hash (read-hash input size indefinite-size))
          (:alist (read-alist input size indefinite-size))
          (:plist (read-plist input size indefinite-size))))))

(defun read-bignum (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (= type 2) (type)
            "Expected binary sequence in bignum, found ~A" type)
    (let ((seq (read-binary input argument special?))
          (val 0))
      (declare (type raw-data seq)
               (type integer val))
      (loop with n of-type fixnum = (1- (length seq))
            for i from 0 to n
            for j of-type fixnum from (* 8 n) downto 0 by 8
            for byte = (aref seq i)
            do (setf (ldb (byte 8 j) val) byte))
      val)))

(defun read-datetime (input)
  (declare (type memstream input)
           #.*optimize*)
  (let ((epoch (%decode input)))
    (assert (typep epoch '(or integer float))
            (epoch)
            "Expected integer or float in Unix timestamp, found ~A"
            (type-of epoch))
    (multiple-value-bind (seconds split-seconds) (floor epoch)
      (local-time:unix-to-timestamp seconds :nsec (floor (* split-seconds
                                                            1000 1000 1000))))))

(defun read-string-datetime (input)
  (declare (type memstream input)
           #.*optimize*)
  (local-time:parse-rfc3339-timestring
   (with-tag (input (ms-read-byte input))
     (assert (= type 3) (type)
             "Expecting text string in datetime, but found ~A" type)
     (read-string input argument special?))))

(defun read-symbol (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (and (= type 4) (= argument 2)) (type argument)
            "Expected array of two elements in read-symbol")
    (let* ((pak-name (%decode input))
           (sym-name (%decode input))
           (package (case pak-name
                      ((t) #.(find-package "KEYWORD"))
                      ((nil) nil)
                      (otherwise (find-package pak-name)))))
      (if package
          (intern sym-name package)
          (make-symbol sym-name)))))

(defun read-cons (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (and (= type 4) (= argument 2)) (type argument)
            "Expected array of two elements in read-cons")
    (cons (%decode input)
          (%decode input))))

(defun read-tagged (input tag)
  (declare (type memstream input)
           (type (integer 0 #.*max-uint64*) tag)
           #.*optimize*)
  (case tag
    (0 (read-string-datetime input))
    (1 (read-datetime input))
    (2 (read-bignum input))
    (3 (- 0 1 (read-bignum input)))
    (#.+tag-ratio+ (read-ratio input))
    (#.+tag-symbol+ (read-symbol input))
    (#.+tag-cons+ (read-cons input))
    (55799 (%decode input))
    (t
     (if *custom-tag-reader*
         (funcall *custom-tag-reader* tag (%decode input))
         (error "Unsupported sematic tag ~A" tag)))))

(defun read-float (argument)
  (declare #.*optimize*)
  (etypecase argument
    ((unsigned-byte 16) (decode-float16 argument))
    ((unsigned-byte 32) (decode-float32 argument))
    ((unsigned-byte 64) (decode-float64 argument))))

(defun read-ratio (input)
  (with-tag (input (ms-read-byte input))
    (assert (and (= type 4) (= argument 2)) (type argument)
            "Expected array of two integers in read-ratio")
    (let ((numerator (%decode input))
          (denominator (%decode input)))
      (assert (and (typep numerator 'integer)
                   (typep denominator 'integer))
              (numerator denominator)
              "Expected two integers in ratio")
      (assert (not (zerop denominator))
              (denominator)
              "Division by zero in ratio")
      (/ numerator denominator))))

(defun %decode (input)
  (declare (type memstream input)
           #.*optimize*)
  (let ((tag (ms-read-byte input)))
    (case tag
      (244 nil)                         ; false
      (245 t)                           ; true
      (246 nil)                         ; null
      (247 nil)                         ; undefined
      (otherwise
       (with-tag (input tag)
         (case type
           (0 argument)
           (1 (- 0 1 argument))
           (2 (read-binary input argument special?))
           (3 (read-string input argument special?))
           (4 (read-array input argument special?))
           (5 (read-map input argument special?))
           (6 (read-tagged input argument))
           (7 (if simple?
                  (cons 'simple argument)
                  (read-float argument)))))))))
