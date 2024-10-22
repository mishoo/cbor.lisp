(in-package #:cbor)

(defun decode (input)
  (unless (memstream-p input)
    (setf input (make-memstream input)))
  (with-sharedrefs-decode
    (%decode input)))

(defmacro unroll-read-byte (size input)
  `(let ((value 0))
     (declare (type (integer 0 ,(1- (expt 2 (* 8 size)))) value))
     ,@(loop for i from (* 8 (1- size)) downto 0 by 8
             collect `(setf (ldb (byte 8 ,i) value)
                            (ms-read-byte ,input)))
     value))

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
  (let ((seq
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
               seq)))))
    (stringref-assign seq (length seq))
    seq))

(defun read-string (input size &optional indefinite-size)
  (declare (type memstream input)
           (type (integer 0 #.*max-uint64*) size)
           (type boolean indefinite-size)
           #.*optimize*)
  (let ((str
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
                                                  :end (incf (ms-position input) size))))))
    (stringref-assign str)
    str))

(defun read-stringref (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (= type 0) (type)
            "Expecting unsigned integer in read-stringref")
    (stringref-get argument)))

;; the reason why I made the inner `add' a macro is that its argument
;; will typically involve a call to %decode, and we must delay it
;; until after the first cons has been created and recorded with
;; decode-set-shareable, so that circular references will work (just
;; in case %decode encounters another shareable object).
(defmacro build-list (&body body)
  (let ((vlist (gensym))
        (vp (gensym)))
    `(let ((,vlist nil)
           (,vp nil))
       (macrolet
           ((add (el)
              `(let ((cell (cons nil nil)))
                 (setf ,',vp (if ,',vp
                                 (setf (cdr ,',vp) cell)
                                 (setf ,',vlist
                                       (decode-set-shareable cell))))
                 (setf (car cell) ,el)
                 cell)))
         ,@body
         ,vlist))))

(defun read-entries (input size callback)
  (declare (type memstream input)
           (type (or null fixnum) size)
           (type (function ()) callback)
           #.*optimize*)
  (cond
    (size
     (loop repeat size do (funcall callback)))
    (t ;; indefinite size
     (loop for tag = (ms-peek-byte input) until (= tag 255)
           do (funcall callback)
           finally (incf (ms-position input))))))

(defun read-array (input size &optional indefinite-size)
  (declare (type memstream input)
           (type (integer 0 #.*max-uint64*) size)
           (type boolean indefinite-size)
           #.*optimize*)
  (let ((wants-list (or *jsown-semantics* (eq *array-format* :list))))
    (cond
      (indefinite-size
       (cond
         (wants-list
          (build-list
            (read-entries input nil
                          (lambda ()
                            (add (%decode input))))))
         (t
          (let ((seq (make-array 0 :adjustable t :fill-pointer 0)))
            (decode-set-shareable seq)
            (read-entries input nil
                          (lambda ()
                            (vector-push-extend (%decode input) seq)))
            seq))))

      (wants-list
       (build-list
         (loop repeat size do (add (%decode input)))))

      (t
       (let ((seq (make-array size)))
         (decode-set-shareable seq)
         (loop for i below size
               do (setf (aref seq i) (%decode input)))
         seq)))))

(labels
    ((maybe-symbol (thing)
       (declare #.*optimize*)
       (if (and (stringp thing) *string-to-symbol*)
           (funcall *string-to-symbol* thing)
           thing))
     (read-alist (input &optional size)
       (declare #.*optimize*)
       (build-list
         (read-entries input size
                      (lambda ()
                        (add (cons (maybe-symbol (%decode input))
                                   (%decode input)))))))
     (read-plist (input &optional size)
       (declare #.*optimize*)
       (build-list
         (read-entries input size
                      (lambda ()
                        (add (maybe-symbol (%decode input)))
                        (add (%decode input))))))
     (read-hash (input &optional size)
       (declare #.*optimize*)
       (let ((hash (make-hash-table :test (if *string-to-symbol* #'eq #'equal))))
         (decode-set-shareable hash)
         (read-entries input size
                      (lambda ()
                        (setf (gethash (maybe-symbol (%decode input)) hash)
                              (%decode input))))
         hash)))
  (declare (inline maybe-symbol read-alist read-plist read-hash))
  (defun read-map (input size &optional indefinite-size)
    (declare (type memstream input)
             (type (integer 0 #.*max-uint64*) size)
             (type boolean indefinite-size)
             #.*optimize*)
    (if *jsown-semantics*
        (let ((obj (list :obj)))
          (decode-set-shareable obj)
          (setf (cdr obj)
                (read-alist input (unless indefinite-size size)))
          obj)
        (ecase *dictionary-format*
          (:hash (read-hash input (unless indefinite-size size)))
          (:alist (read-alist input (unless indefinite-size size)))
          (:plist (read-plist input (unless indefinite-size size)))))))

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
      (decode-set-shareable
       (local-time:unix-to-timestamp seconds :nsec (floor (* split-seconds
                                                             1000 1000 1000)))))))

(defun expect-string (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (cond
      ((= type 3)
       (read-string input argument special?))
      ((and (= type 6) (= argument +tag-stringref+))
       (read-stringref input))
      (t (error "Expecting string")))))

(defun read-string-datetime (input)
  (declare (type memstream input)
           #.*optimize*)
  (decode-set-shareable
   (local-time:parse-rfc3339-timestring
    (expect-string input))))

(defun read-symbol (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (and (= type 4) (= argument 2)) (type argument)
            "Expected array of two elements in read-symbol")
    (let* ((pak-name (cond
                       ((= 245 (ms-peek-byte input))
                        (ms-read-byte input)
                        t)
                       ((= 246 (ms-peek-byte input))
                        (ms-read-byte input)
                        nil)
                       (t
                        (expect-string input))))
           (sym-name (expect-string input))
           (package (case pak-name
                      ((t) #.(find-package "KEYWORD"))
                      ((nil) nil)
                      (otherwise (find-package pak-name)))))
      (decode-set-shareable
       (if package
           (intern sym-name package)
           (make-symbol sym-name))))))

(defun read-cons (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (and (= type 4) (= argument 2)) (type argument)
            "Expected array of two elements in read-cons")
    (let ((cell (cons nil nil)))
      (decode-set-shareable cell)
      (setf (car cell) (%decode input)
            (cdr cell) (%decode input))
      cell)))

(defun read-proper-list (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (= type 4) (type)
            "Expected array in read-proper-list")
    (let ((*array-format* :list))
      (read-array input argument special?))))

(defun read-character (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (= type 0) (type)
            "Expected unsigned integer in read-character")
    (code-char argument)))

(defun read-object (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (and (= type 4) (= argument 2))
            (type argument)
            "Expected array of two elements in read-object"))
  (let* ((name (%decode input)))
    (assert (symbolp name) (name) "Expected class name (symbol) in read-object")
    (with-tag (input (ms-read-byte input))
      (assert (= type 5) (type) "Expected map in read-object")
      (let* ((class (find-class name))
             (object (allocate-instance class)))
        (decode-set-shareable object)
        (read-entries input
                      (unless special? argument)
                      (lambda ()
                        (setf (slot-value object (%decode input))
                              (%decode input))))
        object))))

(defun read-structure (input)
  (read-object input))

(defun read-tagged (input tag)
  (declare (type memstream input)
           (type (integer 0 #.*max-uint64*) tag)
           #.*optimize*)
  (case tag
    (#.+tag-string-datetime+ (read-string-datetime input))
    (#.+tag-float-datetime+ (read-datetime input))
    (#.+tag-positive-bignum+ (read-bignum input))
    (#.+tag-negative-bignum+ (- 0 1 (read-bignum input)))
    (#.+tag-stringref-namespace+ (with-stringrefs nil (%decode input)))
    (#.+tag-stringref+ (read-stringref input))
    (#.+tag-sharedref+ (decode-get-shareable (%decode input)))
    (#.+tag-shareable+ (with-decode-shareable (%decode-no-shareable input)))
    (#.+tag-ratio+ (read-ratio input))
    (#.+tag-complex+ (read-complex input))
    (#.+tag-symbol+ (read-symbol input))
    (#.+tag-cons+ (read-cons input))
    (#.+tag-list+ (read-proper-list input))
    (#.+tag-character+ (read-character input))
    (#.+tag-object+ (read-object input))
    (#.+tag-structure+ (read-structure input))
    (#.+tag-embedded-cbor+ (%decode input))
    (t
     (if *custom-tag-reader*
         (funcall *custom-tag-reader* tag (%decode input))
         (error "Unsupported sematic tag ~A" tag)))))

(defun %decode-float (argument)
  (declare #.*optimize*)
  (etypecase argument
    ((unsigned-byte 16) (decode-float16 argument))
    ((unsigned-byte 32) (decode-float32 argument))
    ((unsigned-byte 64) (decode-float64 argument))))

(defun read-ratio (input)
  (declare (type memstream input)
           #.*optimize*)
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

(defun read-complex (input)
  (declare (type memstream input)
           #.*optimize*)
  (with-tag (input (ms-read-byte input))
    (assert (and (= type 4) (= argument 2)) (type argument)
            "Expected array of two numbers in read-complex")
    (let ((real (%decode input))
          (imag (%decode input)))
      (complex real imag))))

(defun %decode (input)
  (declare (type memstream input)
           #.*optimize*)
  (let ((*in-shareable* nil))
    (%decode-no-shareable input)))

(defun %decode-no-shareable (input)
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
           (2 (decode-set-shareable
               (read-binary input argument special?)))
           (3 (decode-set-shareable
               (read-string input argument special?)))
           (4 (read-array input argument special?))
           (5 (read-map input argument special?))
           (6 (read-tagged input argument))
           (7 (if simple?
                  (cons 'simple argument)
                  (%decode-float argument)))))))))
