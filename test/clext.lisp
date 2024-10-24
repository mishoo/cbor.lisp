(in-package #:cbor-tests)

(defclass baseclass () ())

(defclass testclass (baseclass)
  ((slot1 :initarg :slot1 :accessor slot1-of)
   (slot2 :initarg :slot2 :accessor slot2-of)
   (slot3 :initarg :slot3 :accessor slot3-of)))

(defstruct teststruct slot1 slot2 slot3)

(defun objects-equal (a b)
  (let* ((ca (class-of a))
         (cb (class-of b)))
    (when (equals ca cb)
      (loop for slot in (closer-mop:class-slots ca)
            for name = (closer-mop:slot-definition-name slot)
            always (or (and (not (slot-boundp a name))
                            (not (slot-boundp b name)))
                       (equals (slot-value a name) (slot-value b name)))))))

(defmethod equals:equals ((x baseclass) (y baseclass) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys recursive))
  (objects-equal x y))

(defmethod equals:equals ((x teststruct) (y teststruct) &rest keys &key recursive &allow-other-keys)
  (declare (ignore keys recursive))
  (objects-equal x y))

(define-test clext
  (setf *strict* t
        *string-to-symbol* nil
        *symbol-to-string* nil
        *dictionary-format* :hash
        *use-stringrefs* nil
        *use-sharedrefs* t))

(define-test (clext basic-struct)
  (t-encode (make-teststruct)
    (is equals val data))
  (t-encode (make-teststruct :slot1 1
                             :slot2 'foo
                             :slot3 #${ "foo": 1, "bar": 2 })
    (is equals val data)))

(define-test (clext basic-objects)
  (t-encode (make-instance 'testclass)
    (is equals val data)
    (false (slot-boundp val 'slot1))
    (false (slot-boundp val 'slot2))
    (false (slot-boundp val 'slot3)))
  (let ((obj (make-instance 'testclass :slot1 1
                                       :slot2 'foo)))
    (t-encode obj
      (is equals val obj)
      (false (slot-boundp val 'slot3))))
  (let ((obj (make-instance 'testclass :slot1 1
                                       :slot2 'foo
                                       :slot3 #${ "foo": 1, "bar": 2 })))
    (t-encode obj
      (is equals val obj))))

(define-test (clext reference)
  (let* ((a (make-instance 'testclass))
         (b (make-instance 'testclass :slot1 a))
         (v (vector a b)))
    (t-encode v
      (true (eq (aref val 0)
                (slot1-of (aref val 1)))))))

(define-test (clext forward-reference)
  (let* ((a (make-instance 'testclass))
         (b (make-instance 'testclass))
         (v (vector a b)))
    (setf (slot1-of a) b)
    (t-encode v
      (true (eq (slot1-of (aref val 0))
                (aref val 1))))))

(define-test (clext object-circular-ref)
  (let ((a (make-instance 'testclass))
        (b (make-instance 'testclass)))
    (setf (slot1-of a) b
          (slot1-of b) a)
    (t-encode (cons a b)
      (true (eq (slot1-of (car val))
                (cdr val)))
      (true (eq (slot1-of (cdr val))
                (car val))))))

(define-test (clext circular-cons)
  (let ((a (cons nil nil)))
    (setf (car a) a (cdr a) a)
    (t-encode a
      (true (eq val (car val)))
      (true (eq val (cdr val))))))

(define-test (clext circular-array)
  (let ((a (vector 1 2 nil)))
    (setf (aref a 2) a)
    (t-encode a
      (true (eq val (aref val 2))))))

(define-test (clext circular-list)
  (let ((lst (list 1 2 3)))
    (setf (cdr (last lst)) lst)
    (t-encode lst
      ;; can't use (last val) here.
      (true (eq val (cdddr val))))))

(define-test (clext circular-hash)
  (let ((hash (alexandria:copy-hash-table #${ "foo": 1, "bar": 2 })))
    (setf (gethash 'self hash) hash)
    (t-encode hash
      (is equals 1 (gethash "foo" val))
      (is equals 2 (gethash "bar" val))
      (true (eq val (gethash 'self val))))))

(define-test (clext circular-list-array)
  (let* ((a (vector 1 2 nil))
         (lst (list 'a 'b 'c a)))
    (setf (aref a 2) lst)
    (t-encode lst
      (true (eq val (aref (fourth val) 2))))))

(define-test (clext circular-list-hash)
  (let* ((hash (alexandria:copy-hash-table #${ "foo": 1, "bar": 2 }))
         (lst (list 'a 'b 'c hash)))
    (setf (gethash :list hash) lst)
    (t-encode lst
      (true (eq val (gethash :list (fourth val)))))))
