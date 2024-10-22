(in-package #:cbor)

;;; shared value utilities
;;; http://cbor.schmorp.de/value-sharing

(defparameter *sharedref-cache* nil)
(defparameter *sharedref-index* nil)
(defparameter *in-shareable* nil)

(defmacro with-sharedrefs-encode (data &body body)
  `(cond
     (*use-sharedrefs*
      (let ((*sharedref-cache* (dig-references ,data))
            (*sharedref-index* -1))
        ,@body))
     (t
      ,@body)))

(defmacro with-sharedrefs-decode (&body body)
  `(let ((*sharedref-cache* (make-array 0 :adjustable t :fill-pointer 0)))
     ,@body))

(defmacro with-decode-shareable (&body body)
  `(let ((*in-shareable* t))
     (progn ,@body)))

(defun decode-set-shareable (value)
  (when *in-shareable*
    ;; we only do it once per with-decode-shareable
    (vector-push-extend value *sharedref-cache*)
    (setf *in-shareable* nil))
  value)

(defun decode-get-shareable (index)
  (aref *sharedref-cache* index))

(defmacro encode-maybe-shared ((_value _output) &body body)
  (let ((value (gensym))
        (output (gensym)))
    `(let* ((,value ,_value)
            (,output ,_output)
            (ref (shared-value ,value)))
       (declare (type fixnum *sharedref-index*))
       (cond
         ((car ref)
          ;; already encoded, index in `car' - write reference
          (write-tag 6 +tag-sharedref+ ,output)
          (encode-positive-integer (the fixnum (car ref)) ,output))
         (ref
          ;; should encode. store index and tag as shareable.
          (setf (car ref) (incf *sharedref-index*))
          (write-tag 6 +tag-shareable+ ,output)
          ,@body)
         (t
          ,@body)))))

(defun shared-value (value)
  (when *sharedref-cache*
    (gethash value *sharedref-cache*)))

(defun dig-references (data)
  "Builds a set (as a hash table) of objects that are seen more than
once. Will return NIL if no duplicates are encountered."
  ;; (declare #.*optimize*)
  (let ((seen (make-hash-table :test #'eq))
        (refd (make-hash-table :test #'eq)))
    (labels
        ((mark (obj)
           (let ((saved (gethash obj seen)))
             (cond
               ((not saved)
                ;; first time encountered, save and keep digging
                (setf (gethash obj seen) (list nil))
                t)
               ((not (car saved))
                ;; second time, save it in the other hash
                (setf (gethash obj refd) (list nil)
                      (car saved) t)
                ;; no more digging
                nil))))
         (dig (data)
           (typecase data
             (string
              (mark data))
             (symbol
              (mark data))
             ((vector (unsigned-byte 8))
              (mark data))
             (hash-table
              (when (mark data)
                (loop for key being the hash-key
                        using (hash-value val) of data
                      do (dig key)
                         (dig val))))
             (cons
              (when (mark data)
                (dig (car data))
                (dig (cdr data))))
             (vector
              (when (mark data)
                (loop for el across data do (dig el))))
             ((or standard-object structure-object)
              (when (mark data)
                (let ((class (class-of data)))
                  (loop for slot in (closer-mop:class-slots class)
                        for name = (closer-mop:slot-definition-name slot)
                        when (slot-boundp data name) do
                          (dig name)
                          (dig (slot-value data name)))))))))
      (dig data)
      (when (>= (hash-table-count refd) 0)
        refd))))
