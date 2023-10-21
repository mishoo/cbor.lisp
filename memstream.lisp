(in-package #:cbor)

(declaim (type fixnum *buffer-size*))
(defparameter *buffer-size* (* 64 1024))

(defstruct (memstream
            (:constructor %make-memstream)
            (:conc-name ms-))
  (data (make-array *buffer-size* :element-type '(unsigned-byte 8))
   :type raw-data)
  (position 0 :type (integer 0 #.array-total-size-limit))
  (size 0 :type (integer 0 #.array-total-size-limit)))

(defun make-memstream (&optional data)
  (declare (type (or null raw-data) data))
  (if data
      (%make-memstream :data data
                       :position 0
                       :size (length data))
      (%make-memstream)))

(defmacro with-ms-position ((stream position) &body body)
  (let ((vstream (gensym))
        (vposition (gensym))
        (savepos (gensym)))
    `(let* ((,vstream ,stream)
            (,vposition ,position)
            (,savepos (ms-position ,vstream)))
       (declare (type memstream ,vstream)
                (type fixnum ,vposition ,savepos))
       (setf (ms-position ,vstream) ,vposition)
       (unwind-protect
            (progn ,@body)
         (unless (= ,vposition ,savepos)
           (setf (ms-position ,vstream) ,savepos))))))

(declaim (inline ms-read-byte))
(defun ms-read-byte (stream)
  (declare (type memstream stream)
           #.*optimize*)
  (with-slots (data size position) stream
    (cond
      ((< position size)
       (prog1
           (aref data position)
         (incf position)))
      (t (error 'end-of-file :stream stream)))))

(declaim (inline ms-peek-byte))
(defun ms-peek-byte (stream)
  (declare (type memstream stream)
           #.*optimize*)
  (with-slots (data size position) stream
    (cond
      ((< position size)
       (aref data position))
      (t (error 'end-of-file :stream stream)))))

(declaim (inline ms-extend-stream))
(defun ms-extend-stream (stream &optional (min-size 0))
  (declare (type memstream stream)
           (type (integer 0 #.array-total-size-limit) min-size)
           #.*optimize*)
  (with-slots (data) stream
    (let ((newdata (make-array (max min-size
                                    (min (* 2 (array-total-size data))
                                         array-total-size-limit))
                               :element-type '(unsigned-byte 8))))
      (replace newdata data)
      (setf data newdata))))

(declaim (inline ms-write-byte))
(defun ms-write-byte (byte stream)
  (declare (type memstream stream)
           (type (unsigned-byte 8) byte)
           #.*optimize*)
  (with-slots (data size position) stream
    (when (>= position (array-total-size data))
      (ms-extend-stream stream))
    (setf (aref data position) byte)
    (unless (< position size)
      (incf size))
    (incf position)))

(declaim (inline ms-read-sequence))
(defun ms-read-sequence (sequence stream &key (start 0) (end (length sequence)))
  (declare (type raw-data sequence)
           (type memstream stream)
           (type (integer 0 #.array-total-size-limit) start end)
           #.*optimize*)
  (with-slots (data size position) stream
    (let* ((count (- end start))
           (end2 (min size (+ position count))))
      (replace sequence data :start1 start :end1 end
                             :start2 position :end2 end2)
      (prog1
          (- end2 position)
        (setf position end2)))))

(declaim (inline ms-write-sequence))
(defun ms-write-sequence (sequence stream &key (start 0) (end (length sequence)))
  (declare (type raw-data sequence)
           (type memstream stream)
           (type (integer 0 #.array-total-size-limit) start end)
           #.*optimize*)
  (with-slots (data size position) stream
    (let* ((count (- end start))
           (end1 (+ position count)))
      (when (> end1 (array-total-size data))
        (ms-extend-stream stream end1))
      (replace data sequence :start1 position :end1 end1
                             :start2 start :end2 end)
      (setf position end1)
      (when (> end1 size)
        (setf size end1))
      sequence)))

(defun ms-whole-data (stream)
  (declare (type memstream stream)
           #.*optimize*)
  (with-slots (data size) stream
    (declare (type raw-data data))
    (subseq data 0 size)))
