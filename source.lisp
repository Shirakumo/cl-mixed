(in-package #:org.shirakumo.fraf.mixed)

(defclass source (virtual)
  ((pack :initform NIL :reader pack)
   (frame-position :initform 0 :accessor frame-position)
   (done-p :initform NIL :accessor done-p)))

(defmethod initialize-instance :after ((source source) &key pack)
  (setf (pack source) pack))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type T)
    (ignore-errors
     (cond ((done-p source)
            (write-string "DONE" stream))
           ((null (frame-count source))
            (write-string "STREAM" stream))
           (T
            (format stream "~2d%" (floor (* (/ (frame-position source) (frame-count source)) 100))))))))

(defmethod (setf pack) (thing (source source))
  (etypecase thing
    ((or null pack) (setf (slot-value source 'pack) thing))
    (unpacker (setf (pack source) (pack thing)))))

(defmethod info ((source source))
  (list :name (string (class-name (class-of source)))
        :description "Input source."
        :flags ()
        :min-inputs 0
        :max-inputs 0
        :outputs 0
        :fields ()))

(defmethod output-field ((field (eql :pack)) (location (eql 0)) (source source))
  (pack source))

(defmethod (setf output-field) ((value pack) (field (eql :pack)) (location (eql 0)) (source source))
  (setf (pack source) value))

(defmethod (setf output-field) ((value null) (field (eql :pack)) (location (eql 0)) (source source))
  (setf (pack source) value))

(defmethod output ((location (eql 0)) (source source))
  (pack source))

(defmethod seek ((source source) position &key (mode :absolute) (by :frame))
  (assert (<= 0 position))
  (ecase by
    (:second
     (setf position (floor (* position (samplerate (pack source))))))
    (:percentage
     (setf position (floor (* position (frame-count source)) 100)))
    (:frame))
  (ecase mode
    (:relative
     (setf mode :absolute)
     (incf position (frame-position source)))
    (:absolute))
  (seek-to-frame source position)
  (cond ((<= (frame-count source) position)
         (setf (frame-position source) (frame-count source))
         (setf (done-p source) T))
        (T
         (setf (frame-position source) position)
         (setf (done-p source) NIL)))
  source)

(defmethod framesize ((source source))
  (framesize (pack source)))

(defmethod byte-position ((source source))
  (* (frame-position source) (framesize (pack source))))

(defmethod (setf byte-position) (position (source source))
  (setf (frame-position source) (floor position (framesize (pack source)))))

(defgeneric seek-to-frame (source position))
(defgeneric frame-count (source))

(defmethod channel-order ((source source))
  *default-channel-order*)

(defmethod duration ((source source))
  (float (/ (frame-count source)
            (samplerate (pack source)))
         0f0))

(defclass device-source (source)
  ((program-name :initform "Mixed" :initarg :program-name :accessor program-name)))

(defmethod print-object ((source device-source) stream)
  (print-unreadable-object (source stream :type T :identity T)
    (format stream "~a" (device source))))

(defclass pcm-stream-source (source)
  ((data-stream :initform NIL :accessor data-stream)
   (data-start :initform 0 :accessor data-start)
   (data-end :initform 0 :accessor data-end)))

(defmethod mix ((source pcm-stream-source))
  (declare (optimize speed))
  (with-buffer-tx (data start size (pack source) :direction :output)
    (when (< 0 size)
      (let* ((end (data-end source))
             (start (data-start source))
             (stream (data-stream source))
             (avail (max 0 (min size (- end (the (unsigned-byte 32) (max start (file-position stream)))))))
             (read (- (read-sequence data stream :start start :end (+ start avail)) start)))
        (declare (type (signed-byte 32) end start read))
        (cond ((< 0 read)
               (incf (the (unsigned-byte 32) (byte-position source)) read)
               (finish read))
              ((<= (available-read (pack source)) 2)
               (setf (done-p source) T)))))))

(defmethod end ((source pcm-stream-source))
  (close (data-stream source))
  (setf (data-stream source) NIL))

(defmethod free ((source pcm-stream-source))
  (when (data-stream source)
    (close (data-stream source))
    (setf (data-stream source) NIL)))

(defmethod seek-to-frame ((source pcm-stream-source) position)
  (file-position (data-stream source)
                 (min (data-end source)
                      (+ (data-start source) (max 0 (* position (framesize (pack source))))))))

(defmethod frame-count ((source pcm-stream-source))
  (/ (- (data-end source) (data-start source)) (framesize (pack source))))

(defclass pcm-vector-source (source)
  ((data-buffer :initform NIL :accessor data-buffer)
   (byte-position :initform 0 :accessor byte-position)
   (framesize :initform 0 :accessor framesize)))

(defmethod start ((source pcm-vector-source)))

(defmethod mix ((source pcm-vector-source))
  (declare (optimize speed))
  (let ((pack (pack source)))
    (with-buffer-tx (data start size pack :direction :output)
      (when (< 0 size)
        (let* ((buffer (data-buffer source))
               (index (byte-position source))
               (avail (max 0 (min size (- (length buffer) index)))))
          (declare (type (simple-array (unsigned-byte 8) (*)) data buffer))
          (declare (type (unsigned-byte 32) index))
          (cond ((< 0 avail)
                 (replace data buffer :start1 start :start2 index :end1 (+ start avail))
                 (setf (byte-position source) (+ index avail))
                 (finish avail))
                ((<= (available-read pack) 2)
                 (setf (done-p source) T))))))))

(defmethod end ((source pcm-vector-source)))

(defmethod seek-to-frame ((source pcm-vector-source) position)
  (setf (byte-position source) (max 0 (min (length (data-buffer source))
                                           (* position (framesize source))))))

(defmethod frame-position ((source pcm-vector-source))
  (/ (byte-position source)
     (framesize source)))

(defmethod frame-count ((source pcm-vector-source))
  (/ (length (data-buffer source))
     (framesize source)))
