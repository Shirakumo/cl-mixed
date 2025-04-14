(in-package #:org.shirakumo.fraf.mixed)

(defclass pack (bip-buffer c-object)
  ((data :reader data)))

(defmethod initialize-instance :after ((pack pack) &key frames encoding channels samplerate)
  (let ((handle (handle pack)))
    (setf (data pack) (when frames
                        (static-vectors:make-static-vector
                         (* frames channels (samplesize encoding))
                         :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (mixed:pack-encoding handle) encoding)
    (setf (mixed:pack-channels handle) channels)
    (setf (mixed:pack-samplerate handle) samplerate)))

(defun make-pack (&key (encoding :float) (channels 2) (samplerate *default-samplerate*) (frames (floor samplerate 100)))
  (make-instance 'pack :frames frames
                       :encoding encoding
                       :channels channels
                       :samplerate samplerate))

(defmethod allocate-handle ((pack pack))
  (calloc '(:struct mixed:pack)))

(defmethod free ((pack pack))
  (when (slot-boundp pack 'data)
    (static-vectors:free-static-vector (data pack))
    (slot-makunbound pack 'data)))

(defmethod (setf data) ((data vector) (pack pack))
  (setf (mixed:pack-data (handle pack)) (static-vectors:static-vector-pointer data))
  (setf (mixed:pack-size (handle pack)) (length data))
  (setf (slot-value pack 'data) data))

(defmethod (setf data) ((data null) (pack pack))
  (setf (mixed:pack-data (handle pack)) (cffi:null-pointer))
  (setf (mixed:pack-size (handle pack)) 0)
  (slot-makunbound pack 'data))

(define-accessor size pack mixed:pack-size)
(define-accessor encoding pack mixed:pack-encoding)
(define-accessor channels pack mixed:pack-channels)
(define-accessor samplerate pack mixed:pack-samplerate)

(declaim (ftype (function (T) (values (unsigned-byte 8) &optional)) channels))
(declaim (ftype (function (T) (values (unsigned-byte 16) &optional)) framesize))
(declaim (ftype (function (T) (values (unsigned-byte 32) &optional)) samplerate))
(declaim (ftype (function (T) (values (unsigned-byte 32) &optional)) size))

(defmethod clear ((pack pack))
  (mixed:clear-pack (handle pack)))

(defmethod (setf size) :before (size (pack pack))
  (unless (= size (size pack))
    (let ((old (data pack))
          (new (static-vectors:make-static-vector size :element-type '(unsigned-byte 8))))
      (static-vectors:replace-foreign-memory
       (static-vectors:static-vector-pointer new) (static-vectors:static-vector-pointer old)
       (size pack))
      (setf (slot-value pack 'data) new)
      (setf (mixed:pack-data (handle pack)) (static-vectors:static-vector-pointer new))
      (setf (mixed:pack-size (handle pack)) (length new))
      (static-vectors:free-static-vector old)))
  size)

(defmethod transfer ((from buffer) (to pack))
  (cffi:with-foreign-objects ((buffers :pointer)
                              (volume :float))
    (setf (cffi:mem-ref buffers :pointer) (handle from))
    (setf (cffi:mem-ref volume :float) 1f0)
    (with-error-on-failure ()
      (mixed:buffer-to-pack buffers (handle to) volume 1f0))))

(defmethod transfer ((from sequence) (to pack))
  (cffi:with-foreign-objects ((buffers :pointer (length from))
                              (volume :float))
    (setf (cffi:mem-ref volume :float) 1f0)
    (do-sequence (i buffer from)
      (setf (cffi:mem-aref buffers :pointer i) (handle buffer)))
    (with-error-on-failure ()
      (mixed:buffer-to-pack buffers (handle to) volume 1f0))))

(defmethod transfer ((from pack) (to buffer))
  (cffi:with-foreign-objects ((buffers :pointer)
                              (volume :float))
    (setf (cffi:mem-ref volume :float) 1f0)
    (setf (cffi:mem-ref buffers :pointer) (handle to))
    (with-error-on-failure ()
      (mixed:buffer-from-pack (handle from) buffers volume 1f0))))

(defmethod transfer ((from pack) (to sequence))
  (cffi:with-foreign-objects ((buffers :pointer (length from))
                             (volume :float))
    (setf (cffi:mem-ref volume :float) 1f0)
    (do-sequence (i buffer to)
      (setf (cffi:mem-aref buffers :pointer i) (handle buffer)))
    (with-error-on-failure ()
      (mixed:buffer-from-pack (handle from) buffers volume 1f0))))

(defmethod framesize ((pack pack))
  (let ((handle (handle pack)))
    (* (mixed:pack-channels handle)
       (samplesize (mixed:pack-encoding handle)))))
