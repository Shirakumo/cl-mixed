#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass pack (bip-buffer c-object)
  ((data :reader data)))

(defmethod initialize-instance :after ((pack pack) &key frames encoding channels samplerate)
  (let* ((size (* frames channels (samplesize encoding)))
         (data (static-vectors:make-static-vector size :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (slot-value pack 'data) data)
    (let ((handle (handle pack)))
      (setf (mixed:pack-data handle) (static-vectors:static-vector-pointer data))
      (setf (mixed:pack-size handle) size)
      (setf (mixed:pack-encoding handle) encoding)
      (setf (mixed:pack-channels handle) channels)
      (setf (mixed:pack-samplerate handle) samplerate))))

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

(declaim (ftype (function (T) (unsigned-byte 8)) channels))
(declaim (ftype (function (T) (unsigned-byte 16)) framesize))
(declaim (ftype (function (T) (unsigned-byte 32)) samplerate))
(declaim (ftype (function (T) (unsigned-byte 32)) size))

(define-accessor size pack mixed:pack-size)
(define-accessor encoding pack mixed:pack-encoding)
(define-accessor channels pack mixed:pack-channels)
(define-accessor samplerate pack mixed:pack-samplerate)

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
  (cffi:with-foreign-object (buffers :pointer)
    (setf (cffi:mem-ref buffers :pointer) (handle from))
    (with-error-on-failure ()
      (mixed:buffer-to-pack buffers (handle to) 1.0))))

(defmethod transfer ((from sequence) (to pack))
  (cffi:with-foreign-object (buffers :pointer (length from))
    (do-sequence (i buffer from)
      (setf (cffi:mem-aref buffers :pointer i) (handle buffer)))
    (with-error-on-failure ()
      (mixed:buffer-to-pack buffers (handle to) 1.0))))

(defmethod transfer ((from pack) (to buffer))
  (cffi:with-foreign-object (buffers :pointer)
    (setf (cffi:mem-ref buffers :pointer) (handle to))
    (with-error-on-failure ()
      (mixed:buffer-from-pack (handle from) buffers 1.0))))

(defmethod transfer ((from pack) (to sequence))
  (cffi:with-foreign-object (buffers :pointer (length from))
    (do-sequence (i buffer to)
      (setf (cffi:mem-aref buffers :pointer i) (handle buffer)))
    (with-error-on-failure ()
      (mixed:buffer-from-pack (handle from) buffers 1.0))))

(defmethod framesize ((pack pack))
  (let ((handle (handle pack)))
    (* (mixed:pack-channels handle)
       (samplesize (mixed:pack-encoding handle)))))
