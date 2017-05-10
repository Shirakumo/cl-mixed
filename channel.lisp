#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass channel (c-object)
  ((own-data :initform (cons NIL NIL) :reader own-data))
  (:default-initargs
   :encoding :float
   :channels 2
   :layout :alternating
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((channel channel) &key data size encoding channels layout samplerate)
  (unless data
    (setf data (calloc :uchar size))
    ;; We need to use a cons here because otherwise we would have to keep a
    ;; reference to the channel object in the freeing function, making it
    ;; impossible to GC. Using a cons to keep track that circumvents this.
    (setf (car (own-data channel)) T)
    (setf (cdr (own-data channel)) data))
  (let ((handle (handle channel)))
    (setf (cl-mixed-cffi:channel-data handle) data)
    (setf (cl-mixed-cffi:channel-size handle) size)
    (setf (cl-mixed-cffi:channel-encoding handle) encoding)
    (setf (cl-mixed-cffi:channel-channels handle) channels)
    (setf (cl-mixed-cffi:channel-layout handle) layout)
    (setf (cl-mixed-cffi:channel-samplerate handle) samplerate)))

(defmethod allocate-handle ((channel channel))
  (calloc '(:struct cl-mixed-cffi:channel)))

(defmethod free-handle ((channel channel) handle)
  (let ((own (own-data channel)))
    (lambda ()
      (when (car own)
        (cffi:foreign-free (cdr own)))
      (cffi:foreign-free handle)
      (setf (pointer->object handle) NIL))))

(define-accessor data channel cl-mixed-cffi:channel-data)
(define-accessor size channel cl-mixed-cffi:channel-size)
(define-accessor encoding channel cl-mixed-cffi:channel-encoding)
(define-accessor channels channel cl-mixed-cffi:channel-channels)
(define-accessor layout channel cl-mixed-cffi:channel-layout)
(define-accessor samplerate channel cl-mixed-cffi:channel-samplerate)
