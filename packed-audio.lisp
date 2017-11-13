#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass packed-audio (c-object)
  ((own-data :initform (cons NIL NIL) :reader own-data)))

(defmethod initialize-instance :after ((pack packed-audio) &key data size encoding channels layout samplerate)
  (unless data
    (cond ((= 0 size)
           (setf data (null-pointer)))
          (T
           (setf data (calloc :uchar size))
           ;; We need to use a cons here because otherwise we would have to keep a
           ;; reference to the channel object in the freeing function, making it
           ;; impossible to GC. Using a cons to keep track that circumvents this.
           (setf (car (own-data pack)) T)
           (setf (cdr (own-data pack)) data))))
  (let ((handle (handle pack)))
    (setf (cl-mixed-cffi:packed-audio-data handle) data)
    (setf (cl-mixed-cffi:packed-audio-size handle) size)
    (setf (cl-mixed-cffi:packed-audio-encoding handle) encoding)
    (setf (cl-mixed-cffi:packed-audio-channels handle) channels)
    (setf (cl-mixed-cffi:packed-audio-layout handle) layout)
    (setf (cl-mixed-cffi:packed-audio-samplerate handle) samplerate)))

(defun make-packed-audio (data size encoding channels layout samplerate)
  (make-instance 'packed-audio :data data
                               :size size
                               :encoding encoding
                               :channels channels
                               :layout layout
                               :samplerate samplerate))

(defmethod allocate-handle ((pack packed-audio))
  (calloc '(:struct cl-mixed-cffi:packed-audio)))

(defmethod free-handle ((pack packed-audio) handle)
  (let ((own (own-data pack)))
    (lambda ()
      (when (car own)
        (cffi:foreign-free (cdr own)))
      (cffi:foreign-free handle)
      (setf (pointer->object handle) NIL))))

(define-accessor data packed-audio cl-mixed-cffi:packed-audio-data)
(define-accessor size packed-audio cl-mixed-cffi:packed-audio-size)
(define-accessor encoding packed-audio cl-mixed-cffi:packed-audio-encoding)
(define-accessor channels packed-audio cl-mixed-cffi:packed-audio-channels)
(define-accessor layout packed-audio cl-mixed-cffi:packed-audio-layout)
(define-accessor samplerate packed-audio cl-mixed-cffi:packed-audio-samplerate)

(defmethod (setf size) :before (size (pack packed-audio))
  (when (car (own-data pack))
    (cffi:foreign-free (cdr (own-data pack)))
    (setf (cl-mixed-cffi:packed-audio-data (handle pack))
          (setf (cdr (own-data pack)) (calloc :uchar size)))))
