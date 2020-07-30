#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass pack (c-object)
  ((own-data :initform (cons NIL NIL) :reader own-data)))

(defmethod initialize-instance :after ((pack pack) &key data size encoding channels samplerate)
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
    (setf (cl-mixed-cffi:pack-data handle) data)
    (setf (cl-mixed-cffi:pack-size handle) size)
    (setf (cl-mixed-cffi:pack-encoding handle) encoding)
    (setf (cl-mixed-cffi:pack-channels handle) channels)
    (setf (cl-mixed-cffi:pack-samplerate handle) samplerate)))

(defun make-pack (data size encoding channels samplerate)
  (make-instance 'pack :data data
                       :size size
                       :encoding encoding
                       :channels channels
                       :samplerate samplerate))

(defmethod allocate-handle ((pack pack))
  (calloc '(:struct cl-mixed-cffi:pack)))

(defmethod free-handle ((pack pack) handle)
  (let ((own (own-data pack)))
    (lambda ()
      (when (car own)
        (cffi:foreign-free (cdr own)))
      (cffi:foreign-free handle)
      (setf (pointer->object handle) NIL))))

(define-accessor data pack cl-mixed-cffi:pack-data)
(define-accessor size pack cl-mixed-cffi:pack-size)
(define-accessor encoding pack cl-mixed-cffi:pack-encoding)
(define-accessor channels pack cl-mixed-cffi:pack-channels)
(define-accessor samplerate pack cl-mixed-cffi:pack-samplerate)

(defmethod (setf size) :before (size (pack pack))
  (unless (= size (size pack))
    (cond ((car (own-data pack))
           (cffi:foreign-free (cdr (own-data pack))))
          ((= 0 (size pack))
           (setf (car (own-data pack)) T)))
    (setf (cl-mixed-cffi:pack-data (handle pack))
          (setf (cdr (own-data pack)) (calloc :uchar size)))))

(defmacro with-pack-tx ((data start end pack &key (direction :read) (size #xFFFFFFFF)) &body body)
  (let ((packg (gensym "BUFFER"))
        (sizeg (gensym "SIZE"))
        (handle (gensym "HANDLE")))
    `(let* ((,packg ,pack)
            (,data (data ,packg)))
       (ecase ,direction
         (:read
          (multiple-value-bind (,start ,end) (request-read ,packg ,size)
            (flet ((finish (,sizeg) (finish-read ,packg ,sizeg)))
              ,@body)))
         (:write
          (multiple-value-bind (,start ,end) (request-write ,packg ,size)
            (flet ((finish (,sizeg) (finish-write ,packg ,sizeg)))
              (unwind-protect
                   (progn ,@body)
                (let ((,handle (handle ,pack)))
                  (setf (mixed:pack-reserved-size ,handle) 0)
                  (setf (mixed:pack-reserved-start ,handle) 0))))))))))
