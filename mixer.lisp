#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass mixer (c-object)
  ((segments :initform (make-array 0 :adjustable T :fill-pointer T) :reader segments)))

(defmethod initialize-instance :after ((mixer mixer) &key handle)
  (when handle
    ;; Attempt to back-fill.
    (let ((ptr (cl-mixed-cffi:mixer-segments handle)))
      (loop for i from 0 below (size mixer)
            do (vector-push-extend (pointer->object (cffi:mem-aref ptr :pointer i))
                                   (segments mixer))))))

(defmethod allocate-handle ((mixer mixer))
  (calloc '(:struct cl-mixed-cffi:mixer)))

(defmethod free-handle ((mixer mixer) handle)
  (lambda ()
    (cl-mixed-cffi:free-mixer handle)
    (cffi:foreign-free handle)))

(defmethod add ((segment segment) (mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-add (handle segment) (handle mixer)))
  (vector-push-extend segment (segments mixer))
  segment)

(defmethod withdraw ((segment segment) (mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-remove (handle segment) (handle mixer)))
  (loop with s = (segments mixer)
        for i from 0 below (length s)
        do (when (eq segment (aref s i))
             (loop for j from (1+ i) below (length s)
                   do (setf (aref s (1- i)) (aref s i)))
             (setf (aref s (1- (length s))) NIL)
             (decf (fill-pointer s))
             (return)))
  segment)

(defmethod start ((mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-start (handle mixer))))

(defmethod mix ((mixer mixer))
  (cl-mixed-cffi:mixer-mix (handle mixer))
  (unless (eql :no-error (cl-mixed-cffi:error))
    (error 'mixed-error)))

(defmethod end ((mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-end (handle mixer))))

(define-accessor size mixer cl-mixed-cffi:mixer-count)
