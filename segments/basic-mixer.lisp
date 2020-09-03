#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass basic-mixer (segment)
  ((channels :initarg :channels :initform 2 :accessor channels)))

(defmethod initialize-instance :after ((mixer basic-mixer) &key)
  (with-error-on-failure ()
    (mixed:make-segment-basic-mixer (channels mixer) (handle mixer))))

(defun make-basic-mixer (channels)
  (make-instance 'basic-mixer :channels channels))

(define-field-accessor volume basic-mixer :float :volume)

(defmethod add ((new segment) (segment basic-mixer))
  (let ((buffers (outputs new))
        (location (length (inputs segment))))
    (loop for i from 0 below (channels segment)
          do (setf (input-field :buffer (+ i location) segment) (aref buffers i)))
    new))

(defmethod withdraw ((old segment) (segment basic-mixer))
  (let ((buffers (outputs old))
        (inputs (inputs segment)))
    (loop for i from 0 below (channels segment)
          for location = (position (aref buffers i) inputs)
          do (setf (input-field :buffer location segment) NIL))
    old))
