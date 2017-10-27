#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass mixer (segment)
  ((sources :initform (make-array 0 :adjustable T :fill-pointer T) :accessor sources)))

(defmethod add ((buffer buffer) (segment mixer))
  (setf (input (length (inputs segment)) segment) buffer))

(defmethod withdraw ((buffer buffer) (segment mixer))
  (setf (input (position buffer (inputs segment)) segment) NIL))

(defmethod input-field ((field (eql :source)) location (segment mixer))
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get-in field location ptr (handle segment)))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf input-field) ((value segment) (field (eql :source)) location (segment mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:segment-set-in field location (handle value) (handle segment)))
  (vector-insert-pos location value (sources segment))
  value)

(defmethod (setf input-field) :after ((value null) (field (eql :source)) location (segment mixer))
  (vector-remove-pos location (sources segment)))

(defmethod source ((location integer) (segment mixer))
  (input-field :source location segment))

(defmethod source ((buffer buffer) (segment mixer))
  (input-field :source (position buffer (inputs segment)) segment))

(defmethod (setf source) ((value segment) (location integer) (segment mixer))
  (setf (input-field :source location segment) value))

(defmethod (setf source) ((value segment) (buffer buffer) (segment mixer))
  (setf (input-field :source (position buffer (inputs segment)) segment) value))
