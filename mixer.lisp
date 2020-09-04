#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass mixer (segment)
  ((free-locations :initform () :accessor free-locations)))

(defmethod next-location ((mixer mixer))
  (or (pop (free-locations mixer))
      (length (inputs mixer))))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) (location (eql T)) (mixer mixer))
  (setf (input-field field (next-location mixer) mixer) value))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) (location integer) (mixer mixer))
  (with-error-on-failure ()
    (mixed:segment-set-in field location (handle value) (handle mixer)))
  (cond ((< location (length (inputs mixer)))
         ;; FIXME: if explicitly set, location is not removed from FREE-LOCATIONS.
         (setf (aref (inputs mixer) location) value))
        (T
         (loop for i from (length (inputs mixer)) below location
               do (push i (free-locations mixer)))
         (vector-insert-pos location value (inputs mixer))))
  value)

(defmethod (setf input-field) ((value null) (field (eql :buffer)) (location integer) (mixer mixer))
  (with-error-on-failure ()
    (mixed:segment-set-in field location (cffi:null-pointer) (handle mixer)))
  (cond ((< location (length (inputs mixer)))
         (setf (aref (inputs mixer) location) value)
         ;; FIXME: inefficient as hell.
         (setf (free-locations mixer) (sort (list* location (free-locations mixer)) #'<)))
        (T
         (loop for i from (length (inputs mixer)) to location
               do (push i (free-locations mixer)))
         (setf (free-locations mixer) (sort (list* location (free-locations mixer)) #'<))
         (vector-insert-pos location value (inputs mixer))))
  value)
