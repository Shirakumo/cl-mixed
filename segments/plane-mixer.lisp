#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass plane-mixer (mixer)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((plane plane-mixer) &key samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-plane-mixer samplerate (handle plane))))

(defun make-plane-mixer (&rest args &key (samplerate *default-samplerate*) soundspeed doppler-factor min-distance max-distance rolloff attenuation)
  (declare (ignore soundspeed doppler-factor min-distance max-distance rolloff attenuation))
  (let ((instance (make-instance 'plane-mixer :samplerate samplerate)))
    (loop for (field value) on args by #'cddr
          do (unless (eql field :samplerate)
               (setf (field field instance) value)))
    instance))

(defmethod channels ((mixer plane-mixer)) 1)

(define-vector-field-accessor location plane-mixer :plane-location 2)
(define-vector-field-accessor velocity plane-mixer :plane-velocity 2)

(define-input-vector-field-accessor input-location plane-mixer :location :plane-location 2)
(define-input-vector-field-accessor input-velocity plane-mixer :velocity :plane-velocity 2)
(define-input-field-accessor input-min-distance plane-mixer :min-distance :space-min-distance :float)
(define-input-field-accessor input-max-distance plane-mixer :max-distance :space-max-distance :float)
(define-input-field-accessor input-rolloff plane-mixer :rolloff :space-rolloff :float)

(define-field-accessor soundspeed plane-mixer :float :space-soundspeed)
(define-field-accessor doppler-factor plane-mixer :float :space-doppler-factor)
(define-field-accessor min-distance plane-mixer :float :space-min-distance)
(define-field-accessor max-distance plane-mixer :float :space-max-distance)
(define-field-accessor rolloff plane-mixer :float :space-rolloff)
(define-field-accessor volume plane-mixer :float :volume)

(defmethod field ((field (eql :attenuation)) (segment plane-mixer))
  (cffi:with-foreign-object (value-ptr :pointer)
    (with-error-on-failure ()
      (mixed:segment-get :plane-attenuation value-ptr (handle segment)))
    (loop with int = (cffi:mem-ref value-ptr :int)
          for keyword in (cffi:foreign-enum-keyword-list 'mixed:attenuation)
          do (when (= int (cffi:foreign-enum-value 'mixed:attenuation keyword))
               (return keyword))
          finally (return (cffi:mem-ref value-ptr :pointer)))))

(defmethod (setf field) (value (field (eql :attenuation)) (segment plane-mixer))
  (cffi:with-foreign-object (value-ptr :pointer)
    (etypecase value
      (keyword
       (setf (cffi:mem-ref value-ptr 'mixed:size_t)
             (cffi:foreign-enum-value 'mixed:attenuation value)))
      (cffi:foreign-pointer
       (setf (cffi:mem-ref value-ptr :pointer) value)))
    (with-error-on-failure ()
      (mixed:segment-set :plane-attenuation value-ptr (handle segment))))
  value)

(defmethod attenuation ((plane plane-mixer))
  (field :attenuation plane))

(defmethod (setf attenuation) (value (plane plane-mixer))
  (setf (field :attenuation plane) value))

(defmethod add ((new segment) (segment plane-mixer))
  (setf (input-field :buffer T segment) (output 0 new))
  new)

(defmethod withdraw ((old segment) (segment plane-mixer))
  (setf (input-field :buffer (position (output 0 old) (inputs segment)) segment) NIL)
  old)
