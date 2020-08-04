#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass ladspa (segment)
  ()
  (:default-initargs
   :file (error "LADSPA FILE required.")
   :index 0
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment ladspa) &key file index samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-ladspa file index samplerate (handle segment))))

(defun make-ladspa (&rest args &key file (index 0) (samplerate *default-samplerate*) &allow-other-keys)
  (let ((instance (make-instance 'ladspa :file file :index index :samplerate samplerate))
        (other-args (removef args :file :index :samplerate)))
    (loop for (field value) on other-args by #'cddr
          do (setf (field field instance) value))
    instance))

(defmethod field (field (segment ladspa))
  (cffi:with-foreign-object (value-ptr :float)
    (with-error-on-failure ()
      (mixed:segment-get field value-ptr segment))
    (cffi:mem-ref value-ptr :float)))

(defmethod (setf field) (value field (segment ladspa))
  (cffi:with-foreign-object (value-ptr :float)
    (setf (cffi:mem-ref value-ptr :float) (coerce value 'single-float))
    (with-error-on-failure ()
      (mixed:segment-get field value-ptr segment)))
  value)
