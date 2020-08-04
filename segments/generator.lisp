#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass generator (segment)
  ()
  (:default-initargs
   :type :sine
   :frequency 440
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment generator) &key type frequency samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-generator type frequency samplerate (handle segment))))

(defun make-generator (&rest args &key type frequency samplerate)
  (declare (ignore type frequency samplerate))
  (apply #'make-instance 'generator args))

(define-field-accessor volume generator :float :volume)
(define-field-accessor wave-type generator mixed:generator-type :generator-type)
(define-field-accessor frequency generator :float :generator-frequency)
