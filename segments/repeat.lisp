#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass repeat (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment repeat) &key time samplerate bypass)
  (with-error-on-failure ()
    (mixed:make-segment-repeat (float time 0f0) samplerate (handle segment)))
  (setf (bypass segment) bypass))

(defun make-repeat (&rest args &key time samplerate)
  (declare (ignore time samplerate))
  (apply #'make-instance 'repeat args))

(define-field-accessor duration repeat :float :repeat-time)
(define-field-accessor repeat-mode repeat mixed:repeat-mode :repeat-mode)
(define-field-accessor samplerate repeat :float :samplerate)
(define-field-accessor bypass repeat :bool :bypass)
