#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass fader (segment)
  ()
  (:default-initargs
   :from 0.0
   :to 1.0
   :time 1.0
   :type :cubic-in-out
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment fader) &key from to time type samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-fade from to time type samplerate (handle segment))))

(defun make-fader (&rest args &key from to time type samplerate)
  (declare (ignore from to time type samplerate))
  (apply #'make-instance 'fader args))

(define-field-accessor from fader :float :fade-from)
(define-field-accessor to fader :float :fade-to)
(define-field-accessor duration fader :float :fade-time)
(define-field-accessor fade-type fader mixed:fade-type)
(define-field-accessor bypass fader :bool :bypass)
