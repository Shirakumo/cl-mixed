#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass frequency-pass (segment)
  ()
  (:default-initargs
   :pass :low
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment frequency-pass) &key pass cutoff samplerate bypass)
  (with-error-on-failure ()
    (mixed:make-segment-frequency-pass pass cutoff samplerate (handle segment)))
  (setf (bypass segment) bypass))

(defun make-frequency-pass (&rest args &key pass cutoff samplerate)
  (declare (ignore pass cutoff samplerate))
  (apply #'make-instance 'frequency-pass args))

(define-field-accessor cutoff frequency-pass :float :frequency-cutoff)
(define-field-accessor frequency-pass frequency-pass mixed:frequency-pass :frequency-pass)
(define-field-accessor samplerate frequency-pass :float :samplerate)
(define-field-accessor bypass frequency-pass :bool :bypass)
