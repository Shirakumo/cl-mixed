#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass gate (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment gate) &key samplerate open-threshold close-threshold attack hold release)
  (with-error-on-failure ()
    (mixed:make-segment-gate samplerate (handle segment)))
  (when open-threshold (setf (open-threshold segment) open-threshold))
  (when close-threshold (setf (close-threshold segment) close-threshold))
  (when attack (setf (attack segment) attack))
  (when hold (setf (hold segment) hold))
  (when release (setf (release segment) release)))

(defun make-gate (&rest args &key samplerate)
  (declare (ignore samplerate))
  (apply #'make-instance 'gate args))

(define-field-accessor samplerate gate :uint32 :samplerate)
(define-field-accessor open-threshold gate :float :gate-open-threshold)
(define-field-accessor close-threshold gate :float :gate-close-threshold)
(define-field-accessor attack gate :float :gate-attack)
(define-field-accessor hold gate :float :gate-hold)
(define-field-accessor release gate :float :gate-release)
(define-field-accessor bypass gate :bool :bypass)
