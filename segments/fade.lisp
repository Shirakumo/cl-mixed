#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass fade (segment)
  ()
  (:default-initargs
   :from 0.0
   :to 1.0
   :time 1.0
   :type :cubic-in-out
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment fade) &key from to time type samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-fade from to time type samplerate (handle segment))))

(defun make-fade (&rest args &key from to time type samplerate)
  (declare (ignore from to time type samplerate))
  (apply #'make-instance 'fade args))

(define-field-accessor from fade :float :fade-from)
(define-field-accessor to fade :float :fade-to)
(define-field-accessor duration fade :float :fade-time)
(define-field-accessor fade-type fade cl-mixed-cffi:fade-type)
(define-field-accessor bypass fade :bool :bypass)
