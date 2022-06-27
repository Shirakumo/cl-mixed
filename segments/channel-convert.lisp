#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass channel-convert (segment)
  ()
  (:default-initargs
   :in (error "IN required.")
   :out (error "OUT required.")
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment channel-convert) &key in out samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-channel-convert in out samplerate (handle segment))))

(defun make-channel-convert (&rest args &key in out samplerate)
  (declare (ignore in out samplerate))
  (apply #'make-instance 'channel-convert args))

(define-field-accessor channel-count-in channel-convert mixed:channel_t)
(define-field-accessor channel-count-out channel-convert mixed:channel_t)

(defmethod (setf field) :after (value (field (eql :channel-count-in)) (segment channel-convert))
  (setf (slot-value segment 'inputs) (adjust-array (inputs segment) value)))

(defmethod (setf field) :after (value (field (eql :channel-count-out)) (segment channel-convert))
  (setf (slot-value segment 'outputs) (adjust-array (outputs segment) value)))
