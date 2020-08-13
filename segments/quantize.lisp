#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass quantize (segment)
  ()
  (:default-initargs
   :steps (error "STEPS required.")))

(defmethod initialize-instance :after ((segment quantize) &key steps)
  (with-error-on-failure ()
    (mixed:make-segment-quantize steps (handle segment))))

(defun make-quantize (&rest args &key steps)
  (declare (ignore steps))
  (apply #'make-instance 'quantize args))

(define-field-accessor steps quantize :uint32 :quantize-steps)
(define-field-accessor bypass quantize :bool :bypass)
