(in-package #:org.shirakumo.fraf.mixed)

(defclass quantize (segment)
  ()
  (:default-initargs
   :steps (error "STEPS required.")))

(defmethod initialize-instance :after ((segment quantize) &key steps bypass wet)
  (with-error-on-failure ()
    (mixed:make-segment-quantize steps (handle segment)))
  (when wet (setf (wet segment) wet))
  (setf (bypass segment) bypass))

(defun make-quantize (&rest args &key steps)
  (declare (ignore steps))
  (apply #'make-instance 'quantize args))

(define-field-accessor steps quantize :uint32 :quantize-steps)
(define-field-accessor bypass quantize :bool :bypass)
(define-field-accessor wet quantize :float :mix)
