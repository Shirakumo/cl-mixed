(in-package #:org.shirakumo.fraf.mixed)

(defclass speed-change (segment)
  ()
  (:default-initargs
   :speed-factor 1.0))

(defmethod initialize-instance :after ((segment speed-change) &key speed-factor bypass)
  (with-error-on-failure ()
    (mixed:make-segment-speed-change (float speed-factor 0d0) (handle segment)))
  (setf (bypass segment) bypass))

(defun make-speed-change (&rest args &key speed-factor)
  (declare (ignore speed-factor))
  (apply #'make-instance 'speed-change args))

(define-field-accessor speed-factor speed-change :double :speed-factor)
(define-field-accessor bypass speed-change :bool :bypass)
