(in-package #:org.shirakumo.fraf.mixed)

(defclass distributor (segment)
  ())

(defmethod initialize-instance :after ((segment distributor) &key)
  (with-error-on-failure ()
    (mixed:make-segment-distribute (handle segment))))

(defun make-distributor (&rest args &key)
  (apply #'make-instance 'distributor args))

(defmethod connect ((source distributor) source-location (drain segment) drain-location &optional buffer)
  (declare (ignore buffer))
  (let ((buffer (make-instance 'buffer :virtual T)))
    (setf (output source-location source) buffer)
    (setf (input drain-location drain) buffer)
    (setf (slot-value buffer 'data) (data (input 0 source)))))
