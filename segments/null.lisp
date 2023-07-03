(in-package #:org.shirakumo.fraf.mixed)

(defclass void (segment)
  ())

(defmethod initialize-instance :after ((segment void) &key)
  (with-error-on-failure ()
    (mixed:make-segment-void (handle segment))))

(defun make-void (&rest args &key)
  (apply #'make-instance 'void args))

(defclass zero (segment)
  ())

(defmethod initialize-instance :after ((segment zero) &key)
  (with-error-on-failure ()
    (mixed:make-segment-zero (handle segment))))

(defun make-zero (&rest args &key)
  (apply #'make-instance 'zero args))
