#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass distributor (segment)
  ())

(defmethod initialize-instance :after ((segment distributor) &key)
  (with-error-on-failure ()
    (mixed:make-segment-distribute (handle segment))))

(defun make-distributor (&rest args &key)
  (apply #'make-instance 'distributor args))

