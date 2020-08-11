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
   :out (error "OUT required.")))

(defmethod initialize-instance :after ((segment channel-convert) &key in out)
  (with-error-on-failure ()
    (mixed:make-segment-channel-convert in out (handle segment))))

(defun make-channel-convert (&rest args &key in out)
  (declare (ignore in out))
  (apply #'make-instance 'channel-convert args))
