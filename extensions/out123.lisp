#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.out123
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:out123 #:org.shirakumo.fraf.out123))
  (:export
   #:out123-drain))
(in-package #:org.shirakumo.fraf.mixed.out123)

(defclass out123-drain (mixed:drain)
  ((out :initform (out123:make-output NIL) :accessor out)))

(defmethod initialize-instance :after ((drain out123-drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix)))

(defmethod mixed:start ((drain out123-drain))
  (out123:connect (out drain))
  (out123:start (out drain) :rate (mixed:target-samplerate drain) :channels 2 :encoding :float)
  (setf (mixed:samplerate (mixed:pack drain)) (out123:rate (out drain)))
  (setf (mixed:encoding (mixed:pack drain)) (out123:encoding (out drain))))

(cffi:defcallback mix :int ((segment :pointer))
  (let ((drain (mixed:pointer->object segment)))
    (mixed:with-buffer-tx (data start end (mixed:pack drain))
      (mixed:finish (out123:play-directly (out drain) (mixed:data-ptr) (- end start))))
    1))

(defmethod mixed:end ((drain out123-drain))
  (out123:stop (out drain))
  (out123:disconnect (out drain)))
