#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.dummy
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.dummy)

(defclass drain (mixed:drain)
  ())

(defmethod initialize-instance :after ((drain drain) &key))

(defmethod mixed:free ((drain drain)))

(defmethod mixed:start ((drain drain)))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (sleep (/ size (mixed:samplerate (mixed:pack drain))))
    (mixed:finish size)))

(defmethod mixed:end ((drain drain)))
