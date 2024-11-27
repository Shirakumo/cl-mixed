(defpackage #:org.shirakumo.fraf.mixed.dummy
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:source
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.dummy)

(defclass source (mixed:source)
  ())

(defmethod mixed:free ((source source)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (mixed:finish size)))

(defmethod mixed:end ((source source)))

(defclass drain (mixed:drain)
  ())

(defmethod mixed:free ((drain drain)))

(defmethod mixed:start ((drain drain)))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (sleep (/ size (mixed:samplerate (mixed:pack drain))))
    (mixed:finish size)))

(defmethod mixed:end ((drain drain)))
