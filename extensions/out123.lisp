(defpackage #:org.shirakumo.fraf.mixed.out123
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:out123 #:org.shirakumo.fraf.out123))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.out123)

(defclass drain (mixed:drain)
  ((out :initform NIL :accessor out)))

(defmethod initialize-instance :after ((drain drain) &key)
  (let* ((pack (mixed:pack drain))
         (out (out123:make-output NIL :rate (mixed:samplerate pack) :channels (mixed:channels pack) :encoding :float)))
    (out123:connect out)
    (setf (out drain) out)))

(defmethod mixed:free ((drain drain))
  (when (out drain)
    (out123:disconnect (out drain))
    (cl-out123-cffi:del (out123:handle (out drain)))
    (setf (out drain) NIL)))

(defmethod mixed:start ((drain drain))
  (out123:start (out drain))
  (setf (mixed:samplerate (mixed:pack drain)) (out123:rate (out drain)))
  (setf (mixed:encoding (mixed:pack drain)) (out123:encoding (out drain)))
  (setf (mixed:channels (mixed:pack drain)) (out123:channels (out drain))))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (mixed:finish (out123:play-directly (out drain) (mixed:data-ptr) size))))

(defmethod mixed:end ((drain drain))
  (out123:stop (out drain)))
