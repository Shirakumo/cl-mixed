#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.flac
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:flac #:org.shirakumo.fraf.flac))
  (:export
   #:source))
(in-package #:org.shirakumo.fraf.mixed.flac)

(defclass source (mixed:source)
  ((file :accessor file)))

(defmethod initialize-instance :after ((source source) &key file)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle source)) (cffi:callback mix))
  (setf (file source) (cl-flac:make-file file)))

(defmethod mixed:start ((source source))
  (setf (mixed:samplerate (mixed:pack source)) (flac:samplerate (file source)))
  (setf (mixed:channels (mixed:pack source)) (flac:channels (file source)))
  (setf (mixed:encoding (mixed:pack source)) :float))

(cffi:defcallback mix :int ((segment :pointer))
  (let ((source (mixed:pointer->object segment)))
    (mixed:with-buffer-tx (data start end (mixed:pack source) :direction :output)
      (let ((read (flac:read-directly (file source) (mixed:data-ptr) (- end start))))
        (incf (mixed:byte-position source) read)
        (mixed:finish read)))))

(defmethod mixed:end ((source source))
  (flac:disconnect (file source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (cl-flac:seek (file source) position))

(defmethod mixed:frame-count ((source source))
  (cl-flac:frame-count (file source)))
