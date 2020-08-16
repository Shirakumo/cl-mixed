#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.mpg123
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:mpg123 #:org.shirakumo.fraf.mpg123))
  (:export
   #:mpg123-source))
(in-package #:org.shirakumo.fraf.mixed.mpg123)

(defclass mpg123-source (mixed:source)
  ((file :accessor file)))

(defmethod initialize-instance :after ((source mpg123-source) &key file)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle source)) (cffi:callback mix))
  (setf (file source) (mpg123:make-file file :buffer-size NIL)))

(defmethod mixed:start ((source mpg123-source))
  (mpg123:connect (file source))
  (multiple-value-bind (rate channels encoding) (mpg123:file-format file)
    (setf (mixed:samplerate (mixed:pack source)) rate)
    (setf (mixed:channels (mixed:pack source)) channels)
    (setf (mixed:encoding (mixed:pack source)) encoding)))

(cffi:defcallback mix :int ((segment :pointer))
  (let ((source (mixed:pointer->object segment)))
    (mixed:with-buffer-tx (data start end (mixed:pack source) :direction :output)
      (mixed:finish (mpg123:read-directly (file source) (mixed:data-ptr) (- end start))))))

(defmethod mixed:end ((source mpg123-source))
  (mpg123:disconnect (file source)))

(defmethod mixed:seek-to-frame ((source mp3-source) position)
  (cl-mpg123:seek (file source) position :mode :absolute :by :frame))

(defmethod mixed:frame-count ((source mp3-source))
  (cl-mpg123:frame-count (file source)))
