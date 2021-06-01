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
   #:source))
(in-package #:org.shirakumo.fraf.mixed.mpg123)

(defclass source (mixed:source)
  ((file :accessor file)))

(defmethod initialize-instance :after ((source source) &key file)
  (setf (file source) (mpg123:make-file file :buffer-size NIL))
  ;; Early start to set pack properties
  (mixed:start source))

(defmethod mixed:free ((source source))
  (when (file source)
    (when (mpg123:connected (file source))
      (mpg123:disconnect (file source)))
    (setf (file source) NIL)))

(defmethod mixed:start ((source source))
  (unless (mpg123:connected (file source))
    (mpg123:connect (file source))
    (mpg123:scan (file source))
    (multiple-value-bind (rate channels encoding) (mpg123:file-format (file source))
      (setf (mixed:samplerate (mixed:pack source)) rate)
      (setf (mixed:channels (mixed:pack source)) channels)
      (setf (mixed:encoding (mixed:pack source)) encoding))))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (handler-bind ((mpg123:read-failed
                       (lambda (e)
                         (case (mpg123:error-code e)
                           (:new-format))
                         (continue e))))
        (let ((read (mpg123:read-directly (file source) (mixed:data-ptr) size)))
          (cond ((< 0 read)
                 (incf (mixed:byte-position source) read)
                 (mixed:finish read))
                ((= 0 (mixed:available-read (mixed:pack source)))
                 (setf (mixed:done-p source) T))))))))

(defmethod mixed:end ((source source))
  (mpg123:disconnect (file source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (cl-mpg123:seek (file source) position :mode :absolute :by :sample))

(defmethod mixed:frame-count ((source source))
  (cl-mpg123:sample-count (file source)))
