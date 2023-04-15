#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.opus
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:opus #:org.shirakumo.fraf.opus)
   (#:opus-cffi #:org.shirakumo.fraf.opus.cffi))
  (:export
   #:source))
(in-package #:org.shirakumo.fraf.mixed.opus)

(defclass source (mixed:source)
  ((file :initform NIL :accessor file)))

(defmethod initialize-instance :after ((source source) &key file)
  (setf (file source) (opus:open file))
  (setf (mixed:samplerate (mixed:pack source)) (opus:samplerate (file source)))
  (setf (mixed:channels (mixed:pack source)) (opus:channels (file source)))
  (setf (mixed:encoding (mixed:pack source)) :float))

(defmethod mixed:free ((source source))
  (when (file source)
    (opus:close (file source))
    (setf (file source) NIL)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (let ((file (file source)))
    (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
      (when (< 0 size)
        (let* ((frames (opus:check-return file (opus-cffi:read-float (opus:handle file) (mixed:data-ptr) size (cffi:null-pointer))))
               (read (* frames 4 (opus:channels file))))
          (cond ((< 0 read)
                 (incf (mixed:frame-position source) frames)
                 (mixed:finish read))
                ((= 0 (mixed:available-read (mixed:pack source)))
                 (setf (mixed:done-p source) T))))))))

(defmethod mixed:end ((source source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (setf (opus:index (file source)) position))

(defmethod mixed:frame-count ((source source))
  (opus:sample-count (file source)))
