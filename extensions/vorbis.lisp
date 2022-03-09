#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.vorbis
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:vorbis #:org.shirakumo.fraf.vorbis)
   (#:vorbis-cffi #:org.shirakumo.fraf.vorbis.cffi))
  (:export
   #:source))
(in-package #:org.shirakumo.fraf.mixed.vorbis)

(defclass source (mixed:source)
  ((file :initform NIL :accessor file)))

(defmethod initialize-instance :after ((source source) &key file)
  (setf (file source) (vorbis:open file))
  (setf (mixed:samplerate (mixed:pack source)) (vorbis:samplerate (file source)))
  (setf (mixed:channels (mixed:pack source)) (vorbis:channels (file source)))
  (setf (mixed:encoding (mixed:pack source)) :float))

(defmethod mixed:free ((source source))
  (when (file source)
    (vorbis:close (file source))
    (setf (file source) NIL)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (let ((file (file source)))
    (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
      (when (< 0 size)
        (let* ((channels (vorbis:channels file))
               (frames (vorbis-cffi:get-samples-float-interleaved (vorbis:handle file) channels (mixed:data-ptr) (/ size 4)))
               (read (* frames 4 channels)))
          (vorbis::check-file-for-error file)
          (cond ((< 0 read)
                 (incf (mixed:frame-position source) frames)
                 (mixed:finish read))
                ((= 0 (mixed:available-read (mixed:pack source)))
                 (setf (mixed:done-p source) T))))))))

(defmethod mixed:end ((source source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (let ((file (file source)))
    (vorbis:seek file position)))

(defmethod mixed:frame-count ((source source))
  (let ((file (file source)))
    (* (vorbis:sample-count file) (vorbis:channels file))))
