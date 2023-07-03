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
  (setf (file source) (cl-flac:make-file file))
  (setf (mixed:samplerate (mixed:pack source)) (flac:samplerate (file source)))
  (setf (mixed:channels (mixed:pack source)) (flac:channels (file source)))
  (setf (mixed:encoding (mixed:pack source)) :float))

(defmethod mixed:free ((source source))
  (when (file source)
    (cl-flac:close-file (file source))
    (setf (file source) NIL)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (let ((read (flac:read-directly (file source) (mixed:data-ptr) size)))
        (cond ((< 0 read)
               (incf (mixed:byte-position source) read)
               (mixed:finish read))
              ((= 0 (mixed:available-read (mixed:pack source)))
               (setf (mixed:done-p source) T)))))))

(defmethod mixed:end ((source source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (cl-flac:seek (file source) position))

(defmethod mixed:frame-count ((source source))
  (cl-flac:frame-count (file source)))
