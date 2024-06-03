(defpackage #:org.shirakumo.fraf.mixed.qoa
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:qoa #:org.shirakumo.qoa))
  (:export
   #:source))
(in-package #:org.shirakumo.fraf.mixed.qoa)

(defclass source (mixed:source)
  ((file :accessor file)
   (frame-index :initform 0 :accessor frame-index)))

(defmethod initialize-instance :after ((source source) &key file)
  (setf (file source) (qoa:read-file file))
  (setf (mixed:samplerate (mixed:pack source)) (qoa:samplerate (file source)))
  (setf (mixed:channels (mixed:pack source)) (qoa:channels (file source)))
  (setf (mixed:encoding (mixed:pack source)) :int16))

(defmethod mixed:free ((source source))
  (when (file source)
    (setf (file source) NIL)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (multiple-value-bind (read frame-index)
          (qoa:decode-to-buffer (file source) (mixed:data-ptr) :end (truncate size 2) :frame-start (frame-index source))
        (cond ((< 0 read)
               (incf (mixed:byte-position source) (* 2 read))
               (setf (frame-index source) frame-index)
               (mixed:finish (* 2 read)))
              ((= 0 (mixed:available-read (mixed:pack source)))
               (setf (mixed:done-p source) T)))))))

(defmethod mixed:end ((source source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (setf (frame-index source) (truncate position QOA::FRAME-LENGTH)))

(defmethod mixed:frame-count ((source source))
  (qoa:samples/channel (file source)))
