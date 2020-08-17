#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.alsa
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:alsa #:org.shirakumo.fraf.mixed.alsa.cffi))
  (:export
   #:alsa-error
   #:code
   #:alsa-drain))
(in-package #:org.shirakumo.fraf.mixed.alsa)

(define-condition alsa-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "Pulse error ~d: ~a"
                                 (code c) (alsa:strerror (code c))))))

(defmacro with-error (() &body body)
  (let ((error (gensym "ERROR")))
    `(let ((,error (progn ,@body)))
       (when (< ,error 0)
         (error 'alsa-error :code ,error)))))

(defclass alsa-drain (mixed:drain)
  ((pcm :initform NIL :accessor pcm)))

(defmethod initialize-instance :after ((drain alsa-drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (cffi:use-foreign-library alsa:libasound))

(defmethod mixed:start ((drain alsa-drain))
  (unless (pcm drain)
    (cffi:with-foreign-object (pcm :pointer)
      (with-error ()
        (alsa:pcm-open pcm "default" :playback 0))
      (let ((pcm (cffi:mem-ref pcm :pointer)))
        (with-error ()
          (alsa:pcm-set-params pcm :float-le :rw-interleaved 2
                                            (mixed:target-samplerate drain)
                                            1 10000)) ;; 1ms
        (setf (pcm drain) pcm)))))

(cffi:defcallback mix :int ((segment :pointer))
  (let ((drain (mixed:pointer->object segment)))
    (mixed:with-buffer-tx (data start end (mixed:pack drain))
      (let ((error (alsa:pcm-writei (pcm drain) (mixed:data-ptr) (- end start))))
        (when (< error 0)
          (setf error (alsa:pcm-recover (pcm drain) error 0))
          (when (< error 0)
            (error 'alsa-error :code error)))
        (mixed:finish error)))))

(defmethod mixed:end ((drain alsa-drain))
  (when (pcm drain)
    (with-error ()
      (alsa:pcm-drain (pcm drain)))
    (alsa:pcm-close (pcm drain))
    (setf (pcm drain) NIL)))
