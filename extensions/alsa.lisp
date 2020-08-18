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
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.alsa)

(define-condition alsa-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "Pulse error ~d: ~a"
                                 (code c) (alsa:strerror (code c))))))

(defun check-result (result)
  (if (< result 0)
      (error 'alsa-error :code result)
      result))

(defclass drain (mixed:drain)
  ((pcm :initform NIL :accessor pcm)))

(defmethod initialize-instance :after ((drain drain) &key)
  (cffi:use-foreign-library alsa:libasound))

(defmethod mixed:start ((drain drain))
  (unless (pcm drain)
    (cffi:with-foreign-objects ((pcm :pointer)
                                (params :uint8 (alsa:pcm-hw-params-size))
                                (format 'alsa:pcm-format)
                                (channels :uint)
                                (rate :uint)
                                (dir :int))
      (check-result
       (alsa:pcm-open pcm "default" :playback 0))
      (let ((pcm (cffi:mem-ref pcm :pointer))
            (pack (mixed:pack drain)))
        (check-result
          (alsa:pcm-set-params pcm :float :rw-interleaved
                               (mixed:channels pack)
                               (mixed:samplerate pack)
                               1 1000))
        ;; Extract actual parameters now.
        (check-result
          (alsa:pcm-hw-params-current pcm params))
        (check-result
          (alsa:pcm-hw-params-get-format params format))
        (check-result
          (alsa:pcm-hw-params-get-channels params channels))
        (check-result
          (alsa:pcm-hw-params-get-rate params rate dir))
        (setf (mixed:encoding pack) (cffi:mem-ref format 'alsa:pcm-format))
        (setf (mixed:channels pack) (cffi:mem-ref channels :uint))
        (setf (mixed:samplerate pack) (cffi:mem-ref rate :uint))
        (setf (pcm drain) pcm)))))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start end (mixed:pack drain))
    (let* ((framesize (mixed:framesize (mixed:pack drain)))
           (played (alsa:pcm-writei (pcm drain) (mixed:data-ptr) (/ (- end start) framesize))))
      (if (< played 0)
          (check-result
           (alsa:pcm-recover (pcm drain) played 0))
          (mixed:finish (max 0 (* played framesize)))))))

(defmethod mixed:end ((drain drain))
  (when (pcm drain)
    (alsa:pcm-drain (pcm drain))
    (alsa:pcm-close (pcm drain))
    (setf (pcm drain) NIL)))
