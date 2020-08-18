#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.oss
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:oss #:org.shirakumo.fraf.mixed.oss.cffi))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.oss)

(defclass drain (mixed:drain)
  ((device :initform "/dev/dsp" :initarg :device :accessor device)
   (fd :initform NIL :accessor fd)))

(defmethod initialize-instance :after ((drain drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix)))

(defun ioctl (fd ctl parameter)
  (cffi:with-foreign-object (param :int)
    (setf (cffi:mem-ref param :int) parameter)
    (let ((result (oss:ioctl fd ctl :pointer param)))
      (when (= -1 result)
        (error "ioctl failed."))
      (cffi:mem-ref param :int))))

(defmethod mixed:start ((drain drain))
  (unless (fd drain)
    (let ((pack (mixed:pack drain))
          (fd (oss:fd-open (device drain) :write-only :int 0)))
      (when (= -1 fd)
        (error "Failed to acquire sound device."))
      (let* ((format (cffi:foreign-enum-value 'oss:encoding (mixed:encoding pack)))
             (format (ioctl fd :sndctl-dsp-setfmt format)))
        (setf (mixed:encoding pack) (cffi:foreign-enum-keyword 'oss:encoding format)))
      (setf (mixed:channels pack) (ioctl fd :sndctl-dsp-channels (mixed:channels pack)))
      (setf (mixed:samplerate pack) (ioctl fd :sndctl-dsp-speed (mixed:samplerate pack)))
      (setf (fd drain) fd))))

(cffi:defcallback mix :int ((segment :pointer))
  (let ((drain (mixed:pointer->object segment)))
    (mixed:with-buffer-tx (data start size (mixed:pack drain))
      (let ((result (oss:fd-write (fd drain) (mixed:data-ptr) size)))
        (when (< result 0)
          (error "Failed to write."))
        (mixed:finish result)))))

(defmethod mixed:end ((drain drain))
  (when (fd drain)
    (oss:fd-close (fd drain))
    (setf (fd drain) NIL)))
