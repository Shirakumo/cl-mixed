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

(defun ioctl (fd ctl parameter)
  (cffi:with-foreign-object (param :int)
    (setf (cffi:mem-ref param :int) parameter)
    (let ((result (oss:ioctl fd ctl :pointer param)))
      (when (= -1 result)
        (error "ioctl failed."))
      (cffi:mem-ref param :int))))

(defclass drain (mixed:drain)
  ((device :initform "/dev/dsp" :initarg :device :accessor device)
   (fd :initform NIL :accessor fd)))

(defmethod initialize-instance :after ((drain drain) &key)
  (let ((pack (mixed:pack drain))
        (fd (oss:fd-open (device drain) :write-only :int 0)))
    (when (= -1 fd)
      (error "Failed to acquire sound device."))
    (setf (fd drain) fd)
    (let* ((format (cffi:foreign-enum-value 'oss:encoding (case (mixed:encoding pack)
                                                            (:uint24 :int24)
                                                            (:uint32 :int32)
                                                            ;; On my test device, trying to use float
                                                            ;; just resulted in ungodly garbage, so
                                                            ;; we simply deny its use here entirely.
                                                            ((:double :float) :int32)
                                                            (T (mixed:encoding pack)))))
           (format (ioctl fd :sndctl-dsp-setfmt format)))
      (setf (mixed:encoding pack) (cffi:foreign-enum-keyword 'oss:encoding format))
      (setf (mixed:channels pack) (ioctl fd :sndctl-dsp-channels (mixed:channels pack)))
      (setf (mixed:samplerate pack) (ioctl fd :sndctl-dsp-speed (mixed:samplerate pack))))))

(defmethod mixed:free ((drain drain))
  (when (fd drain)
    (oss:fd-close (fd drain))
    (setf (fd drain) NIL)))

(defmethod mixed:start ((drain drain)))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (let ((result (oss:fd-write (fd drain) (mixed:data-ptr) size)))
      (when (< result 0)
        (error "Failed to write."))
      (mixed:finish result))))

(defmethod mixed:end ((drain drain)))
