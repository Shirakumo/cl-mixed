(defpackage #:org.shirakumo.fraf.mixed.oss
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:oss #:org.shirakumo.fraf.mixed.oss.cffi))
  (:export
   #:source
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.oss)

(defun ioctl (fd ctl parameter)
  (cffi:with-foreign-object (param :int)
    (setf (cffi:mem-ref param :int) parameter)
    (let ((result (oss:ioctl fd ctl :pointer param)))
      (when (= -1 result)
        (error "ioctl failed."))
      (cffi:mem-ref param :int))))

(defclass oss-device ()
  ((device :initform "/dev/dsp" :initarg :device :accessor device)
   (fd :initform NIL :accessor fd)))

(defmethod initialize-instance :after ((oss-device oss-device) &key)
  (let ((pack (mixed:pack oss-device))
        (fd (oss:fd-open (device oss-device) (fd-flag oss-device) :int 0)))
    (when (= -1 fd)
      (error "Failed to acquire sound device."))
    (setf (fd oss-device) fd)
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

(defmethod mixed:free ((oss-device oss-device))
  (when (fd oss-device)
    (oss:fd-close (fd oss-device))
    (setf (fd oss-device) NIL)))

(defmethod mixed:start ((oss-device oss-device)))

(defmethod mixed:end ((oss-device oss-device)))

;; In this class and in DRAIN put OSS-DEVICE first as its INITIALIZE-INSTANCE :AFTER method should be run last.
(defclass source (oss-device mixed:source) ())

(defmethod fd-flag ((source source)) :read-only)

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (let ((result (oss:fd-read (fd source) (mixed:data-ptr) size)))
      (when (< result 0)
        (error "Failed to read."))
      (mixed:finish result))))

(defclass drain (oss-device mixed:drain) ())

(defmethod fd-flag ((drain drain)) :write-only)

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (let ((result (oss:fd-write (fd drain) (mixed:data-ptr) size)))
      (when (< result 0)
        (error "Failed to write."))
      (mixed:finish result))))
