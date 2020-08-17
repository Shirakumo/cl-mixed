#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.winmm
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:winmm #:org.shirakumo.fraf.mixed.winmm.cffi))
  (:export
   #:winmm-error
   #:code
   #:winmm-drain))
(in-package #:org.shirakumo.fraf.mixed.winmm)

(define-condition winmm-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "WinMM error ~a" (code c)))))

(defun check-result (error)
  (unless (eql :ok error)
    (error 'winmm-error :code error)))

(defclass winmm-drain (mixed:drain)
  ((device :initform NIL :accessor device)
   (event :initform NIL :accessor event)
   (header :initform NIL :accessor header)))

(defmethod initialize-instance :after ((drain winmm-drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (cffi:use-foreign-library winmm:winmm))

(defmethod mixed:start ((drain winmm-drain))
  (unless (event drain)
    (let ((event (winmm:create-event (cffi:null-pointer) 0 0 (cffi:null-pointer))))
      (if (cffi:null-pointer-p event)
          (error 'winmm-error :cod :event-creation-failed)
          (setf (event drain) event))))
  (unless (header drain)
    (setf (header drain) (cffi:foreign-alloc '(:struct winmm:wave-header))))
  (setf (winmm:wave-header-flags (header drain)) ())
  (unless (device drain)
    (cffi:with-foreign-objects ((device :pointer)
                                (format '(:struct winmm:waveformat-ex)))
      (winmm:encode-wave-format format (mixed:target-samplerate drain) 2 :int32)
      (check-result
       (winmm:wave-out-open device winmm:WAVE-MAPPER format (event drain) (cffi:null-pointer) '(:default-device :callback-event :allow-sync)))
      (multiple-value-bind (samplerate channels encoding) (winmm:decode-wave-format format)
        (setf (mixed:samplerate (mixed:pack drain)) samplerate)
        (setf (mixed:channels (mixed:pack drain)) channels)
        (setf (mixed:encoding (mixed:pack drain)) encoding))
      (setf (device drain) (cffi:mem-ref device :pointer)))))

(defmethod mixed:mix ((drain winmm-drain))
  (let ((device (device drain))
        (header (header drain))
        (size (cffi:foreign-type-size '(:struct winmm:wave-header))))
    (mixed:with-buffer-tx (data start end (mixed:pack drain))
      (when (winmm:wave-header-flags header)
        (loop until (find :done (winmm:wave-header-flags header))
              do (winmm:wait-for-single-object (event drain) 100))
        (check-result
         (winmm:wave-out-unprepare device header size)))
      (setf (winmm:wave-header-data header) (mixed:data-ptr))
      (setf (winmm:wave-header-buffer-length header) (- end start))
      (setf (winmm:wave-header-flags header) ())
      (setf (winmm:wave-header-loops header) 0)
      (check-result
       (winmm:wave-out-prepare device header size))
      (check-result
       (winmm:wave-out-write device header size)))))

(defmethod mixed:end ((drain winmm-drain))
  (when (device drain)
    (winmm:wave-out-reset (device drain))
    (winmm:wave-out-close (device drain))
    (setf (device drain) NIL)))
