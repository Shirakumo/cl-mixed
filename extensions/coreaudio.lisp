#|
This file is a part of cl-mixed
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.coreaudio
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:coreaudio #:org.shirakumo.fraf.mixed.coreaudio.cffi))
  (:export
   #:coreaudio-error
   #:code
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.coreaudio)

(define-condition coreaudio-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "CoreAudio error ~d" (code c)))))

(defmacro with-error (() &body body)
  (let ((error (gensym "ERROR")))
    `(let ((,error (progn ,@body)))
       (when (/= ,error 0)
         (error 'coreaudio-error :code ,error)))))

(defmacro with-no-interrupts (() &body body)
  ;; On SBCL using WITHOUT-INTERRUPTS would cause interrupts
  ;; to be processed explicitly on exit. We want to avoid that.
  #+sbcl `(let ((sb-sys:*interrupts-enabled* NIL)
                (sb-kernel:*gc-inhibit* T))
            ,@body)
  #+ccl `(ccl:without-interrupts
           ,@body)
  #-(or sbcl ccl) `(progn ,@body))

(defun create-component-description (description)
  ;; This is always the same. Why we need this at all, I don't know. #justapplethings
  (setf (coreaudio:audio-component-description-component-type description)
        coreaudio:kAudioUnitType_Output)
  (setf (coreaudio:audio-component-description-component-sub-type description)
        coreaudio:kAudioUnitSubType_DefaultOutput)
  (setf (coreaudio:audio-component-description-component-manufacturer description)
        coreaudio:kAudioUnitManufacturer_Apple)
  (setf (coreaudio:audio-component-description-component-flags description)
        0)
  (setf (coreaudio:audio-component-description-component-flags-mask description)
        0)
  description)

(defun create-stream-description (stream samplerate channels format)
  (setf (coreaudio:audio-stream-basic-description-sample-rate stream)
        (coerce samplerate 'double-float))
  (setf (coreaudio:audio-stream-basic-description-format-id stream)
        coreaudio:kAudioFormatLinearPCM)
  (setf (coreaudio:audio-stream-basic-description-format-flags stream)
        (ecase format
          ((:double :float) '(:native :float :packed))
          ((:int8 :int16 :int24 :int32 :int64) '(:native :signed :packed))
          ((:uint8 :uint16 :uint24 :uint32 :uint64) '(:native :packed))))
  (setf (coreaudio:audio-stream-basic-description-bytes-per-frame stream)
        (* channels (mixed:samplesize format)))
  (setf (coreaudio:audio-stream-basic-description-channels-per-frame stream)
        channels)
  (setf (coreaudio:audio-stream-basic-description-bits-per-channel stream)
        (* 8 (mixed:samplesize format)))
  (setf (coreaudio:audio-stream-basic-description-bytes-per-packet stream)
        (coreaudio:audio-stream-basic-description-bytes-per-frame stream))
  (setf (coreaudio:audio-stream-basic-description-frames-per-packet stream)
        1)
  stream)

(defun decode-stream-description (stream)
  (let ((flags (coreaudio:audio-stream-basic-description-format-flags stream))
        (bits (coreaudio:audio-stream-basic-description-bits-per-channel stream)))
    (list (round (coreaudio:audio-stream-basic-description-sample-rate stream))
          (coreaudio:audio-stream-basic-description-channels-per-frame stream)
          (cond ((find :float flags)
                 (ecase bits (32 :float) (64 :double)))
                ((find :signed flags)
                 (ecase bits (8 :int8) (16 :int16) (24 :int24) (32 :int32) (64 :int64)))
                (T
                 (ecase bits (8 :uint8) (16 :uint16) (24 :uint24) (32 :uint32) (64 :uint64)))))))

(defun create-callback-description (callback data)
  (setf (coreaudio:au-render-callback-struct-input-proc callback)
        (cffi:callback buffer-render))
  (setf (coreaudio:au-render-callback-struct-input-proc-ref-con callback)
        data)
  callback)

(defun get-default-device-sample-rate ()
  (cffi:with-foreign-objects ((address '(:struct coreaudio::audio-object-property-address))
                              (size :uint32)
                              (id :uint32)
                              (rate :double))
    (setf (coreaudio::audio-object-property-address-selector address) 1682929012) ;; 'dOut' as int.
    (setf (coreaudio::audio-object-property-address-scope address) 1735159650) ;; 'glob' as int.
    (setf (coreaudio::audio-object-property-address-element address) 0)
    (setf (cffi:mem-ref size :uint32) (cffi:foreign-type-size :uint32))
    (with-error ()
      (coreaudio::audio-object-get-property-data 1 address 0 (cffi:null-pointer) size id))
    (setf (coreaudio::audio-object-property-address-selector address) 1853059700) ;; 'nsrt' as int.
    (setf (coreaudio::audio-object-property-address-scope address) 1735159650) ;; 'glob' as int.
    (setf (coreaudio::audio-object-property-address-element address) 0)
    (setf (cffi:mem-ref size :uint32) (cffi:foreign-type-size :double))
    (with-error ()
      (coreaudio::audio-object-get-property-data (cffi:mem-ref id :uint32) address 0 (cffi:null-pointer) size rate))
    (truncate (cffi:mem-ref rate :double))))

(defclass drain (mixed:drain)
  ((audio-unit :initform NIL :accessor audio-unit)
   (semaphore :initform (bt:make-semaphore :name "CoreAudio sync") :reader semaphore)))

(defmethod initialize-instance :after ((drain drain) &key)
  (cffi:use-foreign-library coreaudio:audio-unit)
  (cffi:use-foreign-library coreaudio:audio-toolbox)
  (cffi:with-foreign-objects ((description '(:struct coreaudio:audio-component-description))
                              (stream '(:struct coreaudio:audio-stream-basic-description))
                              (callback '(:struct coreaudio:au-render-callback-struct))
                              (unit 'coreaudio:audio-unit)
                              (frames :uint32)
                              (size :uint32))
    ;; Search for device
    (let ((component (coreaudio:audio-component-find-next (cffi:null-pointer) (create-component-description description))))
      (when (cffi:null-pointer-p component)
        (error "No component found."))
      (with-error ()
        (coreaudio:audio-component-instance-new component unit))
      (let ((unit (cffi:mem-ref unit :pointer))
            (pack (mixed:pack drain)))
        (setf (mixed:samplerate pack) (get-default-device-sample-rate))
        (format *error-output* "~& [CoreAudio] Matching default device sample rate ~d~%" (mixed:samplerate pack))
        (setf (cffi:mem-ref size :uint32) (cffi:foreign-type-size '(:struct coreaudio:audio-stream-basic-description)))
        (with-error ()
          (coreaudio:audio-unit-get-property unit :stream-format :in 0 stream size))
        (format *error-output* "~& [CoreAudio] Audio unit is preset to ~a~%" (decode-stream-description stream))
        ;; Set unit properties
        (create-callback-description callback (mixed:handle drain))
        (with-error ()
          (coreaudio:audio-unit-set-property unit :render-callback :in 0 callback (cffi:foreign-type-size '(:struct coreaudio:au-render-callback-struct))))
        (create-stream-description stream (mixed:samplerate pack) (mixed:channels pack) (mixed:encoding pack))
        (with-error ()
          (coreaudio:audio-unit-set-property unit :stream-format :in 0 stream (cffi:foreign-type-size '(:struct coreaudio:audio-stream-basic-description))))
        ;; Read back actual properties
        (with-error ()
          (coreaudio:audio-unit-get-property unit :stream-format :in 0 stream size))
        (format *error-output* "~& [CoreAudio] Audio unit is configured to ~a~%" (decode-stream-description stream))
        (destructuring-bind (samplerate channels encoding) (decode-stream-description stream)
          (setf (mixed:samplerate pack) samplerate)
          (setf (mixed:channels pack) channels)
          (setf (mixed:encoding pack) encoding))
        (setf (cffi:mem-ref size :uint32) (cffi:foreign-type-size :uint32))
        (with-error ()
          (coreaudio:audio-unit-get-property unit :maximum-frames-per-slice :global 0 frames size))
        (setf (mixed:size pack) (* (cffi:mem-ref frames :uint32) (mixed:framesize pack)))
        (format *error-output* "~& [CoreAudio] Resizing pack to fit ~a~%" (mixed:size pack))
        ;; Fire it up!
        (float-features:with-float-traps-masked T
          (with-error ()
            (coreaudio:audio-unit-initialize unit)))
        (setf (audio-unit drain) unit)))))

(defmethod mixed:free ((drain drain))
  (when (audio-unit drain)
    (float-features:with-float-traps-masked T
      (coreaudio:audio-unit-uninitialize (audio-unit drain))
      (coreaudio:audio-component-instance-dispose (audio-unit drain)))))

(defmethod mixed:start ((drain drain))
  (float-features:with-float-traps-masked T
    (with-error ()
      (coreaudio:audio-output-unit-start (audio-unit drain)))))

(cffi:defcallback buffer-render coreaudio:os-status ((handle :pointer)
                                                     (action-flags :pointer)
                                                     (time-stamp :pointer)
                                                     (bus-number :uint32)
                                                     (frames :uint32)
                                                     (io-data :pointer))
  (declare (ignore action-flags time-stamp bus-number))
  (declare (type (unsigned-byte 32) frames))
  (declare (optimize speed))
  (let* ((drain (mixed:pointer->object handle))
         (pack (mixed:pack drain))
         (bytes (* frames (mixed:framesize pack)))
         (buffer (cffi:foreign-slot-pointer io-data '(:struct coreaudio:audio-buffer-list) 'coreaudio::buffers)))
    ;; We /have/ to adjust the size of the pack to fit what they request here or we're //fucked// and will
    ;; underrun on every callback.
    (when (< (mixed:size pack) bytes)
      (format *error-output* "~& [CoreAudio] Requested ~d bytes of data, resizing buffer.~%" bytes)
      (setf (mixed:size pack) bytes))
    (mixed:with-buffer-tx (data start size pack :size bytes)
      (static-vectors:replace-foreign-memory (coreaudio:audio-buffer-data buffer) (mixed:data-ptr) size)
      (mixed:finish size))
    (bt:signal-semaphore (semaphore drain)))
  coreaudio:no-err)

(defmethod mix ((drain drain))
  (when (= 0 (mixed:available-write (mixed:pack drain)))
    (bt:wait-on-semaphore (semaphore drain) :timeout 0.1)))

(defmethod end ((drain drain))
  (float-features:with-float-traps-masked T
    (coreaudio:audio-output-unit-stop (audio-unit drain))))
