#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.drains.coreaudio
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:coreaudio #:org.shirakumo.fraf.mixed.coreaudio.cffi))
  (:export
   #:coreaudio-error
   #:code
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.drains.coreaudio)

(define-condition coreaudio-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "CoreAudio error ~d" (code c)))))

(defmacro with-error (() &body body)
  (let ((error (gensym "ERROR")))
    `(let ((,error (progn ,@body)))
       (when (/= ,error 0)
         (error 'coreaudio-error :code ,error)))))

(defclass drain (mixed:drain)
  ((audio-unit :initform NIL :accessor audio-unit)))

(defmethod initialize-instance :after ((drain drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (cffi:use-foreign-library coreaudio:audio-unit)
  (cffi:use-foreign-library coreaudio:audio-toolbox))

(defmacro with-no-interrupts (() &body body)
  ;; On SBCL using WITHOUT-INTERRUPTS would cause interrupts
  ;; to be processed explicitly on exit. We want to avoid that.
  #+sbcl `(let ((sb-sys:*interrupts-enabled* NIL)
                (sb-kernel:*gc-inhibit* T))
            ,@body)
  #+ccl `(ccl:without-interrupts
           ,@body)
  #-(or sbcl ccl) `(progn ,@body))

(cffi:defcallback buffer-render mixed-coreaudio-cffi:os-status ((handle :pointer)
                                                                (action-flags :pointer)
                                                                (time-stamp :pointer)
                                                                (bus-number :uint32)
                                                                (frames :uint32)
                                                                (io-data :pointer))
  (declare (ignore action-flags time-stamp bus-number))
  (with-no-interrupts ()
    (let* ((pack (mixed:pack (mixed:pointer->object handle)))
           (bytes (* frames (mixed:channels pack) (cffi:foreign-type-size :float)))
           (buffer (cffi:foreign-slot-pointer io-data
                                              '(:struct mixed-coreaudio-cffi:audio-buffer-list)
                                              'harmony-coreaudio-cffi::buffers)))
      (mixed:with-buffer-tx ((data start end pack :size bytes))
        (memcpy (harmony-coreaudio-cffi:audio-buffer-data buffer) (mixed:data-ptr) (- start end))
        (mixed:finish (- start end)))))
  harmony-coreaudio-cffi:no-err)

(defun create-component-description (description)
  ;; This is always the same. Why we need this at all, I don't know. #justapplethings
  (setf (harmony-coreaudio-cffi:audio-component-description-component-type description)
        harmony-coreaudio-cffi:kAudioUnitType_Output)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-sub-type description)
        harmony-coreaudio-cffi:kAudioUnitSubType_DefaultOutput)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-manufacturer description)
        harmony-coreaudio-cffi:kAudioUnitManufacturer_Apple)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-flags description)
        0)
  (setf (harmony-coreaudio-cffi:audio-component-description-component-flags-mask description)
        0))

(defun create-stream-description (stream samplerate channels)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-sample-rate stream)
        (coerce samplerate 'double-float))
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-format-id stream)
        harmony-coreaudio-cffi:kAudioFormatLinearPCM)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-format-flags stream)
        harmony-coreaudio-cffi:kAudioFormatFlagsNativeFloatPacked)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-bytes-per-packet stream)
        (* 4 channels))
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-frames-per-packet stream)
        1)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-bytes-per-frame stream)
        (* 4 channels))
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-channels-per-frame stream)
        channels)
  (setf (harmony-coreaudio-cffi:audio-stream-basic-description-bits-per-channel stream)
        (* 4 8)))

(defun create-callback-description (callback data)
  (setf (harmony-coreaudio-cffi:au-render-callback-struct-input-proc callback)
        (cffi:callback buffer-render))
  (setf (harmony-coreaudio-cffi:au-render-callback-struct-input-proc-ref-con callback)
        data))

(defmethod mixed:start ((drain drain))
  (cffi:with-foreign-objects ((description '(:struct harmony-coreaudio-cffi:audio-component-description))
                              (stream '(:struct harmony-coreaudio-cffi:audio-stream-basic-description))
                              (callback '(:struct harmony-coreaudio-cffi:au-render-callback-struct))
                              (unit 'harmony-coreaudio-cffi:audio-unit))
    (let ((pack (mixed:pack drain)))
      (setf (mixed:encoding pack) :float)
      ;; Prepare needed information
      (create-component-description description)
      (create-stream-description stream (mixed:samplerate pack) (mixed:channels pack))
      (create-callback-description callback (mixed:handle drain))
      ;; Search for device
      (let ((component (harmony-coreaudio-cffi:audio-component-find-next (cffi:null-pointer) description)))
        (when (cffi:null-pointer-p component)
          (error "No component found."))
        (with-error ()
          (harmony-coreaudio-cffi:audio-component-instance-new component unit))
        (let ((unit (cffi:mem-ref unit :pointer)))
          ;; Set unit properties
          (with-error ()
            (harmony-coreaudio-cffi:audio-unit-set-property
             unit
             harmony-coreaudio-cffi:kAudioUnitProperty_SetRenderCallback
             harmony-coreaudio-cffi:kAudioUnitScope_Input
             0
             callback
             (cffi:foreign-type-size '(:struct harmony-coreaudio-cffi:au-render-callback-struct))))
          (with-error ()
            (harmony-coreaudio-cffi:audio-unit-set-property
             unit
             harmony-coreaudio-cffi:kAudioUnitProperty_StreamFormat
             harmony-coreaudio-cffi:kAudioUnitScope_Input
             0
             stream
             (cffi:foreign-type-size '(:struct harmony-coreaudio-cffi:audio-stream-basic-description))))
          ;; FIXME: check for actual properties selected and propagate to pack?
          ;; Fire it up!
          (float-features:with-float-traps-masked ()
            (with-error ()
              (harmony-coreaudio-cffi:audio-unit-initialize unit))
            (with-error ()
              (harmony-coreaudio-cffi:audio-output-unit-start unit)))
          (setf (audio-unit drain) unit))))))

(cffi:defcallback mix :int ((segment :pointer)) 1)

(defmethod end ((drain drain))
  (let ((unit (audio-unit drain)))
    (when unit
      (with-float-traps-masked ()
        (harmony-coreaudio-cffi:audio-output-unit-stop unit)
        (harmony-coreaudio-cffi:audio-unit-uninitialize unit)
        (harmony-coreaudio-cffi:audio-component-instance-dispose unit)))))
