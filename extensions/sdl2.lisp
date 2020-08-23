#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.sdl2
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.sdl2)

(cffi:defcstruct (audio-spec :conc-name audio-spec-)
  (samplerate :int)
  (format :uint16)
  (channels :uint8)
  (silence :uint8)
  (samples :uint16)
  (size :uint32)
  (callback :pointer)
  (data :pointer))

(defun encode-audio-format (format)
  (let ((f 0))
    (setf (ldb (byte 8 0) f) (* 8 (mixed:samplesize format)))
    (case format
      ((:double :float)
       (setf (ldb (byte 1 8) f) 1))
      ((:int8 :int16 :int24 :int32 :int64)
       (setf (ldb (byte 1 15) f) 1)))
    f))

(defun decode-audio-format (format)
  (let ((bittage (ldb (byte 8 0) format)))
    (cond ((logbitp 8 format)
           (ecase bittage
             (64 :double) (32 :float)))
          ((logbitp 15 format)
           (ecase bittage
             (8 :int8) (16 :int16) (24 :int24) (32 :int32) (64 :int64)))
          (T
           (ecase bittage
             (8 :uint8) (16 :uint16) (24 :uint24) (32 :uint32) (64 :uint64))))))

(defclass drain (mixed:drain)
  ((device :initform NIL :accessor device)))

(defmethod initialize-instance :after ((drain drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (sdl2:init :audio)
  (cffi:with-foreign-objects ((want '(:struct audio-spec))
                              (have '(:struct audio-spec)))
    (static-vectors:fill-foreign-memory want (cffi:foreign-type-size '(:struct audio-spec)) 0)
    (static-vectors:fill-foreign-memory have (cffi:foreign-type-size '(:struct audio-spec)) 0)
    (let ((pack (mixed:pack drain)))
      (setf (audio-spec-samplerate want) (mixed:samplerate pack))
      (setf (audio-spec-format want) (encode-audio-format (mixed:encoding pack)))
      (setf (audio-spec-channels want) (mixed:channels pack))
      (setf (audio-spec-samples want) (/ (mixed:size pack) (mixed:framesize pack)))
      (setf (audio-spec-callback want) (cffi:callback render))
      (setf (audio-spec-data want) (mixed:handle pack))
      (let ((device (sdl2::check-zero (sdl2-ffi.functions:sdl-open-audio-device (cffi:null-pointer) 0 want have sdl2-ffi:+sdl-audio-allow-any-change+))))
        (setf (device drain) device)
        (setf (mixed:samplerate pack) (audio-spec-samplerate have))
        (setf (mixed:encoding pack) (decode-audio-format (audio-spec-format have)))
        (setf (mixed:channels pack) (audio-spec-channels have))
        (setf (mixed:size pack) (audio-spec-size have))))))

(defmethod mixed:free ((drain drain))
  (when (device drain)
    (sdl2-ffi.functions:sdl-close-audio-device (device drain))
    (setf (device drain) NIl)))

(defmethod mixed:start ((drain drain))
  (sdl2-ffi.functions:sdl-pause-audio-device (device drain) 0))

(cffi:defcallback mix :int ((segment :pointer)) (declare (ignore segment)) 1)

(cffi:defcallback render :void ((pack :pointer) (target :pointer) (available :int))
  (cffi:with-foreign-objects ((source :pointer)
                              (size :uint32))
    (setf (cffi:mem-ref size :uint32) available)
    (when (< (mixed:size pack) available)
      (setf (mixed:size pack) available))
    (mixed-cffi:pack-request-read source size pack)
    (let ((size (cffi:mem-ref size :uint32)))
      (static-vectors:replace-foreign-memory target source size)
      (mixed-cffi:pack-finish-read size source))))

(defmethod mixed:end ((drain drain))
  (sdl2-ffi.functions:sdl-pause-audio-device (device drain) 1))
