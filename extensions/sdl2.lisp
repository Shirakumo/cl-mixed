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

(defun encode-audio-format (format)
  (let ((f 0))
    (setf (ldb (byte 8 0) f) (* 8 (mixed:samplesize format)))
    (case format
      ((:double :float)
       (setf (ldb (byte 1 8) f) 1)
       (setf (ldb (byte 1 15) f) 1))
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

(defclass drain (mixed:device-drain)
  ((dev :initform NIL :accessor dev)
   (device :initform NIL :initarg :device :accessor device)
   (lock :initform (bt:make-lock "SDL2 sync") :reader lock)
   (cvar :initform (bt:make-condition-variable :name "SDL2 sync") :reader cvar)))

(defmethod initialize-instance :after ((drain drain) &key)
  (sdl2:init* '(:audio))
  (init drain (device drain)))

(defun init (drain device)
  (plus-c:c-let ((want sdl2-ffi:sdl-audio-spec :calloc T)
                 (have sdl2-ffi:sdl-audio-spec :calloc T))
    (let ((pack (mixed:pack drain)))
      (setf (plus-c:c-ref want sdl2-ffi:sdl-audio-spec :freq) (mixed:samplerate pack))
      (setf (plus-c:c-ref want sdl2-ffi:sdl-audio-spec :format) (encode-audio-format (mixed:encoding pack)))
      (setf (plus-c:c-ref want sdl2-ffi:sdl-audio-spec :channels) (mixed:channels pack))
      (setf (plus-c:c-ref want sdl2-ffi:sdl-audio-spec :samples) (/ (mixed:size pack) (mixed:framesize pack)))
      (setf (plus-c:c-ref want sdl2-ffi:sdl-audio-spec :callback) (cffi:callback render))
      (setf (plus-c:c-ref want sdl2-ffi:sdl-audio-spec :userdata) (mixed:handle drain))
      (let ((dev (sdl2::check-zero (sdl2-ffi.functions:sdl-open-audio-device (if (or (null device) (eql :default device)) (cffi:null-pointer) device)
                                                                             0 want have sdl2-ffi:+sdl-audio-allow-any-change+))))
        (setf (device drain) device)
        (setf (dev drain) dev)
        (setf (mixed:samplerate pack) (plus-c:c-ref have sdl2-ffi:sdl-audio-spec :freq))
        (setf (mixed:encoding pack) (decode-audio-format (plus-c:c-ref have sdl2-ffi:sdl-audio-spec :format)))
        (setf (mixed:channels pack) (plus-c:c-ref have sdl2-ffi:sdl-audio-spec :channels))
        (setf (mixed:size pack) (plus-c:c-ref have sdl2-ffi:sdl-audio-spec :size))))))

(defmethod mixed:list-devices ((drain drain))
  (loop for i from 0 below (sdl2-ffi.functions:sdl-get-num-audio-devices 0)
        collect (sdl2-ffi.functions:sdl-get-audio-device-name i 0)))

(defmethod mixed:device ((drain drain))
  (device drain))

(defmethod (setf mixed:device) (device (drain drain))
  (mixed:free drain)
  (init drain device)
  device)

(defmethod mixed:free ((drain drain))
  (when (dev drain)
    (sdl2-ffi.functions:sdl-close-audio-device (dev drain))
    (setf (dev drain) NIL)))

(defmethod mixed:start ((drain drain))
  (sdl2-ffi.functions:sdl-pause-audio-device (dev drain) 0))

(defmethod mixed:mix ((drain drain))
  (let ((pack (mixed:pack drain)))
    (bt:with-lock-held ((lock drain))
      (bt:condition-wait (cvar drain) (lock drain) :timeout 0.01)
      (when (= 0 (mixed:available-read pack))
        (mixed:clear pack)))))

(cffi:defcallback render :void ((handle :pointer) (ptr :pointer) (total :int))
  (let* ((drain (mixed:pointer->object handle))
         (pack (mixed:pack drain))
         (remaining total))
    (loop (mixed:with-buffer-tx (data start size pack :size remaining)
            (when (< 0 size)
              (static-vectors:replace-foreign-memory ptr (mixed:data-ptr) size)
              (mixed:finish size)
              (cffi:incf-pointer ptr size)
              (decf remaining size)))
          (bt:condition-notify (cvar drain))
          (if (= remaining total)
              (bt:with-lock-held ((lock drain))
                (bt:condition-wait (cvar drain) (lock drain) :timeout 0.1))
              (return)))))

(defmethod mixed:end ((drain drain))
  (sdl2-ffi.functions:sdl-pause-audio-device (dev drain) 1))
