#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.pulse
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:pulse #:org.shirakumo.fraf.mixed.pulse.cffi))
  (:export
   #:pulse-error
   #:code
   #:pulse-drain))
(in-package #:org.shirakumo.fraf.mixed.pulse)

(define-condition pulse-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "Pulse error ~d: ~a"
                                 (code c) (pulse:strerror (code c))))))

(defmacro with-error ((errorvar) &body body)
  `(cffi:with-foreign-object (,errorvar :int)
     (when (< (progn ,@body) 0)
       (error 'pulse-error :code (cffi:mem-ref ,errorvar :int)))))

(defclass pulse-drain (mixed:drain)
  ((simple :initform NIL :accessor simple)
   (server :initform NIL :initarg :server :accessor server)))

(defmethod initialize-instance :after ((drain pulse-drain) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (cffi:use-foreign-library pulse:libpulse)
  (cffi:use-foreign-library pulse:libpulse-simple))

(defmethod mixed:start ((drain pulse-drain))
  (unless (simple drain)
    (cffi:with-foreign-object (sample-spec '(:struct pulse:sample-spec))
      (setf (pulse:sample-spec-format sample-spec) :float)
      (setf (pulse:sample-spec-rate sample-spec) (mixed:target-samplerate drain))
      (setf (pulse:sample-spec-channels sample-spec) (mixed:channels (mixed:pack drain)))
      (with-error (error)
        (setf (simple drain) (pulse:simple-new
                              (or (server drain) (cffi:null-pointer)) (mixed:program-name drain)
                              :playback (cffi:null-pointer) (program-name drain)
                              sample-spec (cffi:null-pointer) (cffi:null-pointer)
                              error))
        (when (cffi:null-pointer-p (simple drain)) -1 1))
      (setf (mixed:samplerate (mixed:pack drain)) (pulse:sample-spec-rate sample-spec))
      (setf (mixed:encoding (mixed:pack drain)) (pulse:sample-spec-format sample-spec))
      (setf (mixed:channels (mixed:pack drain)) (pulse:sample-spec-channels sample-spec)))))

(cffi:defcallback mix :int ((segment :pointer))
  (let ((drain (mixed:pointer->object segment)))
    (mixed:with-buffer-tx (data start end (mixed:pack drain))
      (with-error (err)
        (mixed:finish (pulse:simple-write (simple drain) (mixed:data-ptr) (- end start) err))))))

(defmethod mixed:end ((drain pulse-drain))
  (when (simple drain)
    (with-error (err)
      (pulse:simple-drain (simple drain) err))
    (pulse:simple-free (simple drain))
    (setf (simple drain) NIL)))
