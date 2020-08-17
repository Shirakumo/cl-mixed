#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.jack
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:jack #:org.shirakumo.fraf.mixed.jack.cffi))
  (:export
   #:jack-error
   #:code
   #:jack-drain))
(in-package #:org.shirakumo.fraf.mixed.jack)

(define-condition jack-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "Pulse error ~d: ~a"
                                 (code c) (jack:strerror (code c))))))

(defmacro with-error (() &body body)
  (let ((error (gensym "ERROR")))
    `(let ((,error (progn ,@body)))
       (when (< ,error 0)
         (error 'jack-error :code ,error)))))

(cffi:defcstruct (data :conc-name data-)
  (handle :pointer)
  (channels :uint8)
  (running :boolean)
  (ports :pointer))

(defclass jack-drain (mixed:virtual)
  ((client :initform NIL :accessor client)
   (channels :initarg :channels :reader channels)
   (name :initarg :name :accessor name)
   (server :initarg :server :initform "default" :accessor server)))

(defmethod initialize-instance :after ((drain jack-drain) &key channels)
  (cffi:use-foreign-library jack:libjack)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (let ((data (cffi:foreign-alloc :uint8 :count (+ (cffi:foreign-struct-size '(:struct data))
                                                   (* :pointer 2 (channels drain))))))
    (setf (mixed-cffi:direct-segment-data (mixed:handle drain)) data)
    (setf (data-handle data) (mixed:handle drain))
    (setf (data-channels data) channels)))

(defmethod mixed:start ((drain jack-drain))
  (unless (client drain)
    (cffi:with-foreign-object (status 'jack:status)
      (let ((client (jack:open-client (name drain) '(:server-name) status :string (server drain)))
            (data (mixed-cffi:direct-segment-data (mixed:handle drain))))
        (when (cffi:null-pointer-p client)
          (error 'jack-error :code (cffi:mem-ref status 'jack:status)))
        (jack:set-process-callback client (cffi:callback process) data)
        (jack:set-sample-rate-callback client (cffi:callback samplerate) data)
        (jack:set-shutdown-callback client (cffi:callback shutdown) data)
        (loop with ports = (cffi:foreign-slot-pointer data '(:struct data) 'ports)
              for i from 0 below (channels drain)
              for port = (jack:register-port client (princ-to-string i) jack:DEFAULT-AUDIO-TYPE '(:output) 0)
              do (when (cffi:null-pointer-p port)
                   (error 'jack-error :code :port-registration-failed))
                 (setf (cffi:mem-aref ports :pointer (* 2 i)) port))
        (setf (data-running data) T)))))

(cffi:defcallback mix :int ((segment :pointer))
  (if (data-running (mixed-cffi:direct-segment-data segment))
      1 0))

(cffi:defcallback process :int ((frames :uint32) (data :pointer))
  (cffi:with-foreign-objects ((source :pointer)
                              (samples :uint32))
    (setf (cffi:mem-ref samples :uint32) frames)
    (loop with ports = (cffi:foreign-slot-pointer data '(:struct data) 'ports)
          for i from 0 below (data-channels data)
          for buffer = (cffi:mem-aref ports :pointer (1+ (* 2 i)))
          for port = (cffi:mem-aref ports :pointer (* 2 i))
          do (mixed-cffi:buffer-request-read source samples buffer)
             (let* ((samples (cffi:mem-ref samples :uint32))
                    (target (jack:port-buffer port samples)))
               (static-vectors:replace-foreign-memory target (cffi:mem-ref source :pointer) samples)
               (mixed-cffi:buffer-finish-read samples buffer)))
    1))

(cffi:defcallback samplerate :int ((samplerate :uint32) (data :pointer))
  ;; FIXME: might need to resample...
  1)

(cffi:defcallback shutdown :int ((data :pointer))
  (setf (data-running data) NIL)
  1)

(defmethod mixed:end ((drain jack-drain))
  (when (client drain)
    (jack:deactivate-client (client drain))
    (jack:close-client (client drain))
    (setf (client drain) NIL)))
