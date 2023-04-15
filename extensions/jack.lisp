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
   #:jack-present-p
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.jack)

(define-condition jack-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "Jack error ~d"
                                 (code c)))))

(defun load-lib ()
  (unless (cffi:foreign-library-loaded-p 'jack:libjack)
    (cffi:load-foreign-library 'jack:libjack)
    #+sbcl (ignore-errors (cffi:foreign-funcall "restore_sbcl_signals" :void))))

(defun jack-present-p ()
  (handler-case (load-lib)
    (error () (return-from jack-present-p NIL)))
  (cffi:with-foreign-object (status 'jack:status)
    (let* ((client (jack:open-client "mixed-server-probe" '(:no-start-server) status :int 0))
           (status (cffi:mem-ref status 'jack:status)))
      (cond ((eql :ok status)
             (jack:close-client client)
             T)
            (T
             NIL)))))

(cffi:defcstruct (data :conc-name data-)
  (handle :pointer)
  (channels :uint8)
  (ports :pointer :count 32))

(defclass drain (mixed:virtual)
  ((client :initform NIL :accessor client)
   (program-name :initarg :program-name :initform "mixed" :accessor program-name)
   (server :initarg :server :initform "default" :accessor server)
   (samplerate :initform 44100 :accessor mixed:samplerate)))

(defmethod initialize-instance :after ((drain drain) &key (channels 2))
  (assert (<= channels 16) (channels))
  (load-lib)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (let* ((size (cffi:foreign-type-size '(:struct data)))
         (data (cffi:foreign-alloc :uint8 :count size)))
    (static-vectors:fill-foreign-memory data size 0)
    (setf (data-handle data) (mixed:handle drain))
    (setf (data-channels data) channels)
    (setf (mixed-cffi:direct-segment-data (mixed:handle drain)) data))
  (cffi:with-foreign-object (status 'jack:status)
    (let ((client (jack:open-client (program-name drain) '(:server-name) status :string (server drain)))
          (data (mixed-cffi:direct-segment-data (mixed:handle drain))))
      #+sbcl
      (cffi:foreign-funcall "restore_sbcl_signals" :void)
      (when (cffi:null-pointer-p client)
        (error 'jack-error :code (cffi:mem-ref status 'jack:status)))
      (setf (mixed:samplerate drain) (jack:get-sample-rate client))
      (jack:set-process-callback client (cffi:callback process) data)
      (jack:set-sample-rate-callback client (cffi:callback samplerate) data)
      (jack:set-shutdown-callback client (cffi:callback shutdown) data)
      (loop with ports = (cffi:foreign-slot-pointer data '(:struct data) 'ports)
            for i from 0 below channels
            for port = (jack:register-port client (princ-to-string i) jack:DEFAULT-AUDIO-TYPE '(:output) 0)
            do (when (cffi:null-pointer-p port)
                 (error 'jack-error :code :port-registration-failed))
               (setf (cffi:mem-aref ports :pointer (* 2 i)) port))
      (setf (client drain) client))))

(defmethod mixed:free ((drain drain))
  (when (client drain)
    (jack:close-client (client drain))
    (setf (client drain) NIL)))

(defmethod (setf mixed:input-field) :after (value (field (eql :buffer)) (location integer) (drain drain))
  (let* ((data (mixed-cffi:direct-segment-data (mixed:handle drain)))
         (ports (cffi:foreign-slot-pointer data '(:struct data) 'ports)))
    (setf (cffi:mem-aref ports :pointer (1+ (* 2 location)))
          (etypecase value
            (mixed:buffer (mixed:handle value))
            (null (cffi:null-pointer))))))

(defmethod mixed:channels ((drain drain))
  (data-channels (mixed-cffi:direct-segment-data (mixed:handle drain))))

(defmethod mixed:info ((drain drain))
  (list :name "jack-drain"
        :description "Jack output drain."
        :flags ()
        :min-inputs (mixed:channels drain)
        :max-inputs (mixed:channels drain)
        :outputs 0
        :fields ()))

(defmethod mixed:start ((drain drain))
  (if (client drain)
      (jack:activate-client (client drain))
      (error "Client not allocated.")))

(cffi:defcallback mix :int ((segment :pointer))
  (if (client (mixed:pointer->object segment)) 1 0))

(cffi:defcallback process :int ((frames :uint32) (data :pointer))
  (cffi:with-foreign-objects ((source :pointer)
                              (samples :uint32))
    (setf (cffi:mem-ref samples :uint32) frames)
    (loop with ports = (cffi:foreign-slot-pointer data '(:struct data) 'ports)
          for i from 0 below (data-channels data)
          for port = (cffi:mem-aref ports :pointer (* 2 i))
          for buffer = (cffi:mem-aref ports :pointer (1+ (* 2 i)))
          do (mixed-cffi:buffer-request-read source samples buffer)
             (let* ((samples (cffi:mem-ref samples :uint32))
                    (target (jack:port-buffer port samples)))
               (static-vectors:replace-foreign-memory target (cffi:mem-ref source :pointer) samples)
               (mixed-cffi:buffer-finish-read samples buffer)))
    1))

(cffi:defcallback samplerate :int ((samplerate :uint32) (data :pointer))
  (let ((drain (mixed:pointer->object (data-handle data))))
    (setf (mixed:samplerate drain) samplerate))
  1)

(cffi:defcallback shutdown :int ((data :pointer))
  (let ((drain (mixed:pointer->object (data-handle data))))
    (setf (client drain) NIL))
  1)

(defmethod mixed:end ((drain drain))
  (jack:deactivate-client (client drain)))
