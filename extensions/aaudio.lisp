#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.aaudio
  (:use #:cl)
  (:shadow #:stream)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:aaudio #:org.shirakumo.fraf.mixed.aaudio.cffi))
  (:export
   #:aaudio-error
   #:code
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.aaudio)

(define-condition aaudio-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "AAudio error ~d: ~a"
                                 (code c) (aaudio:result-string (code c))))))

(declaim (inline check-result))
(defun check-result (result)
  (etypecase result
    ((or (eql :ok) unsigned-byte)
     result)
    (T
     (error 'aaudio-error :code result))))

(defclass drain (mixed:device-drain)
  ((stream :initform NIL :accessor stream)
   (mode :initarg :mode :initform :shared :accessor mode)
   (usage :initarg :usage :initform :media :accessor usage)
   (content-type :initarg :content-type :initform :music :accessor content-type)
   (performance-mode :initarg :performance-mode :initform :none :accessor performance-mode)
   (spatialization :initarg :spatialization :initform NIL :accessor spatialization)
   (capture-policy :initarg :capture-policy :initform :allow-by-all :accessor capture-policy)
   (session-id :initarg :session-id :initform :none :accessor session-id)
   (privacy-sensitive :initarg :privacy-sensitive :initform NIL :accessor privacy-sensitive)))

(defmethod initialize-instance :after ((drain drain) &key device)
  (unless (cffi:foreign-library-loaded-p 'aaudio:libaaudio)
    (cffi:use-foreign-library aaudio:libaaudio))
  (connect drain device))

(defmacro with-deref ((var type) call)
  `(cffi:with-foreign-object (,var ,type)
     (check-result ,call)
     (cffi:mem-ref ,var ,type)))

(defmacro with-unwind-protect (cleanup &body body)
  `(unwind-protect (progn ,@body) ,cleanup))

(defmethod connect ((drain drain) device)
  (let ((builder (with-deref (builder :pointer) (aaudio:create-stream-builder builder)))
        (pack (mixed:pack drain)))
    (with-unwind-protect (aaudio:stream-builder-delete builder)
      (when device
        (aaudio:stream-builder-set-device-id builder device))
      (aaudio:stream-builder-set-direction builder :output)
      (aaudio:stream-builder-set-format builder (mixed:encoding pack))
      (aaudio:stream-builder-set-channel-count builder (mixed:channels pack))
      (aaudio:stream-builder-set-sample-rate builder (mixed:samplerate pack))
      (aaudio:stream-builder-set-sharing-mode builder (mode drain))
      (ignore-errors (aaudio:stream-builder-set-usage builder (usage drain)))
      (ignore-errors (aaudio:stream-builder-set-content-type builder (content-type drain)))
      (ignore-errors (aaudio:stream-builder-set-performance-mode builder (performance-mode drain)))
      (ignore-errors (case (spatialization drain)
                       (:internal (aaudio:stream-builder-set-content-spatialized builder T))
                       (T (aaudio:stream-builder-set-spatialization-behavior builder (spatialization drain)))))
      (ignore-errors (aaudio:stream-builder-set-capture-policy builder (capture-policy drain)))
      (ignore-errors (aaudio:stream-builder-set-session-id builder (session-id drain)))
      (ignore-errors (aaudio:stream-builder-set-privacy-sensitive builder (privacy-sensitive drain)))
      (ignore-errors (aaudio:stream-builder-set-attribution-tag builder (mixed:program-name drain)))
      (let ((stream (with-deref (stream :pointer) (aaudio:stream-builder-open-stream builder stream)))
            (ok NIL))
        (when (and device (/= device (aaudio:stream-get-device-id stream)))
          (with-unwind-protect (unless ok (aaudio:stream-close stream)) 
            (with-simple-restart (continue "Continue by using the device anyway.")
              (error 'mixed:device-not-found :device device))
            (setf ok T)))
        (setf (mixed:encoding pack) (aaudio:stream-get-format stream))
        (setf (mixed:channels pack) (aaudio:stream-get-channel-count stream))
        (setf (mixed:samplerate pack) (aaudio:stream-get-sample-rate stream))
        (setf (stream drain) stream)))))

(defmethod mixed:device ((drain drain))
  (aaudio:stream-get-device-id (stream drain)))

(defmethod (setf mixed:device) (device (drain drain))
  (cond ((stream drain)
         (aaudio:stream-close (stream drain))
         (setf (stream drain) NIL)
         (connect drain device))
        (T
         (connect drain device))))

(defmethod mixed:list-devices ((drain drain))
  ())

(defmethod mixed:free ((drain drain))
  (when (stream drain)
    (aaudio:stream-close (stream drain))
    (setf (stream drain) NIL)))

(defmethod mixed:start ((drain drain))
  (check-result (aaudio:stream-request-start (stream drain))))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (let* ((framesize (mixed:framesize (mixed:pack drain)))
           (played (check-result (aaudio:stream-write (stream drain) (mixed:data-ptr) (truncate size framesize) 1000000000))))
      (if (eq played :ok)
          (mixed:finish 0)
          (mixed:finish (max 0 (* played framesize)))))))

(defmethod mixed:end ((drain drain))
  (check-result (aaudio:stream-request-stop (stream drain))))
