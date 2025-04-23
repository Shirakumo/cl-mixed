(defpackage #:org.shirakumo.fraf.mixed.nxau
  (:use #:cl)
  (:shadow #:stream)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:nxau #:org.shirakumo.fraf.mixed.nxau.cffi))
  (:export
   #:nxau-error
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.nxau)

(define-condition nxau-error (error)
  ()
  (:report "NXAU operation failed."))

(declaim (inline check-result))
(defun check-result (result)
  (unless result
    (error 'nxau-error)))

(defclass drain (mixed:drain)
  ((device :initform NIL :accessor device)))

(defmethod initialize-instance :after ((drain drain) &key)
  (unless (cffi:foreign-library-loaded-p 'nxau:libnxau)
    (cffi:use-foreign-library nxau:libnxau))
  (let ((pack (mixed:pack drain)))
    (cffi:with-foreign-object (format '(:struct nxau:audio-format))
      (setf (nxau:audio-format-samplerate format) (mixed:samplerate pack))
      (setf (nxau:audio-format-channels format) (mixed:channels pack))
      (setf (nxau:audio-format-format format) (mixed:encoding pack))
      (setf (nxau:audio-format-buffersize format) (mixed:size pack))
      (let ((device (nxau:open format)))
        (when (cffi:null-pointer-p device)
          (error 'nxau-error))
        (setf (device drain) device)
        (setf (mixed:samplerate pack) (nxau:audio-format-samplerate format))
        (setf (mixed:channels pack) (nxau:audio-format-channels format))
        (setf (mixed:encoding pack) (nxau:audio-format-format format))
        (setf (mixed:size pack) (* 2 (nxau:audio-format-buffersize format)))))))

(defmethod mixed:free ((drain drain))
  (when (device drain)
    (nxau:close (device drain))
    (setf (device drain) NIL)))

(defmethod mixed:start ((drain drain))
  (check-result (nxau:start (device drain))))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (< 0 size)
      (mixed:finish (nxau:play (mixed:data-ptr) size 0.01 (device drain))))))

(defmethod mixed:end ((drain drain))
  (check-result (nxau:stop (device drain))))
