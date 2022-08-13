#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.alsa
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:alsa #:org.shirakumo.fraf.mixed.alsa.cffi))
  (:export
   #:alsa-error
   #:code
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.alsa)

(define-condition alsa-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "ALSA error ~d: ~a"
                                 (code c) (alsa:strerror (code c))))))

(defun check-result (result)
  (if (< result 0)
      (error 'alsa-error :code result)
      result))

(defclass drain (mixed:device-drain)
  ((pcm :initform NIL :accessor pcm)))

(defmethod initialize-instance :after ((drain drain) &key device)
  (cffi:use-foreign-library alsa:libasound)
  (connect drain device))

(defmethod connect ((drain drain) device)
  (cffi:with-foreign-objects ((pcm :pointer)
                              (params :uint8 (alsa:pcm-hw-params-size))
                              (format 'alsa:pcm-format)
                              (channels :uint)
                              (rate :uint)
                              (dir :int))
    (restart-case
        (let ((error (alsa:pcm-open pcm (if (or (null device) (eql :default device)) "default" device) :playback 0)))
          (case error
            (0)
            (-6 (error 'mixed:device-not-found :device device))
            (T (error 'alsa-error :code error))))
      (continue (&optional c)
        :report "Continue by using the default device."
        (declare (ignore c))
        (alsa:pcm-open pcm "default" :playback 0)))
    (let ((pcm (cffi:mem-ref pcm :pointer))
          (pack (mixed:pack drain)))
      (check-result
       (alsa:pcm-set-params pcm :float :rw-interleaved
                            (mixed:channels pack)
                            (mixed:samplerate pack)
                            1 1000))
      ;; Extract actual parameters now.
      (check-result
       (alsa:pcm-hw-params-current pcm params))
      (check-result
       (alsa:pcm-hw-params-get-format params format))
      (check-result
       (alsa:pcm-hw-params-get-channels params channels))
      (check-result
       (alsa:pcm-hw-params-get-rate params rate dir))
      (setf (mixed:encoding pack) (cffi:mem-ref format 'alsa:pcm-format))
      (setf (mixed:channels pack) (cffi:mem-ref channels :uint))
      (setf (mixed:samplerate pack) (cffi:mem-ref rate :uint))
      (setf (pcm drain) pcm))))

(defmethod mixed:device ((drain drain))
  (alsa:pcm-name (pcm drain)))

(defmethod (setf mixed:device) (device (drain drain))
  (cond ((pcm drain)
         (alsa:pcm-drain (pcm drain))
         (alsa:pcm-close (pcm drain))
         (setf (pcm drain) NIL)
         (connect drain device))
        (T
         (connect drain device))))

(defmethod mixed:list-devices ((drain drain))
  (cffi:with-foreign-object (hints :pointer)
    (check-result (alsa:device-hint -1 "pcm" hints))
    (let ((hints (cffi:mem-ref hints :pointer))
          (names ()))
      (unwind-protect
           (loop for i from 0
                 for hint = (cffi:mem-aref hints :pointer i)
                 until (cffi:null-pointer-p hint)
                 do (let ((name (alsa:device-name hint "NAME")))
                      (unwind-protect
                           (unless (cffi:null-pointer-p name)
                             (push (cffi:foreign-string-to-lisp name) names))
                        (cffi:foreign-free name))))
        (alsa:device-free-hint hints))
      names)))

(defmethod mixed:free ((drain drain))
  (when (pcm drain)
    (alsa:pcm-close (pcm drain))
    (setf (pcm drain) NIL)))

(defmethod mixed:start ((drain drain)))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (let* ((framesize (mixed:framesize (mixed:pack drain)))
           (played (alsa:pcm-writei (pcm drain) (mixed:data-ptr) (/ size framesize))))
      (if (< played 0)
          (check-result
           (alsa:pcm-recover (pcm drain) played 0))
          (mixed:finish (max 0 (* played framesize)))))))

(defmethod mixed:end ((drain drain))
  (alsa:pcm-drain (pcm drain)))
