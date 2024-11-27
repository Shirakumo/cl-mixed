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

(defclass alsa-device ()
  ((pcm :initform NIL :accessor pcm)))

(defmethod initialize-instance :after ((alsa-device alsa-device) &key device)
  (unless (cffi:foreign-library-loaded-p 'alsa:libasound)
    (cffi:load-foreign-library 'alsa:libasound))
  (connect alsa-device device))

(defmethod mixed:device ((alsa-device alsa-device))
  (alsa:pcm-name (pcm alsa-device)))

(defmethod (setf mixed:device) (device (alsa-device alsa-device))
  (cond ((pcm alsa-device)
         (alsa:pcm-drain (pcm alsa-device))
         (alsa:pcm-close (pcm alsa-device))
         (setf (pcm alsa-device) NIL)
         (connect alsa-device device))
        (T
         (connect alsa-device device))))

(defmethod mixed:free ((alsa-device alsa-device))
  (when (pcm alsa-device)
    (alsa:pcm-close (pcm alsa-device))
    (setf (pcm alsa-device) NIL)))

(defmethod mixed:start ((alsa-device alsa-device)))
(defmethod mixed:end ((alsa-device alsa-device)))

(defmethod connect ((alsa-device alsa-device) device)
  (cffi:with-foreign-objects ((pcm :pointer)
                              (params :uint8 (alsa:pcm-hw-params-size))
                              (format 'alsa:pcm-format)
                              (channels :uint)
                              (rate :uint)
                              (dir :int))
    (restart-case
        (let ((error (alsa:pcm-open pcm (if (or (null device) (eql :default device)) "default" device) (stream-type alsa-device) 0)))
          (case error
            (0)
            (-6 (error 'mixed:device-not-found :device device))
            (T (error 'alsa-error :code error))))
      (continue (&optional c)
        :report "Continue by using the default device."
        (declare (ignore c))
        (alsa:pcm-open pcm "default" :playback 0)))
    (let ((pcm (cffi:mem-ref pcm :pointer))
          (pack (mixed:pack alsa-device)))
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
      (setf (pcm alsa-device) pcm))))

(defmethod mixed:list-devices ((alsa-device alsa-device))
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

(defclass source (mixed:device-source alsa-device)
  ())

(defmethod stream-type ((_ source)) :capture)

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (let* ((framesize (mixed:framesize (mixed:pack source)))
           (played (alsa:pcm-readi (pcm source) (mixed:data-ptr) (/ size framesize))))
      (if (< played 0)
          (check-result
           (alsa:pcm-recover (pcm source) played 0))
          (mixed:finish (* played framesize))))))

(defmethod mixed:end ((source source))
  (alsa:pcm-drop (pcm source)))

(defclass drain (mixed:device-drain alsa-device)
  ())

(defmethod stream-type ((_ drain)) :playback)

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (let* ((framesize (mixed:framesize (mixed:pack drain)))
           (played (alsa:pcm-writei (pcm drain) (mixed:data-ptr) (/ size framesize))))
      (if (< played 0)
          (check-result
           (alsa:pcm-recover (pcm drain) played 0))
          (mixed:finish (* played framesize))))))

(defmethod mixed:end ((drain drain))
  (alsa:pcm-drain (pcm drain)))
