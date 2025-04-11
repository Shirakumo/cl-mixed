(defpackage #:org.shirakumo.fraf.mixed.xmp
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:xmp #:org.shirakumo.fraf.mixed.xmp.cffi))
  (:export
   #:xmp-error
   #:source))
(in-package #:org.shirakumo.fraf.mixed.xmp)

(define-condition xmp-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "LibXMP error ~a"
                                 (code c)))))

(defun check-call (result)
  (unless (eq :ok result)
    (error 'xmp-error :code result)))

(defclass source (mixed:source)
  ((context :initform NIL :accessor context)
   (channels :initarg :channels :initform 2 :accessor channels)
   (frame-count :initform NIL :reader mixed:frame-count :accessor frame-count)
   (samplerate :initarg :samplerate :initform mixed:*default-samplerate* :accessor samplerate)))

(defmethod initialize-instance :after ((source source) &key file)
  (unless (cffi:foreign-library-loaded-p 'xmp:libxmp)
    (cffi:load-foreign-library 'xmp:libxmp))
  (let ((context (xmp:create-context)))
    (when (cffi:null-pointer-p context)
      (error "Failed to create context."))
    (check-call (xmp:load-module context (etypecase file
                                           (string file)
                                           (pathname (pathname-utils:native-namestring file)))))
    (check-call (xmp:start-player context (samplerate source) (ecase (channels source)
                                                                (1 '(:mono))
                                                                (2 '()))))
    (setf (context source) context)
    (cffi:with-foreign-objects ((mi '(:struct xmp:module-info)))
      (xmp:get-module-info context mi)
      ;; Estimating frame count based on sum of sequence durations in milliseconds
      (setf (frame-count source) (* (loop for i from 0 below (xmp:module-info-sequence-count mi)
                                          sum (xmp:sequence-duration (cffi:mem-aptr (xmp:module-info-sequences mi) '(:struct xmp:sequence) i)))
                                    1/1000
                                    (samplerate source)))))
  (setf (mixed:samplerate (mixed:pack source)) (samplerate source))
  (setf (mixed:channels (mixed:pack source)) (channels source))
  (setf (mixed:encoding (mixed:pack source)) :int16))

(defmethod mixed:free ((source source))
  (when (context source)
    (xmp:end-player (context source))
    (xmp:release-module (context source))
    (xmp:free-context (context source))
    (setf (context source) NIL)))

(defmethod mixed:start ((source source)))
(defmethod mixed:end ((source source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (xmp:seek-time (context source) (round (* 1000 (/ position (samplerate source))))))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (let* ((framesize (mixed:framesize source))
           (frames (truncate size framesize))
           (result (xmp:play-buffer (context source) (mixed:data-ptr) (* frames framesize) NIL)))
      (case result
        (:ok
         (incf (mixed:frame-position source) frames)
         (mixed:finish size))
        (:end
         (setf (mixed:frame-position source) (mixed:frame-count source))
         (mixed:finish size)
         (setf (mixed:done-p source) T))
        (T
         (error 'xmp-error :code result))))))
