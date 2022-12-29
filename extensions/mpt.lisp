#|
This file is a part of cl-mixed
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.mpt
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:mpt #:org.shirakumo.fraf.mixed.mpt.cffi))
  (:export
   #:source))
(in-package #:org.shirakumo.fraf.mixed.mpt)

(defclass source (mixed:source)
  ((module :initform NIL :accessor module)
   (channels :initarg :channels :initform 2 :accessor channels)
   (samplerate :initarg :samplerate :initform mixed:*default-samplerate* :accessor samplerate)))

(defmethod initialize-instance :after ((source source) &key file)
  (cffi:load-foreign-library 'mpt:libopenmpt)
  (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
    (let ((buffer (static-vectors:make-static-vector (file-length stream) :element-type '(unsigned-byte 8))))
      (unwind-protect
           (cffi:with-foreign-object (message :pointer)
             (read-sequence buffer stream)
             (let ((module (mpt:create (static-vectors:static-vector-pointer buffer)
                                       (length buffer)
                                       (cffi:null-pointer)
                                       (cffi:null-pointer)
                                       (cffi:null-pointer)
                                       (cffi:null-pointer)
                                       (cffi:null-pointer)
                                       message
                                       (cffi:null-pointer))))
               (when (cffi:null-pointer-p module)
                 (let ((message (cffi:mem-ref message :pointer)))
                   (error "Failed to create MPT module:~@[~%  ~a~]"
                          (unless (cffi:null-pointer-p message)
                            (cffi:foreign-string-to-lisp message)))))
               (setf (module source) module)))
        (static-vectors:free-static-vector buffer))))
  (setf (mixed:samplerate (mixed:pack source)) (samplerate source))
  (setf (mixed:channels (mixed:pack source)) (channels source))
  (setf (mixed:encoding (mixed:pack source)) :float))

(defmethod mixed:free ((source source))
  (when (module source)
    (mpt:destroy (module source))
    (setf (module source) NIL)))

(defmethod mixed:start ((source source)))
(defmethod mixed:end ((source source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (mpt:set-position (module source) (float (truncate position (samplerate source)) 0d0)))

(defmethod mixed:frame-count ((source source))
  (truncate (* (mpt:get-duration (module source)) (samplerate source))))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (macrolet ((handle (reader channels)
                 `(let ((frames (,reader (module source) (samplerate source) (floor size ,(* 4 channels)) (mixed:data-ptr))))
                    (cond ((< 0 frames)
                           (incf (mixed:frame-position source) frames)
                           (mixed:finish (* ,(* 4 channels) frames)))
                          ((= 0 (mixed:available-read (mixed:pack source)))
                           (setf (mixed:done-p source) T))))))
      (ecase (channels source)
        (1 (handle mpt:read-mono 1))
        (2 (handle mpt:read-stereo 2))
        (4 (handle mpt:read-quad 4))))))
