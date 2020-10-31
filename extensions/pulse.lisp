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
   #:pulse-present-p
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.pulse)

(define-condition pulse-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "Pulse error ~d: ~a"
                                 (code c) (pulse:strerror (code c))))))

(defmacro with-error ((errorvar) &body body)
  `(cffi:with-foreign-object (,errorvar :int)
     (when (< (progn ,@body) 0)
       (error 'pulse-error :code (cffi:mem-ref ,errorvar :int)))))

(defun pulse-present-p ()
  (handler-case (progn (cffi:use-foreign-library pulse:libpulse)
                       (cffi:use-foreign-library pulse:libpulse-simple))
    (error () (return-from pulse-present-p NIL)))
  (cffi:with-foreign-objects ((err :int)
                              (sample-spec '(:struct pulse:sample-spec)))
    (setf (pulse:sample-spec-format sample-spec) :int16)
    (setf (pulse:sample-spec-rate sample-spec) 44100)
    (setf (pulse:sample-spec-channels sample-spec) 2)
    (let ((drain (pulse:simple-new
                  (cffi:null-pointer) "mixed"
                  :playback (cffi:null-pointer) "mixed"
                  sample-spec (cffi:null-pointer) (cffi:null-pointer)
                  err)))
      (cond ((cffi:null-pointer-p drain)
             NIL)
            (T
             (pulse:simple-free drain)
             T)))))

(defmacro with-no-interrupts (() &body body)
  ;; On SBCL using WITHOUT-INTERRUPTS would cause interrupts
  ;; to be processed explicitly on exit. We want to avoid that.
  #+sbcl `(let ((sb-sys:*interrupts-enabled* NIL)
                (sb-kernel:*gc-inhibit* T))
            ,@body)
  #+ccl `(ccl:without-interrupts
           ,@body)
  #-(or sbcl ccl) `(progn ,@body))

(defclass drain (mixed:drain)
  ((simple :initform NIL :accessor simple)
   (server :initform NIL :initarg :server :accessor server)
   (channel-order :initform () :initarg :channel-order :accessor mixed:channel-order)))

(defmethod initialize-instance :after ((drain drain) &key)
  (cffi:use-foreign-library pulse:libpulse)
  (cffi:use-foreign-library pulse:libpulse-simple)
  (let* ((pack (mixed:pack drain))
         (channels (or (mixed:channel-order drain) (mixed:guess-channel-order-from-count (mixed:channels pack)))))
    (cffi:with-foreign-objects ((sample-spec '(:struct pulse:sample-spec))
                                (channel-map '(:struct pulse:channel-map)))
      (setf (pulse:sample-spec-format sample-spec) :float)
      (setf (pulse:sample-spec-rate sample-spec) (mixed:samplerate pack))
      (setf (pulse:sample-spec-channels sample-spec) (mixed:channels pack))
      (setf channel-map (pulse:channel-map-init channel-map))
      (setf (pulse:channel-map-channels channel-map) (mixed:channels pack))
      (loop with ptr = (cffi:foreign-slot-pointer channel-map '(:struct pulse:channel-map) 'pulse::map)
            for i from 0 below (mixed:channels pack)
            do (setf (cffi:mem-aref ptr 'pulse:channel-position i) (nth i channels)))
      (with-error (error)
        (setf (simple drain) (pulse:simple-new
                              (or (server drain) (cffi:null-pointer)) (mixed:program-name drain)
                              :playback (cffi:null-pointer) (mixed:program-name drain)
                              sample-spec channel-map (cffi:null-pointer)
                              error))
        (if (cffi:null-pointer-p (simple drain)) -1 1))
      (setf (mixed:samplerate pack) (pulse:sample-spec-rate sample-spec))
      (setf (mixed:encoding pack) (pulse:sample-spec-format sample-spec))
      (setf (mixed:channels pack) (pulse:sample-spec-channels sample-spec))
      (setf (mixed:channel-order drain)
            (loop with ptr = (cffi:foreign-slot-pointer channel-map '(:struct pulse:channel-map) 'pulse::map)
                  for i from 0 below (pulse:channel-map-channels channel-map)
                  collect (cffi:mem-aref ptr 'pulse:channel-position i))))))

(defmethod mixed:free ((drain drain))
  (when (simple drain)
    (pulse:simple-free (simple drain))
    (setf (simple drain) NIL)))

(defmethod mixed:start ((drain drain)))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (< 0 size)
      (with-error (err)
        (with-no-interrupts ()
          (pulse:simple-write (simple drain) (mixed:data-ptr) size err)))
      (mixed:finish size))))

(defmethod mixed:end ((drain drain))
  (with-error (err)
    (pulse:simple-drain (simple drain) err)))
