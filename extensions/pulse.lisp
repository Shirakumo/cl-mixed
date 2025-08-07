(defpackage #:org.shirakumo.fraf.mixed.pulse
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:pulse #:org.shirakumo.fraf.mixed.pulse.cffi))
  (:export
   #:pulse-error
   #:code
   #:present-p
   #:drain
   #:source))
(in-package #:org.shirakumo.fraf.mixed.pulse)

(define-condition pulse-error (error)
  ((code :initarg :code :accessor code))
  (:report (lambda (c s) (format s "Pulse error ~d: ~a"
                                 (code c) (pulse:strerror (code c))))))

(defmacro with-error ((errorvar) &body body)
  `(cffi:with-foreign-object (,errorvar :int)
     (when (< (progn ,@body) 0)
       (error 'pulse-error :code (cffi:mem-ref ,errorvar :int)))))

(defun present-p ()
  (unless (cffi:foreign-library-loaded-p 'pulse:libpulse-simple)
    (handler-case (progn (cffi:use-foreign-library pulse:libpulse)
                         (cffi:use-foreign-library pulse:libpulse-simple))
      (error () (return-from present-p NIL))))
  (cffi:with-foreign-objects ((err :int)
                              (sample-spec '(:struct pulse:sample-spec)))
    (setf (pulse:sample-spec-format sample-spec) :int16)
    (setf (pulse:sample-spec-rate sample-spec) mixed:*default-samplerate*)
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

(defun init-segment (segment direction)
  (unless (cffi:foreign-library-loaded-p 'pulse:libpulse-simple)
    (cffi:use-foreign-library pulse:libpulse)
    (cffi:use-foreign-library pulse:libpulse-simple))
  (let* ((pack (mixed:pack segment))
         (channels (or (mixed:channel-order segment) (mixed:guess-channel-order-from-count (mixed:channels pack))))
         (buffer-size (floor (mixed:size pack) (mixed:framesize pack))))
    (cffi:with-foreign-objects ((sample-spec '(:struct pulse:sample-spec))
                                (channel-map '(:struct pulse:channel-map))
                                (buffer-attributes '(:struct pulse:buffer-attr)))
      (setf (pulse:sample-spec-format sample-spec) :float)
      (setf (pulse:sample-spec-rate sample-spec) (mixed:samplerate pack))
      (setf (pulse:sample-spec-channels sample-spec) (mixed:channels pack))
      (setf channel-map (pulse:channel-map-init channel-map))
      (setf (pulse:channel-map-channels channel-map) (mixed:channels pack))
      (setf (pulse:buffer-attr-length buffer-attributes) buffer-size)
      (setf (pulse:buffer-attr-maxlength buffer-attributes) #xffffffff)
      (setf (pulse:buffer-attr-minreq buffer-attributes) #xffffffff)
      (setf (pulse:buffer-attr-prebuf buffer-attributes) #xffffffff)
      (setf (pulse:buffer-attr-fragsize buffer-attributes) buffer-size)
      (loop with ptr = (cffi:foreign-slot-pointer channel-map '(:struct pulse:channel-map) 'pulse::map)
            for i from 0 below (mixed:channels pack)
            do (setf (cffi:mem-aref ptr 'pulse:channel-position i) (nth i channels)))
      (with-error (error)
        (setf (simple segment) (pulse:simple-new
                                (or (server segment) (cffi:null-pointer)) (mixed:program-name segment)
                                direction (cffi:null-pointer) (mixed:program-name segment)
                                sample-spec channel-map buffer-attributes
                                error))
        (if (cffi:null-pointer-p (simple segment)) -1 1))
      (setf (mixed:samplerate pack) (pulse:sample-spec-rate sample-spec))
      (setf (mixed:encoding pack) (pulse:sample-spec-format sample-spec))
      (setf (mixed:channels pack) (pulse:sample-spec-channels sample-spec))
      (setf (mixed:channel-order segment)
            (loop with ptr = (cffi:foreign-slot-pointer channel-map '(:struct pulse:channel-map) 'pulse::map)
                  for i from 0 below (pulse:channel-map-channels channel-map)
                  collect (cffi:mem-aref ptr 'pulse:channel-position i))))))

(defclass drain (mixed:drain)
  ((simple :initform NIL :accessor simple)
   (server :initform NIL :initarg :server :accessor server)
   (channel-order :initform () :initarg :channel-order :accessor mixed:channel-order)))

(defmethod initialize-instance :after ((drain drain) &key &allow-other-keys)
  (init-segment drain :playback))

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

(defclass source (mixed:source)
  ((mixed:program-name :initform "Mixed" :initarg :program-name :accessor mixed:program-name)
   (simple :initform NIL :accessor simple)
   (server :initform NIL :initarg :server :accessor server)
   (channel-order :initform () :initarg :channel-order :accessor mixed:channel-order)))

(defmethod initialize-instance :after ((source source) &key)
  (init-segment source :record))

(defmethod mixed:free ((source source))
  (when (simple source)
    (pulse:simple-free (simple source))
    (setf (simple source) NIL)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (with-error (err)
        (with-no-interrupts ()
          (pulse:simple-read (simple source) (mixed:data-ptr) size err)))
      (mixed:finish size))))

(defmethod mixed:end ((source source)))
