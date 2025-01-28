(defpackage #:org.shirakumo.fraf.mixed.pipewire
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:pipewire #:org.shirakumo.fraf.mixed.pipewire.cffi))
  (:export
   #:pipewire-error
   #:code
   #:pipewire-present-p
   #:drain
   #:source))
(in-package #:org.shirakumo.fraf.mixed.pipewire)

(define-condition pipewire-error (error)
  ((message :initarg :message :accessor message))
  (:report (lambda (c s) (format s "PipeWire error~@[: ~a~]"
                                 (message c)))))

(defmacro with-error ((errorvar) &body body)
  `(cffi:with-foreign-object (,errorvar :int)
     (when (< (progn ,@body) 0)
       (error 'pipewire-error :code (cffi:mem-ref ,errorvar :int)))))

(defun pipewire-present-p ()
  (unless (cffi:foreign-library-loaded-p 'pipewire:libpipewire)
    (handler-case (cffi:use-foreign-library pipewire:libpipewire)
      (error () (return-from pipewire-present-p NIL))))
  )

(defun init-segment (segment direction)
  (unless (cffi:foreign-library-loaded-p 'pipewire:libpipewire)
    (cffi:use-foreign-library pipewire:libpipewire))
  (let* ((pack (mixed:pack segment))
         (channels (or (mixed:channel-order segment) (mixed:guess-channel-order-from-count (mixed:channels pack))))
         (buffer-size (floor (mixed:size pack) (mixed:framesize pack))))
    ))

(defclass drain (mixed:drain)
  ((simple :initform NIL :accessor simple)
   (server :initform NIL :initarg :server :accessor server)
   (channel-order :initform () :initarg :channel-order :accessor mixed:channel-order)))

(defmethod initialize-instance :after ((drain drain) &key &allow-other-keys)
  (init-segment drain :playback))

(defmethod mixed:free ((drain drain))
  (when (simple drain)
    
    (setf (simple drain) NIL)))

(defmethod mixed:start ((drain drain)))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (< 0 size)
      
      (mixed:finish size))))

(defmethod mixed:end ((drain drain))
  )

(defclass source (mixed:source)
  ((mixed:program-name :initform "Mixed" :initarg :program-name :accessor mixed:program-name)
   (simple :initform NIL :accessor simple)
   (server :initform NIL :initarg :server :accessor server)
   (channel-order :initform () :initarg :channel-order :accessor mixed:channel-order)))

(defmethod initialize-instance :after ((source source) &key)
  (init-segment source :record))

(defmethod mixed:free ((source source))
  (when (simple source)
    
    (setf (simple source) NIL)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      
      (mixed:finish size))))

(defmethod mixed:end ((source source)))
