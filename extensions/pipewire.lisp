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

(defmacro check-null (call &optional format &rest args)
  `(let ((value ,call))
     (if (cffi:null-pointer-p value)
         (error 'pipewire-error :message ,(if format
                                              `(format NIL ,format ,@args)
                                              (format NIL "Call to ~s failed!" (first call))))
         value)))

(defmacro check-call (call &optional format &rest args)
  `(let ((value ,call))
     (if (< value 0)
         (error 'pipewire-error :message ,(if format
                                              `(format NIL ,format ,@args)
                                              (format NIL "Call to ~s failed!" (first call))))
         value)))

(defun pipewire-present-p ()
  (unless (cffi:foreign-library-loaded-p 'pipewire:libpipewire)
    (handler-case (progn (cffi:load-foreign-library 'pipewire:libpipewire)
                         (cffi:load-foreign-library 'pipewire:pipewire-spa))
      (error () (return-from pipewire-present-p (values NIL :no-such-library)))))
  (pipewire:init (cffi:null-pointer) (cffi:null-pointer))
  (let* ((main-loop (check-null (pipewire:make-main-loop (cffi:null-pointer))))
         (context (check-null (pipewire:make-context (pipewire:get-loop main-loop) (cffi:null-pointer) 0))))
    (unwind-protect
         (let ((core (pipewire:connect-context context (cffi:null-pointer) 0)))
           (unless (cffi:null-pointer-p core)
             (pipewire:disconnect-core core)
             T))
      (pipewire:destroy-context context)
      (pipewire:destroy-main-loop main-loop))))

(defclass segment (mixed:virtual)
  ((channel-order :initform () :initarg :channel-order :accessor mixed:channel-order)
   (mixed:program-name :initform "Mixed" :initarg :program-name :accessor mixed:program-name)
   (thread :initform NIL :accessor thread)
   (events :initform NIL :accessor events)
   (pw-loop :initform NIL :accessor pw-loop)
   (pw-stream :initform NIL :accessor pw-stream)))

(defun init-segment (segment direction)
  (unless (pipewire-present-p)
    (error "PipeWire is not present!"))
  (unless (events segment)
    (setf (events segment) (mixed::calloc '(:struct pipewire:stream-events))))
  (unless (pw-loop segment)
    (setf (pw-loop segment) (check-null (pipewire:make-main-loop (cffi:null-pointer)))))
  (cffi:with-foreign-objects ((builder '(:struct pipewire:pod-builder))
                              (audio-info '(:struct pipewire:audio-info))
                              (buffer :uint8 1024)
                              (param :pointer))
    (mixed::clear-mem builder '(:struct pipewire:pod-builder))
    (mixed::clear-mem audio-info '(:struct pipewire:audio-info))
    (setf (pipewire:stream-events-version (events segment)) pipewire:STREAM-EVENTS)
    (setf (pipewire:stream-events-process (events segment))
          (ecase direction
            (:output (cffi:callback drain))
            (:input (cffi:callback source))))
    (let ((props (check-null
                  (pipewire:make-properties :string "media.type" :string "Audio"
                                            :string "media.category" :string (ecase direction
                                                                               (:output "Playback")
                                                                               (:input "Capture"))
                                            :string "media.role" :string "Game"
                                            :string "media.software" :string "cl-mixed"
                                            :size 0))))
      (mixed::with-cleanup-on-failure (pipewire:free-properties props)
        (setf (pw-stream segment) (check-null (pipewire:make-stream (pipewire:get-loop (pw-loop segment))
                                                                    (mixed:program-name segment)
                                                                    props
                                                                    (events segment)
                                                                    (mixed:handle segment))))))
    (let* ((pack (mixed:pack segment))
           (channels (or (mixed:channel-order segment) (mixed:guess-channel-order-from-count (mixed:channels pack)))))
      (setf (pipewire:pod-builder-data builder) buffer)
      (setf (pipewire:pod-builder-size builder) 1024)
      (setf (pipewire:audio-info-format audio-info) (mixed:encoding pack))
      (setf (pipewire:audio-info-rate audio-info) (mixed:samplerate pack))
      (setf (pipewire:audio-info-channels audio-info) (length channels))
      (loop with audio-info-channels = (cffi:foreign-slot-pointer audio-info '(:struct pipewire:audio-info) 'position)
            for i from 0
            for channel in channels
            do (setf (cffi:mem-aref audio-info-channels 'pipewire:audio-channel) channel)))
    (setf (cffi:mem-ref param :pointer) (check-null (pipewire:make-audio-format builder :enum-format audio-info)))
    (check-call (pipewire:connect-stream (pw-stream segment) direction #xFFFFFFFF
                                         '(:autoconnect :map-buffers :rt-process)
                                         param 1))))

(defmethod mixed:start ((segment segment))
  (when (or (not (thread segment)) (not (bt:thread-alive-p (thread segment))))
    (let ((loop (pw-loop segment)))
      (setf (thread segment) (bt:make-thread (lambda () (pipewire:run-main-loop loop))
                                             :name (format NIL "~a" segment)))))
  segment)

(defmethod mixed:mix ((segment segment))
  segment)

(defmethod mixed:end ((segment segment))
  segment)

(defmethod mixed:free ((segment segment))
  (loop while (and (thread segment) (bt:thread-alive-p (thread segment)))
        do (pipewire:quit-main-loop (pw-loop segment))
           (sleep 0.01))
  (when (pw-stream segment)
    (pipewire:destroy-stream (pw-stream segment))
    (setf (pw-stream segment) NIL))
  (when (pw-loop segment)
    (pipewire:destroy-main-loop (pw-loop segment))
    (setf (pw-loop segment) NIL))
  (when (events segment)
    (cffi:foreign-free (events segment))
    (setf (events segment) NIL)))

(defclass drain (mixed:drain segment)
  ())

(defmethod initialize-instance :after ((drain drain) &key &allow-other-keys)
  (init-segment drain :output))

(mixed::define-callback drain :void ((segment :pointer)) ()
  (let* ((segment (mixed:pointer->object segment))
         (pack (mixed:pack segment)))
    (mixed:with-buffer-tx (data start avail-size pack)
      (if (< 0 avail-size)
          (let* ((stream (pw-stream segment))
                 (framesize (mixed:framesize pack))
                 (pw-buffer (pipewire:dequeue-buffer stream)))
            (if (not (cffi:null-pointer-p pw-buffer))
                (let* ((spa-buffer (pipewire:pw-buffer-buffer pw-buffer))
                       (data (pipewire:spa-buffer-data spa-buffer))
                       (chunk (pipewire:data-chunk data))
                       (size (min avail-size
                                  (pipewire:data-max-size data)
                                  (* (pipewire:pw-buffer-requested pw-buffer)
                                     framesize))))
                  ;; TODO: could probably submit the pack itself as a buffer and avoid the copying.
                  (unless (cffi:null-pointer-p (pipewire:data-data data))
                    (cffi:foreign-funcall "memcpy" :pointer (pipewire:data-data data) :pointer (mixed:data-ptr) :size size)
                    (setf (pipewire:chunk-offset chunk) 0)
                    (setf (pipewire:chunk-stride chunk) framesize)
                    (setf (pipewire:chunk-size chunk) size)
                    (pipewire:queue-buffer stream pw-buffer)
                    (mixed:finish size)))
                (format *error-output* "Overrun! (no buffers available to queue)~%")))
          (format *error-output* "Underrun! (no data ready to output)~%")))))

(defclass source (mixed:source segment)
  ())

(defmethod initialize-instance :after ((source source) &key)
  (init-segment source :input))

(mixed::define-callback source :void ((segment :pointer)) ()
  (let ((segment (mixed:pointer->object segment)))
    ))
