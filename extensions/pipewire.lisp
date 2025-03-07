(defpackage #:org.shirakumo.fraf.mixed.pipewire
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:pipewire #:org.shirakumo.fraf.mixed.pipewire.cffi))
  (:export
   #:pipewire-error
   #:code
   #:present-p
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

(defun present-p ()
  (unless (cffi:foreign-library-loaded-p 'pipewire:libpipewire)
    (handler-case (progn (cffi:load-foreign-library 'pipewire:libpipewire)
                         (cffi:load-foreign-library 'pipewire:pipewire-spa))
      (error () (return-from present-p (values NIL :no-such-library)))))
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
   #-pipewire-no-threads (thread :initform NIL :accessor thread)
   #-pipewire-no-threads (lock :initform (bt:make-lock) :accessor lock)
   #-pipewire-no-threads (cvar :initform (bt:make-condition-variable) :accessor cvar)
   (events :initform NIL :accessor events)
   (pw-loop :initform NIL :accessor pw-loop)
   (pw-stream :initform NIL :accessor pw-stream)
   (started-p :initform NIL :accessor started-p)))

(defun init-segment (segment direction)
  (unless (present-p)
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
    (setf (pipewire:stream-events-param-changed (events segment)) (cffi:callback param-changed))
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
    (let ((pack (mixed:pack segment)))
      (setf (pipewire:pod-builder-data builder) buffer)
      (setf (pipewire:pod-builder-size builder) 1024)
      (setf (pipewire:audio-info-format audio-info) (mixed:encoding pack))
      (setf (pipewire:audio-info-rate audio-info) (mixed:samplerate pack))
      (setf (pipewire:audio-info-channels audio-info) (mixed:channels pack))
      (setf (mixed:channel-order segment) (or (mixed:channel-order segment)
                                              (mixed:guess-channel-order-from-count (mixed:channels pack))))
      (loop with audio-info-channels = (cffi:foreign-slot-pointer audio-info '(:struct pipewire:audio-info) 'position)
            for i from 0 below (mixed:channels pack)
            for channel in (mixed:channel-order segment)
            do (setf (cffi:mem-aref audio-info-channels 'pipewire:audio-channel i) channel)))
    (setf (cffi:mem-ref param :pointer) (check-null (pipewire:make-audio-format builder :enum-format audio-info)))
    (check-call (pipewire:connect-stream (pw-stream segment) direction #xFFFFFFFF
                                         '(:autoconnect :map-buffers :rt-process)
                                         param 1))))

(defmethod mixed:start ((segment segment))
  (unless (started-p segment)
    #-pipewire-no-threads
    (when (or (not (thread segment)) (not (bt:thread-alive-p (thread segment))))
      (let ((loop (pw-loop segment)))
        (setf (thread segment) (bt:make-thread (lambda () (pipewire:run-main-loop loop))
                                               :name (format NIL "PipeWire-~a" (type-of segment))))))
    #+pipewire-no-threads
    (pipewire:enter-loop (pipewire:get-loop (pw-loop segment)))
    (setf (started-p segment) T)))

(defmethod mixed:mix ((segment segment))
  #+pipewire-no-threads
  (pipewire:iterate-loop (pipewire:get-loop (pw-loop segment)) 1))

(defmethod mixed:end ((segment segment))
  (when (started-p segment)
    #-pipewire-no-threads
    (when (pw-loop segment)
      (loop for i from 0 below 100
            do (pipewire:quit-main-loop (pw-loop segment))
               (unless (and (thread segment) (bt:thread-alive-p (thread segment)))
                 (return))
               (sleep 0.01)
            finally (progn
                      (bt:destroy-thread (thread segment))
                      (setf (thread segment) NIL))))
    #+pipewire-no-threads
    (pipewire:leave-loop (pipewire:get-loop (pw-loop segment)))
    (setf (started-p segment) NIL)))

(defmethod mixed:free ((segment segment))
  (mixed:end segment)
  (when (pw-stream segment)
    (pipewire:destroy-stream (pw-stream segment))
    (setf (pw-stream segment) NIL))
  (when (pw-loop segment)
    (pipewire:destroy-main-loop (pw-loop segment))
    (setf (pw-loop segment) NIL))
  (when (events segment)
    (cffi:foreign-free (events segment))
    (setf (events segment) NIL)))

(mixed::define-callback param-changed :void ((segment :pointer) (id pipewire:parameter-type) (param :pointer)) ()
  (unless (or (cffi:null-pointer-p param)
              (not (eql :format id)))
    (cffi:with-foreign-objects ((media-type 'pipewire:media-type)
                                (media-subtype 'pipewire:media-subtype)
                                (audio-info '(:struct pipewire:audio-info)))
      (when (and (<= 0 (pipewire:parse-audio-format param media-type media-subtype))
                 (eql :audio (cffi:mem-ref media-type 'pipewire:media-type))
                 (eql :raw (cffi:mem-ref media-subtype 'pipewire:media-subtype))
                 (<= 0 (pipewire:parse-raw-audio-format param audio-info)))
        (mixed::with-fetched-object (segment)
          (let ((pack (mixed:pack segment)))
            (unless (and (= (mixed:samplerate pack) (pipewire:audio-info-rate audio-info))
                         (= (mixed:channels pack) (pipewire:audio-info-channels audio-info))
                         (eql (mixed:encoding pack) (pipewire:audio-info-format audio-info)))
              (setf (mixed:samplerate pack) (pipewire:audio-info-rate audio-info))
              (setf (mixed:channels pack) (pipewire:audio-info-channels audio-info))
              (setf (mixed:encoding pack) (pipewire:audio-info-format audio-info))
              (setf (mixed:channel-order segment)
                    (loop with channels = (cffi:foreign-slot-pointer audio-info '(:struct pipewire:audio-info) 'pipewire::position)
                          for i from 0 below (pipewire:audio-info-channels audio-info)
                          collect (cffi:mem-aref channels 'pipewire:audio-channel i)))
              (format *error-output* "[PipeWire] Changed audio format to ~d channels @ ~dHz, ~a~%"
                      (mixed:channels pack) (mixed:samplerate pack) (mixed:encoding pack)))))))))

(defclass drain (segment mixed:drain)
  ())

(defmethod initialize-instance :after ((drain drain) &key &allow-other-keys)
  (init-segment drain :output))

(mixed::define-callback drain :void ((segment :pointer)) ()
  (declare (optimize speed))
  (mixed::with-fetched-object (segment)
    (let ((pack (mixed:pack segment)))
      (mixed:with-buffer-tx (data start avail-size pack)
        (when (< 0 avail-size)
          (let* ((stream (pw-stream segment))
                 (framesize (mixed:framesize pack))
                 (pw-buffer (pipewire:dequeue-buffer stream)))
            (declare (type (unsigned-byte 16) framesize))
            (unless (cffi:null-pointer-p pw-buffer)
              (let* ((spa-buffer (pipewire:pw-buffer-buffer pw-buffer))
                     (data (pipewire:spa-buffer-data spa-buffer))
                     (chunk (pipewire:data-chunk data))
                     (size (min avail-size
                                (the (unsigned-byte 32) (pipewire:data-max-size data))
                                (* (the (unsigned-byte 32) (pipewire:pw-buffer-requested pw-buffer))
                                   framesize))))
                ;; TODO: could probably submit the pack itself as a buffer and avoid the copying.
                (unless (cffi:null-pointer-p (pipewire:data-data data))
                  (cffi:foreign-funcall "memcpy" :pointer (pipewire:data-data data) :pointer (mixed:data-ptr) :size size)
                  (setf (pipewire:chunk-offset chunk) 0)
                  (setf (pipewire:chunk-stride chunk) framesize)
                  (setf (pipewire:chunk-size chunk) size)
                  (pipewire:queue-buffer stream pw-buffer)
                  (mixed:finish size)
                  (bt:condition-notify (cvar segment)))))))))))

#-pipewire-no-threads
(defmethod mixed:mix ((segment drain))
  (loop with pack = (mixed:pack segment)
        until (< 0 (mixed:available-write pack))
        do (bt:with-lock-held ((lock segment))
             (bt:condition-wait (cvar segment) (lock segment)))))

(defclass source (segment mixed:source)
  ())

(defmethod initialize-instance :after ((source source) &key)
  (init-segment source :input))

(mixed::define-callback source :void ((segment :pointer)) ()
  (mixed::with-fetched-object (segment)
    (let ((pack (mixed:pack segment)))
      (mixed:with-buffer-tx (data start avail-size pack :direction :output)
        (when (< 0 avail-size)
          (let* ((stream (pw-stream segment))
                 (pw-buffer (pipewire:dequeue-buffer stream)))
            (unless (cffi:null-pointer-p pw-buffer)
              (let* ((spa-buffer (pipewire:pw-buffer-buffer pw-buffer))
                     (data (pipewire:spa-buffer-data spa-buffer))
                     (chunk (pipewire:data-chunk data))
                     (size (min avail-size
                                (pipewire:data-max-size data)
                                (pipewire:chunk-size chunk))))
                (unless (cffi:null-pointer-p (pipewire:data-data data))
                  (cffi:foreign-funcall "memcpy" :pointer (mixed:data-ptr) :pointer (pipewire:data-data data) :size size)
                  (pipewire:queue-buffer stream pw-buffer)
                  (mixed:finish size)
                  (bt:condition-notify (cvar segment)))))))))))

#-pipewire-no-threads
(defmethod mixed:mix ((segment source))
  (loop with pack = (mixed:pack segment)
        until (< 0 (mixed:available-read pack))
        do (bt:with-lock-held ((lock segment))
             (bt:condition-wait (cvar segment) (lock segment)))))
