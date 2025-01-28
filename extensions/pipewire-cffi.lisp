(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.pipewire.cffi
  (:use #:cl)
  (:export
   #:libpipewire
   #:audio-format
   #:audio-channel
   #:stream-events
   #:stream-events-version
   #:stream-events-destroy
   #:stream-events-state-changed
   #:stream-events-control-info
   #:stream-events-io-changed
   #:stream-events-param-changed
   #:stream-events-add-buffer
   #:stream-events-remove-buffer
   #:stream-events-process
   #:stream-events-drained
   #:stream-events-command
   #:stream-events-trigger-done
   #:pw-buffer
   #:pw-buffer-buffer
   #:pw-buffer-user-data
   #:pw-buffer-size
   #:pw-buffer-requested
   #:pw-buffer-time
   #:meta
   #:meta-type
   #:meta-size
   #:meta-data
   #:chunk
   #:chunk-offset
   #:chunk-size
   #:chunk-stride
   #:chunk-flags
   #:data
   #:data-type
   #:data-flags
   #:data-fd
   #:data-map-offset
   #:data-max-size
   #:data-data
   #:data-chunk
   #:spa-buffer
   #:spa-buffer-meta-count
   #:spa-buffer-data-count
   #:spa-buffer-meta
   #:spa-buffer-data
   #:pod-builder-state
   #:pod-builder-state-offset
   #:pod-builder-state-flags
   #:pod-builder-state-frame
   #:callbacks
   #:callbacks-funcs
   #:callbacks-data
   #:pod-builder
   #:pod-builder-data
   #:pod-builder-size
   #:pod-builder-padding
   #:pod-builder-state
   #:pod-builder-callbacks
   #:pod
   #:pod-size
   #:pod-type
   #:frame
   #:frame-pod
   #:frame-parent
   #:frame-offset
   #:frame-flags
   #:make-audio-format
   #:make-stream
   #:init
   #:deinit
   #:make-main-loop
   #:get-loop
   #:make-properties
   #:connect-stream
   #:run-main-loop
   #:destroy-stream
   #:destroy-main-loop
   #:queue-buffer
   #:dequeue-buffer))
(in-package #:org.shirakumo.fraf.mixed.pipewire.cffi)

(cffi:define-foreign-library libpipewire
  (T (:or "libpipewire-0.3.so" (:default "libpipewire"))))

(cffi:defcenum audio-format
  :unknown
  :encoded
  (:start-interleaved #x100)
  :s8
  :u8
  :int16
  :s16-be
  :uint16
  :u16-be
  :s24-32-le
  :s24-32-be
  :u24-32-le
  :u24-32-be
  :int32
  :s32-be
  :uint32
  :u32-be
  :int24
  :s24-be
  :uint24
  :u24-be
  :s20-le
  :s20-be
  :u20-le
  :u20-be
  :s18-le
  :s18-be
  :u18-le
  :u18-be
  :float
  :f32-be
  :double
  :f64-be
  :ulaw
  :alaw
  (:start-planar #x200)
  :u8p
  :s16p
  :s24-32p
  :s32p
  :s24p
  :f32p
  :f64p
  :s8p)

(cffi:defcenum audio-channel
  :unknown
  :n/a
  :mono
  :left-front
  :right-front
  :center-front
  :subwoofer
  :left-side
  :right-side
  :left-front-center
  :right-front-center
  :center-rear
  :left-rear
  :right-rear
  :center-top
  :left-front-top
  :center-front-top
  :right-front-top
  :left-rear-top
  :center-rear-top
  :right-rear-top
  :left-rear-center
  :right-rear-center
  :left-front-wide
  :right-front-wide
  :subwoofer-2
  :left-front-high
  :center-front-high
  :right-front-high
  :left-front-center-top
  :right-front-center-top
  :left-side-top
  :right-side-top
  :subwoofer-left
  :subwoofer-right
  :center-bottom
  :left-center-bottom
  :right-center-bottom)

(cffi:defcenum direction
  :input
  :output)

(cffi:defbitfield stream-flags
  (:none 0)
  (:autoconnect #.(ash 1 0))
  (:inactive #.(ash 1 1))
  (:map-buffers #.(ash 1 2))
  (:driver #.(ash 1 3))
  (:rt-process #.(ash 1 4))
  (:no-convert #.(ash 1 5))
  (:exclusive #.(ash 1 6))
  (:dont-reconnect #.(ash 1 7))
  (:alloc-buffers #.(ash 1 8))
  (:trigger #.(ash 1 9))
  (:async #.(ash 1 10))
  (:early-process #.(ash 1 11))
  (:rt-trigger-done #.(ash 1 12)))

(cffi:defcstruct (stream-events :conc-name stream-events-)
  (version :uint32)
  (destroy :pointer)
  (state-changed :pointer)
  (control-info :pointer)
  (io-changed :pointer)
  (param-changed :pointer)
  (add-buffer :pointer)
  (remove-buffer :pointer)
  (process :pointer)
  (drained :pointer)
  (command :pointer)
  (trigger-done :pointer))

(cffi:defcstruct (pw-buffer :conc-name pw-buffer-)
  (buffer :pointer)
  (user-data :pointer)
  (size :uint64)
  (requested :uint64)
  (time :uint64))

(cffi:defcstruct (meta :conc-name meta-)
  (type :uint32)
  (size :uint32)
  (data :pointer))

(cffi:defcstruct (chunk :conc-name chunk-)
  (offset :uint32)
  (size :uint32)
  (stride :int32)
  (flags :int32))

(cffi:defcstruct (data :conc-name data-)
  (type :uint32)
  (flags :uint32)
  (fd :int64)
  (map-offset :uint32)
  (max-size :uint32)
  (data :pointer)
  (chunk :pointer))

(cffi:defcstruct (spa-buffer :conc-name spa-buffer-)
  (meta-count :uint32)
  (data-count :uint32)
  (meta :pointer)
  (data :pointer))

(cffi:defcstruct (pod-builder-state :conc-name pod-builder-state-)
  (offset :uint32)
  (flags :uint32)
  (frame :pointer))

(cffi:defcstruct (callbacks :conc-name callbacks-)
  (funcs :pointer)
  (data :pointer))

(cffi:defcstruct (pod-builder :conc-name pod-builder-)
  (data :pointer)
  (size :uint32)
  (padding :uint32)
  (state (:struct pod-builder-state))
  (callbacks (:struct callbacks)))

(cffi:defcstruct (pod :conc-name pod-)
  (size :uint32)
  (type :uint32))

(cffi:defcstruct (frame :conc-name frame-)
  (pod (:struct pod))
  (parent :pointer)
  (offset :uint32)
  (flags :uint32))

(cffi:defcfun (make-audio-format "spa_format_audio_raw_build") :pointer
  (builder :pointer)
  (id :uint32)
  (info :pointer))

(cffi:defcfun (make-stream "pw_stream_new_simple") :pointer
  (loop :pointer)
  (name :string)
  (props :pointer)
  (events :pointer)
  (data :pointer))

(cffi:defcfun (init "pw_init") :void
  (argc :pointer)
  (argv :pointer))

(cffi:defcfun (deinit "pw_deinit") :void)

(cffi:defcfun (make-main-loop "pw_main_loop_new") :pointer
  (props :pointer))

(cffi:defcfun (get-loop "pw_main_loop_get_loop") :pointer
  (loop :pointer))

(cffi:defcfun (make-properties "pw_properties_new") :pointer
  &rest)

(cffi:defcfun (connect-stream "pw_stream_connect") :int
  (stream :pointer)
  (direction direction)
  (target-id :uint32)
  (flags stream-flags)
  (params :pointer)
  (param-count :uint32))

(cffi:defcfun (run-main-loop "pw_main_loop_run") :int
  (loop :pointer))

(cffi:defcfun (destroy-stream "pw_stream_destroy") :void
  (stream :pointer))

(cffi:defcfun (destroy-main-loop "pw_main_loop_destroy") :void
  (loop :pointer))

(cffi:defcfun (queue-buffer "pw_stream_queue_buffer") :int
  (stream :pointer)
  (buffer :pointer))

(cffi:defcfun (dequeue-buffer "pw_stream_dequeue_buffer") :int
  (stream :pointer)
  (buffer :pointer))
