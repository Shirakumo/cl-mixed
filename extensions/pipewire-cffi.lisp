(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.pipewire.cffi
  (:use #:cl)
  (:export
   #:libpipewire
   #:pipewire-spa
   #:STREAM-EVENTS
   #:audio-format
   #:audio-channel
   #:parameter-type
   #:media-type
   #:media-subtype
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
   #:audio-info
   #:audio-info-format
   #:audio-info-flags
   #:audio-info-rate
   #:audio-info-channels
   #:audio-info-position
   #:make-audio-format
   #:parse-audio-format
   #:parse-raw-audio-format
   #:make-stream
   #:init
   #:deinit
   #:make-main-loop
   #:get-loop
   #:make-properties
   #:free-properties
   #:connect-stream
   #:run-main-loop
   #:quit-main-loop
   #:enter-loop
   #:leave-loop
   #:iterate-loop
   #:destroy-stream
   #:destroy-main-loop
   #:queue-buffer
   #:dequeue-buffer
   #:make-context
   #:destroy-context
   #:connect-context
   #:disconnect-core))
(in-package #:org.shirakumo.fraf.mixed.pipewire.cffi)

(cffi:define-foreign-library libpipewire
  (T (:or "libpipewire-0.3.so" (:default "libpipewire"))))

(cffi:define-foreign-library pipewire-spa
  (:linux (:or #+ARM64 "pipewire-spa-arm64.so"
               #+X86-64 "pipewire-spa-amd64.so"
               "pipewire-spa.so")))

(defconstant STREAM-EVENTS 2)

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

(cffi:defcenum (audio-channel :int32)
  :unknown
  :n/a
  :mono
  (:left-front 3)
  (:left-front-bottom 3)
  (:right-front 4)
  (:right-front-bottom 4)
  (:center-front 5)
  (:subwoofer 6)
  (:left-side 7)
  (:right-side 8)
  (:left-front-center 9)
  (:right-front-center 10)
  (:center-rear 11)
  (:left-rear 12)
  (:left-rear-bottom 12)
  (:right-rear 13)
  (:right-rear-bottom 13)
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

(cffi:defcenum (direction :int32)
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

(cffi:defcenum (parameter-type :int32)
  :invalid
  :prop-info
  :props
  :enum-format
  :format
  :buffers
  :meta
  :io
  :enum-profile
  :profile
  :enum-port-config
  :port-config
  :enum-route
  :route
  :control
  :latency
  :process-latency
  :tag)

(cffi:defcenum (media-type :int32)
  :unknown
  :audio
  :video
  :image
  :binary
  :stream
  :application)

(cffi:defcenum (media-subtype :int32)
  :unknown
  :raw
  :dsp
  :iec958
  :dsd
  (:start-audio #x10000)
  :mp3
  :aac
  :vorbis
  :wma
  :ra
  :sbc
  :adpcm
  :g723
  :g726
  :g729
  :amr
  :gsm
  :alac
  :flac
  :ape
  :opus
  (:start-video #x20000)
  :h264
  :mjpg
  :dv
  :mpegts
  :h263
  :mpeg1
  :mpeg2
  :mpeg4
  :xvid
  :vc1
  :vp8
  :vp9
  :bayer
  (:start-image #x30000)
  :jpeg
  (:start-binary #x40000)
  (:start-stream #x50000)
  :midi
  (:start-application #x60000)
  :control)

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

(cffi:defcstruct (audio-info :conc-name audio-info-)
  (format audio-format)
  (flags :uint32)
  (rate :uint32)
  (channels :uint32)
  (position audio-channel :count 64))

(cffi:defcfun (make-audio-format "_spa_format_audio_raw_build") :pointer
  (builder :pointer)
  (id parameter-type)
  (info :pointer))

(cffi:defcfun (parse-audio-format "_spa_format_parse") :int
  (format :pointer)
  (media-type :uint32)
  (media-subtype :uint32))

(cffi:defcfun (parse-raw-audio-format "_spa_format_audio_raw_parse") :int
  (format :pointer)
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

(cffi:defcfun (free-properties "pw_properties_free") :void
  (properties :pointer))

(cffi:defcfun (connect-stream "pw_stream_connect") :int
  (stream :pointer)
  (direction direction)
  (target-id :uint32)
  (flags stream-flags)
  (params :pointer)
  (param-count :uint32))

(cffi:defcfun (run-main-loop "pw_main_loop_run") :int
  (loop :pointer))

(cffi:defcfun (quit-main-loop "pw_main_loop_quit") :int
  (loop :pointer))

(cffi:defcfun (enter-loop "_pw_loop_enter") :int
  (loop :pointer))

(cffi:defcfun (leave-loop "_pw_loop_leave") :int
  (loop :pointer))

(cffi:defcfun (iterate-loop "_pw_loop_iterate") :int
  (loop :pointer))

(cffi:defcfun (destroy-stream "pw_stream_destroy") :void
  (stream :pointer))

(cffi:defcfun (destroy-main-loop "pw_main_loop_destroy") :void
  (loop :pointer))

(cffi:defcfun (queue-buffer "pw_stream_queue_buffer") :int
  (stream :pointer)
  (buffer :pointer))

(cffi:defcfun (dequeue-buffer "pw_stream_dequeue_buffer") :pointer
  (stream :pointer))

(cffi:defcfun (make-context "pw_context_new") :pointer
  (loop :pointer)
  (properties :pointer)
  (user-data-size :size))

(cffi:defcfun (destroy-context "pw_context_destroy") :void
  (context :pointer))

(cffi:defcfun (connect-context "pw_context_connect") :pointer
  (context :pointer)
  (properties :pointer)
  (user-data-size :size))

(cffi:defcfun (disconnect-core "pw_core_disconnect") :int
  (core :pointer))
