(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.xmp.cffi
  (:use #:cl)
  (:shadow #:sequence)
  (:export
   #:libxmp
   #:sequence
   #:sequence-entry-point
   #:sequence-duration
   #:channel
   #:channel-pan
   #:channel-volume
   #:channel-flags
   #:module
   #:module-name
   #:module-type
   #:module-pattern-count
   #:module-track-count
   #:module-tracks/pattern
   #:module-instrument-count
   #:module-sample-count
   #:module-speed
   #:module-bpm
   #:module-length
   #:module-restart-position
   #:module-volume
   #:module-patterns
   #:module-tracks
   #:module-instruments
   #:module-samples
   #:module-channels
   #:module-orders
   #:module-info
   #:module-info-module
   #:module-info-md5
   #:module-info-volume
   #:module-info-comment
   #:module-info-sequence-count
   #:module-info-sequences
   #:create-context
   #:free-context
   #:load-module
   #:load-module-from-memory
   #:release-module
   #:start-player
   #:end-player
   #:play-buffer
   #:seek-time
   #:get-module-info))
(in-package #:org.shirakumo.fraf.mixed.xmp.cffi)

(cffi:define-foreign-library libxmp
  (T (:or (:default "libxmp") (:default "xmp"))))

(cffi:defbitfield format
  (:8-bit    #b0001)
  (:unsigned #b0010)
  (:mono     #b0100))

(cffi:defcenum error
  (:ok 0)
  (:end -1)
  (:internal -2)
  (:format -3)
  (:load -4)
  (:depack -5)
  (:system -6)
  (:invalid -7)
  (:state -8))

(cffi:defcstruct (sequence :conc-name sequence-)
  (entry-point :int)
  (duration :int))

(cffi:defcstruct (channel :conc-name channel-)
  (pan :int)
  (volume :int)
  (flags :int))

(cffi:defcstruct (module :conc-name module-)
  (name :char :count 64)
  (type :char :count 64)
  (pattern-count :int)
  (track-count :int)
  (tracks/pattern :int)
  (instrument-count :int)
  (sample-count :int)
  (speed :int)
  (bpm :int)
  (length :int)
  (restart-position :int)
  (volume :int)
  (patterns :pointer)
  (tracks :pointer)
  (instruments :pointer)
  (samples :pointer)
  (channels (:struct channel) :count 64)
  (orders :char :count 256))

(cffi:defcstruct (module-info :conc-name module-info-)
  (md5 :char :count 16)
  (volume :int)
  (module (:pointer (:struct module)))
  (comment :string)
  (sequence-count :int)
  (sequences :pointer))

(cffi:defcfun (create-context "xmp_create_context") :pointer)

(cffi:defcfun (free-context "xmp_free_context") :void
  (context :pointer))

(cffi:defcfun (load-module "xmp_load_module") error
  (context :pointer)
  (path :string))

(cffi:defcfun (load-module-from-memory "xmp_load_module_from_memory") error
  (context :pointer)
  (pointer :pointer)
  (length :long))

(cffi:defcfun (release-module "xmp_release_module") :void
  (context :pointer))

(cffi:defcfun (start-player "xmp_start_player") error
  (context :pointer)
  (samplerate :int)
  (format format))

(cffi:defcfun (end-player "xmp_end_player") :void
  (context :pointer))

(cffi:defcfun (play-buffer "xmp_play_buffer") error
  (context :pointer)
  (buffer :pointer)
  (size :int)
  (loop :boolean))

(cffi:defcfun (seek-time "xmp_seek_time") error
  (context :pointer)
  (millis :int))

(cffi:defcfun (get-module-info "xmp_get_module_info") :void
  (context :pointer)
  (info :pointer))
