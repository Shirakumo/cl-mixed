(in-package #:org.shirakumo.fraf.mixed.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

#+windows
(progn
  (define-foreign-library winpthread
    (:windows "libwinpthread-1.dll"))
  (define-foreign-library gcc-s
    (:windows "libgcc_s_seh-1.dll")))

(define-foreign-library libmixed
  (:android
   (:or "libmixed.so"
        #+X86 "android-i686-libmixed.so"
        #+X86-64 "android-amd64-libmixed.so"
        #+ARM64 "android-arm64-libmixed.so"
        #+(and ARM (not ARM64)) "android-arm7a-libmixed.so"))
  (:darwin
   (:or "libmixed2.dylib" "libmixed.dylib"
        #+X86 "mac-i686-libmixed.dylib"
        #+X86-64 "mac-amd64-libmixed.dylib"
        #+ARM64 "mac-arm64-libmixed.dylib"))
  (:linux
   (:or "libmixed.so.2" "libmixed2.so" "libmixed.so"
        #+X86 "lin-i686-libmixed.so"
        #+X86-64 "lin-amd64-libmixed.so"
        #+ARM64 "lin-arm64-libmixed.so"
        #+(and ARM (not ARM64)) "lin-arm7a-libmixed.so"))
  (:freebsd
   (:or "libmixed.so.2" "libmixed2.so" "libmixed.so"
        #+X86 "bsd-i686-libmixed.so"
        #+X86-64 "bsd-amd64-libmixed.so"
        #+ARM64 "bsd-arm64-libmixed.so"
        #+(and ARM (not ARM64)) "bsd-arm7a-libmixed.so"))
  (:windows
   (:or "libmixed2.dll" "libmixed.dll"
        #+X86 "win-i686-libmixed.dll"
        #+X86-64 "win-amd64-libmixed.dll"
        #+ARM64 "win-arm64-libmixed.dll"
        #+(and ARM (not ARM64)) "win-arm7a-libmixed.so"))
  (:nx "libmixed.nro")
  (t (:default "mixed")))

(defctype size_t #+64-bit :uint64 #+32-bit :uint32)
(defctype ssize_t #+64-bit :int64 #+32-bit :int32)
(defctype channel_t :uint8)

(defcenum error
  (:no-error 0)
  :out-of-memory
  :unknown-encoding
  :unknown-layout
  :mixing-failed
  :not-implemented
  :not-initialized
  :invalid-location
  :invalid-field
  :invalid-value
  :segment-already-started
  :segment-already-ended
  :open-ladspa-failed
  :bad-ladspa-library
  :no-ladspa-plugin-at-index
  :ladspa-instantiation-failed
  :resample-failed
  :buffer-empty
  :buffer-full
  :buffer-overcommit
  :bad-resample-factor
  :bad-channel-configuration
  :buffer-allocated
  :buffer-missing
  :duplicate-segment
  :bad-segment)

(defcenum encoding
  (:int8 1)
  :uint8
  :int16
  :uint16
  :int24
  :uint24
  :int32
  :uint32
  :float
  :double)

(defcenum field
  :buffer
  :bypass
  :samplerate
  :volume
  :resample-type
  :volume-control-pan
  :fade-from
  :fade-to
  :fade-time
  :fade-type
  :generator-frequency
  :generator-type
  :space-location
  :space-direction
  :space-velocity
  :space-up
  :space-soundspeed
  :space-doppler-factor
  :space-min-distance
  :space-max-distance
  :space-rolloff
  :space-attenuation
  :delay-time
  :pitch-shift
  :gate-open-threshold
  :gate-close-threshold
  :gate-attack
  :gate-hold
  :gate-release
  :noise-type
  :repeat-time
  :repeat-mode
  :frequency
  :biquad-filter
  :gain
  :q
  :in-count
  :out-count
  :current-segment
  :speed-factor
  :quantize-steps
  :mix
  :plane-location
  :plane-velocity
  :compressor-pregain
  :compressor-threshold
  :compressor-knee
  :compressor-ratio
  :compressor-attack
  :compressor-release
  :compressor-predelay
  :compressor-releasezone
  :compressor-postgain
  :compressor-wet
  :compressor-gain
  :channel-count-in
  :channel-count-out
  :repeat-position
  :framesize
  :oversampling)

(defcenum resample-type
  (:sinc-best-quality 0)
  :sinc-medium-quality
  :sinc-fastest
  :zero-order-hold
  :linear-interpolation)

(defcenum attenuation
  (:none 1)
  :inverse
  :linear
  :exponential)

(defcenum fade-type
  (:linear 1)
  :cubic-in
  :cubic-out
  :cubic-in-out)

(defcenum generator-type
  (:sine 1)
  :square
  :triangle
  :sawtooth)

(defcenum noise-type
  (:white 1)
  :pink
  :brown)

(defcenum repeat-mode
  (:record 1)
  :play
  :record-once)

(defcenum biquad-filter
  (:lowpass 1)
  :highpass
  :bandpass
  :notch
  :peaking
  :allpass
  :lowshelf
  :highshelf)

(defbitfield info-flags
  (:inplace #x1)
  (:modifies-input #x2)
  (:in #x1)
  (:out #x2)
  (:segment #x4)
  (:set #x8)
  (:get #x10))

(defcenum location
  (:mono 0)
  (:left 0)
  (:right 1)
  (:left-front 0)
  (:right-front 1)
  (:left-rear 2)
  (:right-rear 3)
  (:center 4)
  (:subwoofer 5)
  (:left-front-bottom 0)
  (:right-front-bottom 1)
  (:left-rear-bottom 2)
  (:right-rear-bottom 3)
  (:center-front 4)
  (:left-side 6)
  (:right-side 7)
  (:left-front-center 6)
  (:right-front-center 7)
  (:left-front-top 8)
  (:right-front-top 9)
  (:left-rear-top 10)
  (:right-rear-top 11)
  (:center-rear 12)
  (:left-side-top 13)
  (:right-side-top 14)
  (:subwoofer-2 15)
  (:left-front-wide 16)
  (:right-front-wide 17))

(defcenum field-type
  (:unknown 0)
  :int8
  :uint8
  :int16
  :uint16
  :int24
  :uint24
  :int32
  :uint32
  :float
  :double
  :bool
  :size_t
  :string
  :function
  :pointer
  :segment-pointer
  :buffer-pointer
  :pack-pointer
  :chain-pointer
  :location-enum
  :biquad-filter-enum
  :repeat-mode-enum
  :noise-type-enum
  :generator-type-enum
  :fade-type-enum
  :attenuation-enum
  :encoding-enum
  :error-enum
  :resample-type-enum
  :channel_t)

(declaim (ftype (function (cffi:foreign-pointer) (unsigned-byte 32))
                buffer-size buffer-read buffer-write buffer-reserved
                pack-size pack-read pack-write pack-reserved))
(defcstruct (buffer :class buffer :conc-name buffer-)
  (data :pointer)
  (size :uint32)
  (read :uint32)
  (write :uint32)
  (reserved :uint32)
  (virtual-p :char))

(defcstruct (pack :class pack :conc-name pack-)
  (data :pointer)
  (size :uint32)
  (read :uint32)
  (write :uint32)
  (reserved :uint32)
  (encoding encoding)
  (channels :uint8)
  (samplerate :uint32))

(defcstruct (field-info :class field-info :conc-name field-info-)
  (field :uint32)
  (description :string)
  (flags info-flags)
  (type field-type)
  (type-count :uint32))

(defcstruct (segment-info :class segment-info :conc-name segment-info-)
  (name :string)
  (description :string)
  (flags :int)
  (min-inputs :uint32)
  (max-inputs :uint32)
  (outputs :uint32)
  (fields (:struct field-info) :count 32))

(cffi:defcstruct (segment :class segment :conc-name direct-segment-)
  (free :pointer)
  (info :pointer)
  (start :pointer)
  (mix :pointer)
  (end :pointer)
  (set-in :pointer)
  (set-out :pointer)
  (get-in :pointer)
  (get-out :pointer)
  (set :pointer)
  (get :pointer)
  (data :pointer))

(defcfun (make-pack "mixed_make_pack") :int
  (frames :uint32)
  (pack :pointer))

(defcfun (free-pack "mixed_free_pack") :void
  (pack :pointer))

(defcfun (clear-pack "mixed_pack_clear") :int
  (pack :pointer))

(defcfun (pack-available-write "mixed_pack_available_write") :uint32
  (pack :pointer))

(defcfun (pack-available-read "mixed_pack_available_read") :uint32
  (pack :pointer))

(defcfun (pack-request-write "mixed_pack_request_write") :int
  (area :pointer)
  (size :pointer)
  (pack :pointer))

(defcfun (pack-finish-write "mixed_pack_finish_write") :int
  (size :uint32)
  (pack :pointer))

(defcfun (pack-request-read "mixed_pack_request_read") :int
  (area :pointer)
  (size :pointer)
  (pack :pointer))

(defcfun (pack-finish-read "mixed_pack_finish_read") :int
  (size :uint32)
  (pack :pointer))

(defcfun (make-buffer "mixed_make_buffer") :int
  (size :uint32)
  (buffer :pointer))

(defcfun (free-buffer "mixed_free_buffer") :void
  (buffer :pointer))

(defcfun (buffer-from-pack "mixed_buffer_from_pack") :int
  (pack :pointer)
  (buffers :pointer)
  (volume :pointer)
  (target-volume :float))

(defcfun (buffer-to-pack "mixed_buffer_to_pack") :int
  (buffers :pointer)
  (pack :pointer)
  (volume :pointer)
  (target-volume :float))

(defcfun (transfer-buffer "mixed_buffer_transfer") :int
  (from :pointer)
  (to :pointer))

(defcfun (copy-buffer "mixed_buffer_copy") :int
  (from :pointer)
  (to :pointer))

(defcfun (clear-buffer "mixed_buffer_clear") :int
  (buffer :pointer))

(defcfun (resize-buffer "mixed_buffer_resize") :int
  (size :uint32)
  (buffer :pointer))

(defcfun (buffer-available-write "mixed_buffer_available_write") :uint32
  (buffer :pointer))

(defcfun (buffer-available-read "mixed_buffer_available_read") :uint32
  (buffer :pointer))

(defcfun (buffer-request-write "mixed_buffer_request_write") :int
  (area :pointer)
  (size :pointer)
  (buffer :pointer))

(defcfun (buffer-finish-write "mixed_buffer_finish_write") :int
  (size :uint32)
  (buffer :pointer))

(defcfun (buffer-request-read "mixed_buffer_request_read") :int
  (area :pointer)
  (size :pointer)
  (buffer :pointer))

(defcfun (buffer-finish-read "mixed_buffer_finish_read") :int
  (size :uint32)
  (buffer :pointer))

(defcfun (free-segment "mixed_free_segment") :int
  (segment :pointer))

(defcfun (segment-start "mixed_segment_start") :int
  (segment :pointer))

(defcfun (segment-mix "mixed_segment_mix") :int
  (segment :pointer))

(defcfun (segment-end "mixed_segment_end") :int
  (segment :pointer))

(defcfun (segment-set-in "mixed_segment_set_in") :int
  (field field)
  (location location)
  (value :pointer)
  (segment :pointer))

(defcfun (segment-set-out "mixed_segment_set_out") :int
  (field field)
  (location location)
  (value :pointer)
  (segment :pointer))

(defcfun (segment-get-in "mixed_segment_get_in") :int
  (field field)
  (location location)
  (value :pointer)
  (segment :pointer))

(defcfun (segment-get-out "mixed_segment_get_out") :int
  (field field)
  (location location)
  (value :pointer)
  (segment :pointer))

(defcfun (segment-info "mixed_segment_info") :int
  (info :pointer)
  (segment :pointer))

(defcfun (segment-set "mixed_segment_set") :int
  (field field)
  (value :pointer)
  (segment :pointer))

(defcfun (segment-get "mixed_segment_get") :int
  (field field)
  (value :pointer)
  (segment :pointer))

(defcfun (make-segment-unpacker "mixed_make_segment_unpacker") :int
  (pack :pointer)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-packer "mixed_make_segment_packer") :int
  (pack :pointer)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-basic-mixer "mixed_make_segment_basic_mixer") :int
  (channels :uint32)
  (segment :pointer))

(defcfun (make-segment-volume-control "mixed_make_segment_volume_control") :int
  (volume :float)
  (pan :float)
  (segment :pointer))

(defcfun (make-segment-fade "mixed_make_segment_fade") :int
  (from :float)
  (to :float)
  (time :float)
  (type fade-type)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-generator "mixed_make_segment_generator") :int
  (type generator-type)
  (frequency :uint32)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-ladspa "mixed_make_segment_ladspa") :int
  (file :string)
  (index :uint32)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-space-mixer "mixed_make_segment_space_mixer") :int
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-plane-mixer "mixed_make_segment_plane_mixer") :int
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-delay "mixed_make_segment_delay") :int
  (time :float)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-repeat "mixed_make_segment_repeat") :int
  (time :float)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-pitch "mixed_make_segment_pitch") :int
  (pitch :float)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-gate "mixed_make_segment_gate") :int
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-noise "mixed_make_segment_noise") :int
  (type noise-type)
  (segment :pointer))

(defcfun (make-segment-biquad-filter "mixed_make_segment_biquad_filter") :int
  (pass biquad-filter)
  (frequency :float)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-speed-change "mixed_make_segment_speed_change") :int
  (speed :double)
  (segment :pointer))

(defcfun (make-segment-distribute "mixed_make_segment_distribute") :int
  (segment :pointer))

(defcfun (make-segment-queue "mixed_make_segment_queue") :int
  (segment :pointer))

(defcfun (queue-add "mixed_queue_add") :int
  (new :pointer)
  (queue :pointer))

(defcfun (queue-remove "mixed_queue_remove") :int
  (old :pointer)
  (queue :pointer))

(defcfun (queue-remove-at "mixed_queue_remove_at") :int
  (pos :uint32)
  (queue :pointer))

(defcfun (queue-clear "mixed_queue_clear") :int
  (queue :pointer))

(defcfun (make-segment-void "mixed_make_segment_void") :int
  (segment :pointer))

(defcfun (make-segment-zero "mixed_make_segment_zero") :int
  (segment :pointer))

(defcfun (make-segment-channel-convert "mixed_make_segment_channel_convert") :int
  (in :uint8)
  (out :uint8)
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-quantize "mixed_make_segment_quantize") :int
  (steps :uint32)
  (segment :pointer))

(defcfun (make-segment-chain "mixed_make_segment_chain") :int
  (segment :pointer))

(defcfun (chain-add "mixed_chain_add") :int
  (segment :pointer)
  (chain :pointer))

(defcfun (chain-add-at "mixed_chain_add_at") :int
  (i :uint32)
  (segment :pointer)
  (chain :pointer))

(defcfun (chain-remove "mixed_chain_remove") :int
  (segment :pointer)
  (chain :pointer))

(defcfun (chain-remove-at "mixed_chain_remove_at") :int
  (i :uint32)
  (chain :pointer))

(defcfun (make-segment-spatial-reverb "mixed_make_segment_spatial_reverb") :int
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-fwd-fft "mixed_make_segment_fwd_fft") :int
  (samplerate :uint32)
  (segment :pointer))

(defcfun (make-segment-inv-fft "mixed_make_segment_inv_fft") :int
  (samplerate :uint32)
  (segment :pointer))

(defcfun (load-plugin "mixed_load_plugin") :int
  (file :string))

(defcfun (close-plugin "mixed_close_plugin") :int
  (file :string))

(defcfun (register-segment "mixed_register_segment") :int
  (name :string)
  (argc :uint32)
  (args :pointer)
  (function :pointer))

(defcfun (deregister-segment "mixed_deregister_segment") :int
  (name :string))

(defcfun (list-segments "mixed_list_segments") :int
  (count :pointer)
  (names :pointer))

(defcfun (make-segment-info "mixed_make_segment_info") :int
  (name :string)
  (argc :pointer)
  (args :pointer))

(defcfun (make-segment "mixed_make_segment") :int
  (name :string)
  (args :pointer)
  (segment :pointer))

(defcfun (samplesize "mixed_samplesize") :uint8
  (encoding encoding))

(defcfun (fwd-fft "mixed_fwd_fft") :int
  (framesize :uint16)
  (in :pointer)
  (out :pointer))

(defcfun (inv-fft "mixed_inv_fft") :int
  (framesize :uint16)
  (in :pointer)
  (out :pointer))

(defcfun (translator-from "mixed_translator_from") :pointer
  (encoding encoding))

(defcfun (translator-to "mixed_translator_to") :pointer
  (encoding encoding))

(defcfun (error "mixed_error") error)

(defcfun (error-string "mixed_error_string") :string
  (error error))

(defcfun (type-string "mixed_type_string") :string
  (type field-type))

(defcfun (version "mixed_version") :string)
