#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(define-foreign-library libmixed
  (:darwin (:or "libmixed2.dylib" "libmixed.dylib"
                #+X86 "mac32-libmixed.dylib"
                #+X86-64 "mac64-libmixed.dylib"))
  (:unix (:or "libmixed.so.2" "libmixed2.so" "libmixed.so"
              #+X86 "lin32-libmixed.so"
              #+X86-64 "lin64-libmixed.so"))
  (:windows (:or "libmixed2.dll" "libmixed.dll"
                 #+X86 "win32-libmixed.dll"
                 #+X86-64 "win64-libmixed.dll"))
  (t (:default "mixed")))

(use-foreign-library libmixed)

(defctype size_t #+x86-64 :uint64 #+x86 :uint32)

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
  :frequency-cutoff
  :frequency-pass
  :in-count
  :out-count
  :current-segment
  :speed-factor)

(defcenum resample-type
  (:sinc-best-quality 0)
  :sinc-medium-quality
  :sinc-fastest
  :zero-order-hold
  :linear-interpolation)

(defcenum attenuation
  (:no-attenuation 1)
  :inverse-attenuation
  :linear-attenuation
  :exponential-attenuation)

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
  (:white-noise 1)
  :pink-noise
  :brown-noise)

(defcenum repeat-mode
  (:record 1)
  :play)

(defcenum frequency-pass
  (:low 1)
  :high)

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
  (:subwoofer 5))

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
  :segment-sequence-pointer
  :location-enum
  :frequency-pass-enum
  :repeat-mode-enum
  :noise-type-enum
  :generator-type-enum
  :fade-type-enum
  :attenuation-enum
  :encoding-enum
  :error-enum
  :resample-type-enum)

(defcstruct (buffer :class buffer :conc-name buffer-)
  (data :pointer)
  (size size_t)
  (r1-start size_t)
  (r1-size size_t)
  (r2-start size_t)
  (r2-size size_t)
  (reserved-start size_t)
  (reserved-size size_t)
  (virtual-p :char))

(defcstruct (pack :class pack :conc-name pack-)
  (data :pointer)
  (size size_t)
  (r1-start size_t)
  (r1-size size_t)
  (r2-start size_t)
  (r2-size size_t)
  (reserved-start size_t)
  (reserved-size size_t)
  (encoding encoding)
  (channels :uint8)
  (samplerate size_t))

(defcstruct (field-info :class field-info :conc-name field-info-)
  (field size_t)
  (description :string)
  (flags :int)
  (type field-type)
  (type-count size_t))

(defcstruct (segment-info :class segment-info :conc-name segment-info-)
  (name :string)
  (description :string)
  (flags :int)
  (min-inputs size_t)
  (max-inputs size_t)
  (outputs size_t)
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

(defcstruct (segment-sequence :class segment-sequence :conc-name segment-sequence-)
  (segments :pointer)
  (count size_t)
  (size size_t))

(defcfun (make-pack "mixed_make_pack") :int
  (frames size_t)
  (pack :pointer))

(defcfun (free-pack "mixed_free_pack") :void
  (pack :pointer))

(defcfun (clear-pack "mixed_pack_clear") :int
  (pack :pointer))

(defcfun (pack-available-write "mixed_pack_available_write") size_t
  (pack :pointer))

(defcfun (pack-available-read "mixed_pack_available_read") size_t
  (pack :pointer))

(defcfun (pack-request-write "mixed_pack_request_write") :int
  (area :pointer)
  (size :pointer)
  (pack :pointer))

(defcfun (pack-finish-write "mixed_pack_finish_write") :int
  (size size_t)
  (pack :pointer))

(defcfun (pack-request-read "mixed_pack_request_read") :int
  (area :pointer)
  (size :pointer)
  (pack :pointer))

(defcfun (pack-finish-read "mixed_pack_finish_read") :int
  (size size_t)
  (pack :pointer))

(defcfun (make-buffer "mixed_make_buffer") :int
  (size size_t)
  (buffer :pointer))

(defcfun (free-buffer "mixed_free_buffer") :void
  (buffer :pointer))

(defcfun (buffer-from-pack "mixed_buffer_from_pack") :int
  (pack :pointer)
  (buffers :pointer)
  (samples size_t)
  (volume :float))

(defcfun (buffer-to-pack "mixed_buffer_to_pack") :int
  (buffers :pointer)
  (pack :pointer)
  (samples size_t)
  (volume :float))

(defcfun (transfer-buffer "mixed_buffer_transfer") :int
  (from :pointer)
  (to :pointer))

(defcfun (copy-buffer "mixed_buffer_copy") :int
  (from :pointer)
  (to :pointer))

(defcfun (clear-buffer "mixed_buffer_clear") :int
  (buffer :pointer))

(defcfun (resize-buffer "mixed_buffer_resize") :int
  (size size_t)
  (buffer :pointer))

(defcfun (buffer-available-write "mixed_buffer_available_write") size_t
  (buffer :pointer))

(defcfun (buffer-available-read "mixed_buffer_available_read") size_t
  (buffer :pointer))

(defcfun (buffer-request-write "mixed_buffer_request_write") :int
  (area :pointer)
  (size :pointer)
  (buffer :pointer))

(defcfun (buffer-finish-write "mixed_buffer_finish_write") :int
  (size size_t)
  (buffer :pointer))

(defcfun (buffer-request-read "mixed_buffer_request_read") :int
  (area :pointer)
  (size :pointer)
  (buffer :pointer))

(defcfun (buffer-finish-read "mixed_buffer_finish_read") :int
  (size size_t)
  (buffer :pointer))

(defcfun (free-segment "mixed_free_segment") :int
  (segment :pointer))

(defcfun (segment-start "mixed_segment_start") :int
  (segment :pointer))

(defcfun (segment-mix "mixed_segment_mix") :int
  (samples size_t)
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
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-packer "mixed_make_segment_packer") :int
  (pack :pointer)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-basic-mixer "mixed_make_segment_basic_mixer") :int
  (channels size_t)
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
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-generator "mixed_make_segment_generator") :int
  (type generator-type)
  (frequency size_t)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-ladspa "mixed_make_segment_ladspa") :int
  (file :string)
  (index size_t)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-space-mixer "mixed_make_segment_space_mixer") :int
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-delay "mixed_make_segment_delay") :int
  (time :float)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-repeat "mixed_make_segment_repeat") :int
  (time :float)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-pitch "mixed_make_segment_pitch") :int
  (pitch :float)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-gate "mixed_make_segment_gate") :int
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-noise "mixed_make_segment_noise") :int
  (type noise-type)
  (segment :pointer))

(defcfun (make-segment-frequency-pass "mixed_make_segment_frequency_pass") :int
  (pass frequency-pass)
  (cutoff size_t)
  (samplerate size_t)
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
  (pos size_t)
  (queue :pointer))

(defcfun (queue-clear "mixed_queue_clear") :int
  (queue :pointer))

(defcfun (make-segment-void "mixed_make_segment_void") :int
  (segment :pointer))

(defcfun (make-segment-zero "mixed_make_segment_zero") :int
  (segment :pointer))

(defcfun (make-segment-noise "mixed_make_segment_channel_convert") :int
  (in :uint8)
  (out :uint8)
  (segment :pointer))

(defcfun (free-segment-sequence "mixed_free_segment_sequence") :void
  (segment :pointer))

(defcfun (segment-sequence-add "mixed_segment_sequence_add") :int
  (segment :pointer)
  (mixer :pointer))

(defcfun (segment-sequence-remove "mixed_segment_sequence_remove") :int
  (segment :pointer)
  (mixer :pointer))

(defcfun (segment-sequence-start "mixed_segment_sequence_start") :int
  (mixer :pointer))

(defcfun (segment-sequence-mix "mixed_segment_sequence_mix") :int
  (samples size_t)
  (mixer :pointer))

(defcfun (segment-sequence-end "mixed_segment_sequence_end") :int
  (mixer :pointer))

(defcfun (load-plugin "mixed_load_plugin") :int
  (file :string))

(defcfun (close-plugin "mixed_close_plugin") :int
  (file :string))

(defcfun (register-segment "mixed_register_segment") :int
  (name :string)
  (argc size_t)
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
