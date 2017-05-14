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
  (:darwin (:or "libmixed.dylib" "libmixed.so"
                #+X86 "mac32-libmixed.dylib"
                #+X86-64 "mac64-libmixed.dylib"))
  (:unix (:or "libmixed.so"
              #+X86 "lin32-libmixed.so"
              #+X86-64 "lin64-libmixed.so"))
  (:windows (:or "out123.dll"
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
  :segment-already-started
  :segment-already-ended
  :open-ladspa-failed
  :bad-ladspa-library
  :no-ladspa-plugin-at-index
  :ladspa-instantiation-failed)

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

(defcenum layout
  (:alternating 1)
  :sequential)

(defcenum field
  :buffer
  :channel-resampler
  :general-volume
  :general-pan
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
  :space-attenuation)

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

(defcenum info-flags
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

(defcstruct (buffer :class buffer :conc-name buffer-)
  (data :pointer)
  (size size_t))

(defcstruct (channel :class channel :conc-name channel-)
  (data :pointer)
  (size size_t)
  (encoding encoding)
  (channels :uint8)
  (layout layout)
  (samplerate size_t))

(defcstruct (field-info :class field-info :conc-name field-info-)
  (field size_t)
  (description :string)
  (flags :int))

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

(defcstruct (mixer :class mixer :conc-name mixer-)
  (segments :pointer)
  (count size_t)
  (size size_t))

(defcfun (make-buffer "mixed_make_buffer") :int
  (size size_t)
  (buffer :pointer))

(defcfun (free-buffer "mixed_free_buffer") :void
  (buffer :pointer))

(defcfun (copy-buffer "mixed_buffer_copy") :int
  (from :pointer)
  (to :pointer))

(defcfun (resample-nearest "mixed_resample_nearest") :int
  (in :pointer)
  (in-samplerate size_t)
  (out :pointer)
  (out-samplerate size_t)
  (out-samples size_t))

(defcfun (resample-linear "mixed_resample_linear") :int
  (in :pointer)
  (in-samplerate size_t)
  (out :pointer)
  (out-samplerate size_t)
  (out-samples size_t))

(defcfun (resample-cubic "mixed_resample_cubic") :int
  (in :pointer)
  (in-samplerate size_t)
  (out :pointer)
  (out-samplerate size_t)
  (out-samples size_t))

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

(defcfun (segment-info "mixed_segment_info") :pointer
  (segment :pointer))

(defcfun (segment-set "mixed_segment_set") :int
  (field field)
  (value :pointer)
  (segment :pointer))

(defcfun (segment-get "mixed_segment_get") :int
  (field field)
  (value :pointer)
  (segment :pointer))

(defcfun (make-segment-source "mixed_make_segment_source") :int
  (channel :pointer)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-drain "mixed_make_segment_drain") :int
  (channel :pointer)
  (samplerate size_t)
  (segment :pointer))

(defcfun (make-segment-mixer "mixed_make_segment_mixer") :int
  (buffers :pointer)
  (segment :pointer))

(defcfun (make-segment-general "mixed_make_segment_general") :int
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

(defcfun (make-segment-space "mixed_make_segment_space") :int
  (samplerate size_t)
  (segment :pointer))

(defcfun (free-mixer "mixed_free_mixer") :void
  (segment :pointer))

(defcfun (mixer-add "mixed_mixer_add") :int
  (segment :pointer)
  (mixer :pointer))

(defcfun (mixer-remove "mixed_mixer_remove") :int
  (segment :pointer)
  (mixer :pointer))

(defcfun (mixer-start "mixed_mixer_start") :int
  (mixer :pointer))

(defcfun (mixer-mix "mixed_mixer_mix") :int
  (samples size_t)
  (mixer :pointer))

(defcfun (mixer-end "mixed_mixer_end") :int
  (mixer :pointer))

(defcfun (samplesize "mixed_samplesize") :uint8
  (encoding encoding))

(defcfun (error "mixed_error") error)

(defcfun (error-string "mixed_error_string") :string
  (error error))
