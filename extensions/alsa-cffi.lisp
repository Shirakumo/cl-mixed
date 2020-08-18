#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.alsa.cffi
  (:use #:cl)
  (:export
   #:libasound
   #:pcm-stream
   #:pcm-format
   #:pcm-access
   #:pcm-open
   #:pcm-set-params
   #:pcm-hw-params-size
   #:pcm-hw-params-current
   #:pcm-hw-params-get-channels
   #:pcm-hw-params-get-format
   #:pcm-hw-params-get-rate
   #:pcm-writei
   #:pcm-recover
   #:pcm-pause
   #:pcm-drop
   #:pcm-drain
   #:pcm-close
   #:strerror))
(in-package #:org.shirakumo.fraf.mixed.alsa.cffi)

(cffi:define-foreign-library libasound
  (:unix (:or "libasound.so.2.0.0" "libasound.so.2" "libasound.so"))
  (T (:or (:default "libasound") (:default "asound"))))

(cffi:defctype size_t #+x86-64 :uint64 #+x86 :uint32)

(cffi:defcenum pcm-stream
  :playback
  :capture)

(cffi:defcenum pcm-format
  (:unknown -1)
  :int8
  :uint8
  :int16
  :int16-be
  :uint16
  :uint16-be
  :int24
  :int24-be
  :uint24
  :uint24-be
  :int32
  :int32-be
  :uint32
  :uint32-be
  :float
  :float-be
  :double
  :double-be
  :iec958-subframe-le
  :iec958-subframe-be
  :mu-law
  :a-law
  :ima-adpcm
  :mpeg
  :gsm
  :special
  :s24-3le
  :s24-3be
  :u24-3le
  :u24-3be
  :s20-3le
  :s20-3be
  :u20-3le
  :u20-3be
  :s18-3le
  :s18-3be
  :u18-3le
  :u18-3be
  :s16
  :u16
  :s24
  :u24
  :s32
  :u32
  :float32
  :float64
  :iec958-subframe)

(cffi:defcenum pcm-access
  :mmap-interleaved
  :mmap-noninterleaved
  :mmap-complex
  :rw-interleaved
  :rw-noninterleaved)

(cffi:defcfun (pcm-open "snd_pcm_open") :int
  (pcm :pointer)
  (name :string)
  (stream pcm-stream)
  (mode :int))

(cffi:defcfun (pcm-set-params "snd_pcm_set_params") :int
  (pcm :pointer)
  (format pcm-format)
  (access pcm-access)
  (channels :uint)
  (rate :uint)
  (soft-resample :int)
  (latency :uint))

(cffi:defcfun (pcm-hw-params-size "snd_pcm_hw_params_sizeof") size_t)

(cffi:defcfun (pcm-hw-params-current "snd_pcm_hw_params_current") :int
  (pcm :pointer)
  (params :pointer))

(cffi:defcfun (pcm-hw-params-get-format "snd_pcm_hw_params_get_format") :int
  (params :pointer)
  (format :pointer))

(cffi:defcfun (pcm-hw-params-get-channels "snd_pcm_hw_params_get_channels") :int
  (params :pointer)
  (channels :pointer))

(cffi:defcfun (pcm-hw-params-get-rate "snd_pcm_hw_params_get_rate") :int
  (params :pointer)
  (rate :pointer)
  (dir :pointer))

(cffi:defcfun (pcm-writei "snd_pcm_writei") :long
  (pcm :pointer)
  (buffer :pointer)
  (frames :ulong))

(cffi:defcfun (pcm-recover "snd_pcm_recover") :int
  (pcm :pointer)
  (err :int)
  (silent :int))

(cffi:defcfun (pcm-pause "snd_pcm_pause") :int
  (pcm :pointer)
  (enable :int))

(cffi:defcfun (pcm-drop "snd_pcm_drop") :int
  (pcm :pointer))

(cffi:defcfun (pcm-drain "snd_pcm_drain") :int
  (pcm :pointer))

(cffi:defcfun (pcm-close "snd_pcm_close") :int
  (pcm :pointer))

(cffi:defcfun (strerror "snd_strerror") :string
  (error :int))
