#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.pulse.cffi
  (:use #:cl)
  (:import-from #:org.shirakumo.fraf.mixed.cffi #:size_t)
  (:export
   #:libpulse
   #:libpulse-simple
   #:sample-format
   #:stream-direction
   #:channel-position
   #:channel-map
   #:channel-map-channels
   #:channel-map-map
   #:sample-spec
   #:sample-spec-format
   #:sample-spec-rate
   #:sample-spec-channels
   #:buffer-attr
   #:buffer-attr-maxlength
   #:buffer-attr-length
   #:buffer-attr-prebuf
   #:buffer-attr-minreq
   #:buffer-attr-fragsize
   #:strerror
   #:channel-map-init
   #:simple-new
   #:simple-write
   #:simple-read
   #:simple-drain
   #:simple-flush
   #:simple-free))
(in-package #:org.shirakumo.fraf.mixed.pulse.cffi)

(cffi:define-foreign-library libpulse
  (t (:or (:default "libpulse") "libpulse.so.0")))

(cffi:define-foreign-library libpulse-simple
  (t (:or (:default "libpulse-simple") "libpulse-simple.so.0")))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant +channels-max+ 32))

(cffi:defcenum sample-format
  :u8
  :alaw
  :ulaw
  :int16
  :int16-be
  :float
  :float-be
  :int32
  :int32-be
  :int24
  :int24-be
  :s24-32le
  :s24-32be
  :max
  :invalid)

(cffi:defcenum stream-direction
  :no-direction
  :playback
  :record
  :upload)

(cffi:defcenum channel-position
  (:invalid -1)
  (:mono 0)
  :left-front
  :right-front
  :center
  :center-rear
  :left-rear
  :right-rear
  :subwoofer
  :left-center
  :right-center
  :left-side
  :right-side
  :aux0
  :aux1
  :aux2
  :aux3
  :aux4
  :aux5
  :aux6
  :aux7
  :aux8
  :aux9
  :aux10
  :aux11
  :aux12
  :aux13
  :aux14
  :aux15
  :aux16
  :aux17
  :aux18
  :aux19
  :aux20
  :aux21
  :aux22
  :aux23
  :aux24
  :aux25
  :aux26
  :aux27
  :aux28
  :aux29
  :aux30
  :aux31
  :center-top
  :left-front-top
  :right-front-top
  :center-front-top
  :left-rear-top
  :right-rear-top
  :center-rear-top
  :max)

(cffi:defcenum context-flags
  (:no-flags)
  (:no-auto-spawn)
  (:no-fail))

(cffi:defcstruct (channel-map :class channel-map :conc-name channel-map-)
  (channels :uint8)
  (map channel-position :count #.+channels-max+))

(cffi:defcstruct (sample-spec :class sample-spec :conc-name sample-spec-)
    (format sample-format)
  (rate :uint32)
  (channels :uint8))

(cffi:defcstruct (buffer-attr :class buffer-attr :conc-name buffer-attr-)
    (maxlength :uint32)
  (length :uint32)
  (prebuf :uint32)
  (minreq :uint32)
  (fragsize :uint32))

(cffi:defcfun (connect "pa_context_connect") :int
  (context :pointer)
  (server :string)
  (flags context-flags)
  (api :pointer))

(cffi:defcfun (channel-map-init "pa_channel_map_init") :pointer
  (map :pointer))

(cffi:defcfun (channel-map-init-stereo "pa_channel_map_init_stereo") :pointer
  (map :pointer))

(cffi:defcfun (strerror "pa_strerror") :string
  (error :int))

(cffi:defcfun (simple-new "pa_simple_new") :pointer
  (server :string)
  (name :string)
  (direction stream-direction)
  (device :string)
  (stream-name :string)
  (sample-spec :pointer)
  (channel-map :pointer)
  (buffer-attributes :pointer)
  (error :pointer))

(cffi:defcfun (simple-write "pa_simple_write") :int
  (simple :pointer)
  (data :pointer)
  (bytes size_t)
  (error :pointer))

(cffi:defcfun (simple-read "pa_simple_read") :int
  (simple :pointer)
  (data :pointer)
  (bytes size_t)
  (error :pointer))

(cffi:defcfun (simple-drain "pa_simple_drain") :int
  (simple :pointer)
  (error :pointer))

(cffi:defcfun (simple-flush "pa_simple_flush") :int
  (simple :pointer)
  (error :pointer))

(cffi:defcfun (simple-free "pa_simple_free") :void
  (simple :pointer))
