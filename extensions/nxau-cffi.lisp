(defpackage #:org.shirakumo.fraf.mixed.nxau.cffi
  (:use #:cl)
  (:shadow #:open #:close)
  (:export
   #:libnxau
   #:sample-format
   #:audio-format
   #:audio-format-samplerate
   #:audio-format-channels
   #:audio-format-format
   #:audio-format-buffersize
   #:open
   #:close
   #:start
   #:stop
   #:play))
(in-package #:org.shirakumo.fraf.mixed.nxau.cffi)

(cffi:define-foreign-library libnxau
    (T (:or "libnxau.nro" "libnxau")))

(cffi:defcenum sample-format
  (:invalid 0)
  (:int8 1)
  (:int16 2)
  (:int24 3)
  (:int32 4)
  (:float 5)
  (:adpcm 6))

(cffi:defcstruct (audio-format :conc-name audio-format-)
  (samplerate :int32)
  (channels :uint8)
  (format sample-format)
  (buffersize :size))

(cffi:defcfun (open "nxau_open") :pointer
  (format :pointer))

(cffi:defcfun (close "nxau_close") :void
  (device :pointer))

(cffi:defcfun (start "nxau_start") :bool
  (device :pointer))

(cffi:defcfun (stop "nxau_stop") :bool
  (device :pointer))

(cffi:defcfun (play "nxau_play") :bool
  (buffer :pointer)
  (size :size)
  (timeout :float)
  (device :pointer))
