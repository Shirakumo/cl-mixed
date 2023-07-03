(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.mpt.cffi
  (:use #:cl)
  (:export
   #:libopenmpt
   #:create
   #:destroy
   #:read-mono
   #:read-stereo
   #:read-quad
   #:get-duration
   #:set-position))
(in-package #:org.shirakumo.fraf.mixed.mpt.cffi)

(cffi:define-foreign-library libopenmpt
  (T (:or (:default "libopenmpt") (:default "openmpt"))))

(cffi:defcfun (create "openmpt_module_create_from_memory2") :pointer
  (data :pointer)
  (size :size)
  (log-fun :pointer)
  (log-data :pointer)
  (err-fun :pointer)
  (err-data :pointer)
  (error :pointer)
  (error-message :pointer)
  (control :pointer))

(cffi:defcfun (destroy "openmpt_module_destroy") :void
  (module :pointer))

(cffi:defcfun (read-mono "openmpt_module_read_interleaved_float_mono") :size
  (module :pointer)
  (samplerate :int32)
  (count :size)
  (buffer :pointer))

(cffi:defcfun (read-stereo "openmpt_module_read_interleaved_float_stereo") :size
  (module :pointer)
  (samplerate :int32)
  (count :size)
  (buffer :pointer))

(cffi:defcfun (read-quad "openmpt_module_read_interleaved_float_quad") :size
  (module :pointer)
  (samplerate :int32)
  (count :size)
  (buffer :pointer))

(cffi:defcfun (get-duration "openmpt_module_get_duration_seconds") :double
  (module :pointer))

(cffi:defcfun (set-position "openmpt_module_set_position_seconds") :double
  (module :pointer)
  (seconds :double))
