#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.jack.cffi
  (:use #:cl)
  (:export
   #:libjack
   #:DEFAULT-AUDIO-TYPE
   #:status
   #:open-options
   #:port-flags
   #:open-client
   #:close-client
   #:activate-client
   #:deactivate-client
   #:register-port
   #:port-buffer
   #:set-process-callback
   #:set-sample-rate-callback
   #:set-shutdown-callback))
(in-package #:org.shirakumo.fraf.mixed.jack.cffi)

(define-foreign-library libjack
    (:windows "jack.dll")
  (:unix "libjack.so")
  (:darwin "libjack.dylib")
  (T (:or (:default "libjack") (:default "jack"))))

(defvar DEFAULT-AUDIO-TYPE "32 bit float mono audio")

(cffi:defbitfield status
  (:failure #x01)
  (:invalid-option #x02)
  (:name-not-unique #x04)
  (:server-started #x08)
  (:server-failed #x10)
  (:server-error #x20)
  (:no-such-client #x40)
  (:load-failure #x80)
  (:init-failure #x100)
  (:shm-failure #x200)
  (:version-error #x400)
  (:backend-error #x800)
  (:client-zombie #x1000))

(cffi:defbitfield open-options
  (:null            #x00)
  (:no-start-server #x01)
  (:use-exact-name  #x02)
  (:server-name     #x04)
  (:load-name       #x08)
  (:load-init       #x10)
  (:session-id      #x20))

(cffi:defbitfield port-flags
  (:input       #x01)
  (:output      #x02)
  (:physical    #x04)
  (:can-monitor #x08)
  (:is-terminal #x10))

(cffi:defcfun (open-client "jack_client_open") :pointer
  (name :string)
  (options open-options)
  (status :pointer)
  &rest)

(cffi:defcfun (close-client "jack_client_close") :int
  (client :pointer))

(cffi:defcfun (activate-client "jack_activate") :int
  (client :pointer))

(cffi:defcfun (deactivate-client "jack_deactivate") :int
  (client :pointer))

(cffi:defcfun (register-port "jack_port_register") :pointer
  (client :pointer)
  (name :string)
  (type :string)
  (flags port-flags)
  (size :ulong))

(cffi:defcfun (unregister-port "jack_port_unregister") :int
  (client :pointer)
  (port :pointer))

(cffi:defcfun (port-buffer "jack_port_get_buffer") :pointer
  (port :pointer)
  (frames :uint32))

(cffi:defcfun (set-process-callback "jack_set_process_callback") :int
  (client :pointer)
  (callback :pointer)
  (arg :pointer))

(cffi:defcfun (set-sample-rate-callback "jack_set_sample_rate_callback") :int
  (client :pointer)
  (callback :pointer)
  (arg :pointer))

(cffi:defcfun (set-shutdown-callback "jack_on_shutdown") :int
  (client :pointer)
  (callback :pointer)
  (arg :pointer))
