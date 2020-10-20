#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.wasapi.cffi
  (:use #:cl)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on))
  (:shadow #:byte #:sleep)
  (:export
   #:avrt
   #:AUDCLNT-STREAMFLAGS-EVENTCALLBACK
   #:DEVICE-STATE-ACTIVE
   #:WAVE-FORMAT-EXTENSIBLE
   #:INFINITE
   #:word
   #:dword
   #:lpunknown
   #:byte
   #:ulong
   #:reference-time
   #:handle
   #:lpdword
   #:wait-result
   #:sharedmode
   #:bufferflags
   #:hresult
   #:dataflow
   #:role
   #:channel-mask
   #:waveformat-ex
   #:waveformat-ex-format-tag
   #:waveformat-ex-channels
   #:waveformat-ex-samples-per-sec
   #:waveformat-ex-avg-bytes-per-sec
   #:waveformat-ex-block-align
   #:waveformat-ex-bits-per-sample
   #:waveformat-ex-size
   #:waveformat-extensible
   #:waveformat-extensible-format
   #:waveformat-extensible-samples
   #:waveformat-extensible-channel-mask
   #:waveformat-extensible-sub-format
   #:property-key
   #:property-key-fmtid
   #:property-key-pid
   #:imm-device-enumerator
   #:imm-device-enumerator-query-interface
   #:imm-device-enumerator-add-ref
   #:imm-device-enumerator-release
   #:imm-device-enumerator-enum-audio-endpoints
   #:imm-device-enumerator-get-default-audio-endpoint
   #:imm-device-enumerator-get-device
   #:imm-device-enumerator-register-endpoint-notification-callback
   #:imm-device-enumerator-unregister-endpoint-notification-callback
   #:imm-device-collection
   #:imm-device-collection-query-interface
   #:imm-device-collection-add-ref
   #:imm-device-collection-release
   #:imm-device-collection-get-count
   #:imm-device-collection-item
   #:imm-device
   #:imm-device-query-interface
   #:imm-device-add-ref
   #:imm-device-release
   #:imm-device-activate
   #:imm-device-open-property-store
   #:imm-device-get-id
   #:imm-device-get-state
   #:i-audio-client
   #:i-audio-client-query-interface
   #:i-audio-client-add-ref
   #:i-audio-client-release
   #:i-audio-client-initialize
   #:i-audio-client-get-buffer-size
   #:i-audio-client-get-stream-latency
   #:i-audio-client-get-current-padding
   #:i-audio-client-is-format-supported
   #:i-audio-client-get-mix-format
   #:i-audio-client-get-device-period
   #:i-audio-client-start
   #:i-audio-client-stop
   #:i-audio-client-reset
   #:i-audio-client-set-event-handle
   #:i-audio-client-get-service
   #:i-audio-render-client
   #:i-audio-render-client-query-interface
   #:i-audio-render-client-add-ref
   #:i-audio-render-client-release
   #:i-audio-render-client-get-buffer
   #:i-audio-render-client-release-buffer
   #:i-property-store
   #:i-property-store-query-interface
   #:i-property-store-add-ref
   #:i-property-store-release
   #:i-property-store-commit
   #:i-property-store-get-at
   #:i-property-store-get-count
   #:i-property-store-get-value
   #:i-property-store-set-value
   #:i-audio-session-control
   #:i-audio-session-control-query-interface
   #:i-audio-session-control-add-ref
   #:i-audio-session-control-release
   #:i-audio-session-control-get-state
   #:i-audio-session-control-get-display-name
   #:i-audio-session-control-set-display-name
   #:i-audio-session-control-get-icon-path
   #:i-audio-session-control-set-icon-path
   #:i-audio-session-control-get-grouping-param
   #:i-audio-session-control-set-grouping-param
   #:i-audio-session-control-register-audio-session-notification
   #:i-audio-session-control-unregister-audio-session-notification
   #:av-set-mm-thread-characteristics
   #:av-revert-mm-thread-characteristics
   #:wait-for-single-object
   #:sleep
   #:create-event
   #:close-handle
   #:set-event
   #:compose-channel-mask
   #:channel-mask-for-channel-count
   #:IID-IAudioClient
   #:IID-IAudioRenderClient
   #:IID-IAudioSessionControl
   #:IID-IMMDeviceEnumerator
   #:CLSID-MMDeviceEnumerator
   #:KSDATAFORMAT-SUBTYPE-PCM
   #:KSDATAFORMAT-SUBTYPE-IEEE-FLOAT
   #:encode-wave-format
   #:decode-wave-format))
(in-package #:org.shirakumo.fraf.mixed.wasapi.cffi)

(cffi:define-foreign-library avrt
  (:windows "Avrt.dll"))

;; https://github.com/EddieRingle/portaudio/blob/master/src/hostapi/wasapi/mingw-include/audioclient.h
;; https://github.com/EddieRingle/portaudio/blob/master/src/hostapi/wasapi/mingw-include/mmdeviceapi.h

(defvar IID-IAudioClient
  (com:guid #x1CB9AD4C #xDBFA #x4c32 #xB1 #x78 #xC2 #xF5 #x68 #xA7 #x03 #xB2))
(defvar IID-IAudioRenderClient
  (com:guid #xF294ACFC #x3146 #x4483 #xA7 #xBF #xAD #xDC #xA7 #xC2 #x60 #xE2))
(defvar IID-IAudioSessionControl
  (com:guid #xf4b1a599 #x7266 #x4319 #xa8 #xca #xe7 #x0a #xcb #x11 #xe8 #xcd))
(defvar IID-IMMDeviceEnumerator
  (com:guid #xA95664D2 #x9614 #x4F35 #xA7 #x46 #xDE #x8D #xB6 #x36 #x17 #xE6))
(defvar CLSID-MMDeviceEnumerator
  (com:guid #xBCDE0395 #xE52F #x467C #x8E #x3D #xC4 #x57 #x92 #x91 #x69 #x2E))
(defvar KSDATAFORMAT-SUBTYPE-PCM
  (com:guid #x00000001 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))
(defvar KSDATAFORMAT-SUBTYPE-IEEE-FLOAT
  (com:guid #x00000003 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))
(defconstant AUDCLNT-STREAMFLAGS-EVENTCALLBACK #x00040000)
(defconstant DEVICE-STATE-ACTIVE #x00000001)
(defconstant WAVE-FORMAT-EXTENSIBLE #x0000FFFE)
(defconstant INFINITE (1- (expt 2 32)))

(cffi:defctype word :uint16)
(cffi:defctype dword :uint32)
(cffi:defctype lpunknown :pointer)
(cffi:defctype byte :uint8)
(cffi:defctype ulong :unsigned-long)
(cffi:defctype wstring :pointer)
(cffi:defctype reference-time :long-long)
(cffi:defctype handle :pointer)
(cffi:defctype lpdword :pointer)

(com:define-hresult
  (:already-initialized #x88890002)
  (:bufduration-period-not-equal #x88890013)
  (:buffer-error #x88890018)
  (:buffer-operation-pending #x8889000B)
  (:buffer-size-error #x88890016)
  (:buffer-size-not-aligned #x88890019)
  (:buffer-too-large #x88890006)
  (:class-not-registered #x80040154)
  (:cpuusage-exceeded #x88890017)
  (:device-in-use #x8889000A)
  (:device-invalidated #x88890004)
  (:endpoint-create-failed #x8889000F)
  (:exclusive-mode-not-allowed #x8889000E)
  (:invalid-device-period #x88890020)
  (:invalid-size #x88890009)
  (:no-aggregation #x80040110)
  (:not-initialized #x88890001)
  (:out-of-order #x88890007)
  (:service-not-running #x88890010)
  (:unsupported-format #x88890008)
  (:wrong-endpoint-type #x88890003)
  (:not-stopped #x88890005))

(cffi:defcenum (wait-result dword)
  (:abandoned #x00000080)
  (:object-0  #x00000000)
  (:timeout   #x00000102)
  (:failed    #xFFFFFFFF))

(cffi:defcenum sharemode
  :shared
  :exclusive)

(cffi:defcenum bufferflags
  (:data-discontinuity 1)
  (:silent 2)
  (:timestamp-error 4))

(cffi:defcenum dataflow
  :render
  :capture
  :all
  :dataflow-enum-count)

(cffi:defcenum role
  :console
  :multimedia
  :communications
  :role-enum-count)

(cffi:defbitfield channel-mask
  (:left-front             #x1)
  (:right-front            #x2)
  (:center                 #x4)
  (:subwoofer              #x8)
  (:left-rear              #x10)
  (:right-rear             #x20)
  (:front-left-of-center   #x40)
  (:front-right-of-center  #x80)
  (:center-rear            #x100)
  (:left-side              #x200)
  (:right-side             #x400)
  (:center-top             #x800)
  (:left-front-top         #x1000)
  (:center-front-top       #x2000)
  (:right-front-top        #x4000)
  (:left-back-top          #x8000)
  (:center-back-top        #x10000)
  (:right-back-top         #x20000)
  (:reserved               #x80000000))

(cffi:defcstruct (waveformat-ex :conc-name waveformat-ex-)
  (format-tag word)
  (channels word)
  (samples-per-sec dword)
  (avg-bytes-per-sec dword)
  (block-align word)
  (bits-per-sample word)
  (size word))

(cffi:defcstruct (waveformat-extensible :conc-name waveformat-extensible-)
  (format (:struct waveformat-ex))
  (samples word :offset 18)
  (channel-mask channel-mask)
  (sub-format com:guid))

(cffi:defcstruct (property-key :conc-name property-key)
  (fmtid com:guid)
  (pid dword))

(cffi:defcstruct (com :conc-name ||)
  (vtbl :pointer))

(com:define-comstruct imm-device-enumerator
  (enum-audio-endpoints (data-flow dataflow) (state-mask dword) (devices :pointer))
  (get-default-audio-endpoint (data-flow dataflow) (role role) (endpoint :pointer))
  (get-device (pwstrid wstring) (device :pointer))
  (register-endpoint-notification-callback (client :pointer))
  (unregister-endpoint-notification-callback (client :pointer)))

(com:define-comstruct imm-device-collection
  (get-count (devices :pointer))
  (item (device-id :uint) (device :pointer)))

(com:define-comstruct imm-device
  (activate (id com:guid) (cls-ctx dword) (activation-params :pointer) (interface :pointer))
  (open-property-store (access dword) (properties :pointer))
  (get-id (str-id :pointer))
  (get-state (state :pointer)))

(com:define-comstruct i-audio-client
  (initialize (share-mode sharemode) (stream-flags dword) (buffer-duration reference-time) (preiodicity reference-time) (format :pointer) (audio-session-guid com:guid))
  (get-buffer-size (num-buffer-frames :pointer))
  (get-stream-latency (latency :pointer))
  (get-current-padding (num-padding-frames :pointer))
  (is-format-supported (share-mode sharemode) (format :pointer) (closest-match :pointer))
  (get-mix-format (device-format :pointer))
  (get-device-period (default-device-period :pointer) (minimum-device-period :pointer))
  (start)
  (stop)
  (reset)
  (set-event-handle (event-handle handle))
  (get-service (riid com:guid) (service :pointer)))

(com:define-comstruct i-audio-render-client
  (get-buffer (num-frames-requested :uint32) (data :pointer))
  
  (release-buffer (num-frames-written :uint32) (flags bufferflags)))

(com:define-comstruct i-property-store
  (commit)
  (get-at (prop dword) (pkey :pointer))
  (get-count (props :pointer))
  (get-value (key :pointer) (value :pointer))
  (set-value (key :pointer) (value :pointer)))

(com:define-comstruct i-audio-session-control
  (get-state (retval :pointer))
  (get-display-name (retval :pointer))
  (set-display-name (value :pointer) (event-context :pointer))
  (get-icon-path (retval :pointer))
  (set-icon-path (value :pointer) (event-context :pointer))
  (get-grouping-param (retval :pointer))
  (set-grouping-param (value :pointer) (event-context :pointer))
  (register-audio-session-notification (new-notifications :pointer))
  (unregister-audio-session-notification (new-notifications :pointer)))

(cffi:defcfun (av-set-mm-thread-characteristics "AvSetMmThreadCharacteristicsW") handle
  (task-name wstring)
  (task-index lpdword))

(cffi:defcfun (av-revert-mm-thread-characteristics "AvRevertMmThreadCharacteristics") :bool
  (handle handle))

(cffi:defcfun (wait-for-single-object "WaitForSingleObject") wait-result
  (handle handle)
  (milliseconds dword))

(cffi:defcfun (sleep "Sleep") :void
  (miliseconds dword))

(cffi:defcfun (create-event "CreateEventW") handle
  (event-attribute :pointer)
  (manual-reset :bool)
  (initial-state :bool)
  (name wstring))

(cffi:defcfun (close-handle "CloseHandle") :bool
  (object handle))

(cffi:defcfun (set-event "SetEvent") :bool
  (event handle))

(defun encode-wave-format (ptr samplerate channels format)
  (let ((bit-depth (ecase format
                     ((:double :int64 :uint64) 64)
                     ((:float :int32 :uint32) 32)
                     ((:int24 :uint24) 24)
                     ((:int16 :uint16) 16)
                     ((:int8 :uint8) 8)))
        (format (case format
                  ((:double :float) KSDATAFORMAT-SUBTYPE-IEEE-FLOAT)
                  (T KSDATAFORMAT-SUBTYPE-PCM))))
    ;; Clear the data.
    (loop for i from 0 below (cffi:foreign-type-size '(:struct waveformat-extensible))
          do (setf (cffi:mem-ref ptr :uchar i) 0))
    ;; The EX struct is at the beginning, so we can reuse the pointer.
    (setf (waveformat-ex-format-tag ptr) WAVE-FORMAT-EXTENSIBLE)
    (setf (waveformat-ex-size ptr) 22)
    (setf (waveformat-ex-channels ptr) (length channels))
    (setf (waveformat-ex-samples-per-sec ptr) samplerate)
    (setf (waveformat-ex-bits-per-sample ptr) (if (= 24 bit-depth) 32 bit-depth))
    (setf (waveformat-ex-block-align ptr) (/ (* (length channels) (waveformat-ex-bits-per-sample ptr)) 8))
    (setf (waveformat-ex-avg-bytes-per-sec ptr) (* samplerate (waveformat-ex-block-align ptr)))
    (setf (waveformat-extensible-samples ptr) bit-depth)
    (setf (waveformat-extensible-channel-mask ptr) channels)
    (setf (waveformat-extensible-sub-format ptr) format))
  ptr)

(defun decode-wave-format (ptr)
  (values (waveformat-ex-samples-per-sec ptr)
          (waveformat-extensible-channel-mask ptr)
          (if (com:guid= (waveformat-extensible-sub-format ptr) KSDATAFORMAT-SUBTYPE-IEEE-FLOAT)
              (ecase (waveformat-extensible-samples ptr)
                (64 :double)
                (32 :float))
              (ecase (waveformat-extensible-samples ptr)
                (64 :int64)
                (32 :int32)
                (24 :int24)
                (16 :int16)
                (8 :uint8)))))
