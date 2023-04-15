#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.aaudio.cffi
  (:use #:cl)
  (:export
   #:libaaudio
   #:direction
   #:format
   #:result
   #:stream-state
   #:sharing-mode
   #:performance-mode
   #:usage
   #:content-type
   #:spatialization-behavior
   #:input-preset
   #:capture-policy
   #:session-id
   #:channel-mask
   #:callback-result
   #:result-string
   #:state-string
   #:create-stream-builder
   #:stream-builder-set-device-id
   #:stream-builder-set-package-name
   #:stream-builder-set-attribution-tag
   #:stream-builder-set-sample-rate
   #:stream-builder-set-channel-count
   #:stream-builder-set-samples-per-frame
   #:stream-builder-set-format
   #:stream-builder-set-sharing-mode
   #:stream-builder-set-direction
   #:stream-builder-set-buffer-capacity
   #:stream-builder-set-performance-mode
   #:stream-builder-set-usage
   #:stream-builder-set-content-type
   #:stream-builder-set-spatialization-behavior
   #:stream-builder-set-content-spatialized
   #:stream-builder-set-input-preset
   #:stream-builder-set-capture-policy
   #:stream-builder-set-session-id
   #:stream-builder-set-privacy-sensitive
   #:stream-builder-set-data-callback
   #:stream-builder-set-frames-per-data-callback
   #:stream-builder-set-error-callback
   #:stream-builder-open-stream
   #:stream-builder-delete
   #:stream-builder-set-channel-mask
   #:stream-release
   #:stream-close
   #:stream-request-start
   #:stream-request-pause
   #:stream-request-flush
   #:stream-request-stop
   #:stream-get-state
   #:stream-wait-for-state-change
   #:stream-read
   #:stream-write
   #:stream-set-buffer-size
   #:stream-get-buffer-size
   #:stream-get-frames-per-burst
   #:stream-get-buffer-capacity
   #:stream-get-frames-per-data-callback
   #:stream-get-xrun-count
   #:stream-get-sample-rate
   #:stream-get-channel-count
   #:stream-get-samples-per-frame
   #:stream-get-device-id
   #:stream-get-format
   #:stream-get-sharing-mode
   #:stream-get-performance-mode
   #:stream-get-direction
   #:stream-get-frames-written
   #:stream-get-frames-read
   #:stream-get-session-id
   #:stream-get-timestamp
   #:stream-get-usage
   #:stream-get-content-type
   #:stream-get-spatialization-behavior
   #:stream-get-content-spatialized
   #:stream-get-input-preset
   #:stream-get-capture-policy
   #:stream-get-privacy-sensitive
   #:stream-get-channel-mask))
(in-package #:org.shirakumo.fraf.mixed.aaudio.cffi)

(cffi:define-foreign-library libaaudio
  (:unix (:or "libaaudio.so"))
  (T (:or (:default "libaaudio") (:default "aaudio"))))

(cffi:defcenum (direction :int32)
  (:output 0)
  (:input 1))

(cffi:defcenum (format :int32)
  (:invalid -1)
  (:unspecified 0)
  (:int16)
  (:float)
  (:int24)
  (:int32))

(cffi:defcenum (result :int32 :allow-other-keys T)
  (:ok 0)
  (:error-base -900)
  (:disconnected -899)
  (:illegal-argument -898)
  (:internal -896)
  (:invalid-state -895)
  (:invalid-handle -892)
  (:unimplemented -890)
  (:unavailable)
  (:no-free-handles)
  (:no-memory)
  (:null)
  (:timeout)
  (:would-block)
  (:invalid-format)
  (:out-of-range)
  (:no-service)
  (:invalid-rate))

(cffi:defcenum (stream-state :int32)
  (:uninitialized 0)
  (:unknown)
  (:open)
  (:starting)
  (:started)
  (:pausing)
  (:paused)
  (:flushing)
  (:flushed)
  (:stopping)
  (:stopped)
  (:closing)
  (:closed)
  (:disconnected))

(cffi:defcenum (sharing-mode :int32)
  (:exclusive)
  (:shared))

(cffi:defcenum (performance-mode :int32)
  (:none 10)
  (:power-saving)
  (:low-latency))

(cffi:defcenum (usage :int32)
  (:media 1)
  (:voice-communication 2)
  (:voice-communication-signalling 3)
  (:usage-album 4)
  (:usage-notification 5)
  (:usage-notification-ringtone 6)
  (:usage-notification-event 10)
  (:usage-assistance-accessibility 11)
  (:usage-assistance-navigation-guidance 12)
  (:usage-assistance-sonification 13)
  (:usage-game 14)
  (:usage-assistant 16)
  (:system-usage-emergency 1000)
  (:system-usage-safety 1001)
  (:system-usage-vehicle-status 1002)
  (:system-usage-announcement 1003))

(cffi:defcenum (content-type :int32)
  (:speech 1)
  (:music 2)
  (:movie 3)
  (:sonification 4))

(cffi:defcenum (spatialization-behavior :int32)
  (:auto 1)
  (:never 2))

(cffi:defcenum (input-preset :int32)
  (:generic 1)
  (:camcorder 5)
  (:voice-recognition 6)
  (:voice-communication 7)
  (:unprocessed 9)
  (:voice-performance 10))

(cffi:defcenum (capture-policy :int32)
  (:allow-by-all 1)
  (:allow-by-system 2)
  (:deny-all 3))

(cffi:defcenum (session-id :int32 :allow-other-keys T)
  (:none -1)
  (:allocate 0))

(cffi:defbitfield (channel-mask :int32)
  (:invalid -1)
  (:left-front          #b0000000000000000000000000000001)
  (:right-front         #b0000000000000000000000000000010)
  (:center-front        #b0000000000000000000000000000100)
  (:subwoofer           #b0000000000000000000000000001000)
  (:left-rear           #b0000000000000000000000000010000)
  (:right-rear          #b0000000000000000000000000100000)
  (:left-front-center   #b0000000000000000000000001000000)
  (:right-front-center  #b0000000000000000000000010000000)
  (:center-rear         #b0000000000000000000000100000000)
  (:left-side           #b0000000000000000000001000000000)
  (:right-side          #b0000000000000000000010000000000)
  (:center-front-top    #b0000000000000000000100000000000)
  (:left-front-top      #b0000000000000000001000000000000)
  (:right-front-top     #b0000000000000000010000000000000)
  (:left-rear-top       #b0000000000000000100000000000000)
  (:center-rear-top     #b0000000000000001000000000000000)
  (:right-rear-top      #b0000000000000010000000000000000)
  (:left-side-top       #b0000000000000100000000000000000)
  (:right-side-top      #b0000000000001000000000000000000)
  (:left-front-bottom   #b0000000000010000000000000000000)
  (:center-front-bottom #b0000000000100000000000000000000)
  (:right-front-bottom  #b0000000001000000000000000000000)
  (:subwoofer-2         #b0000000010000000000000000000000)
  (:left-front-wide     #b0000000100000000000000000000000)
  (:right-front-wide    #b0000001000000000000000000000000))

(cffi:defcenum (callback-result :int32)
  (:continue 0)
  (:stop))

(cffi:defcfun (result-string "AAudio_convertResultToText") :string
  (result result))

(cffi:defcfun (state-string "AAudio_convertStreamStateToText") :string
  (state stream-state))

(cffi:defcfun (create-stream-builder "AAudio_createStreamBuilder") result
  (builder :pointer))

(cffi:defcfun (stream-builder-set-device-id "AAudioStreamBuilder_setDeviceId") :void
  (builder :pointer)
  (device-id :int32))

(cffi:defcfun (stream-builder-set-package-name "AAudioStreamBuilder_setPackageName") :void
  (builder :pointer)
  (package-name :string))

(cffi:defcfun (stream-builder-set-attribution-tag "AAudioStreamBuilder_setAttributionTag") :void
  (builder :pointer)
  (attribution-tag :string))

(cffi:defcfun (stream-builder-set-sample-rate "AAudioStreamBuilder_setSampleRate") :void
  (builder :pointer)
  (sample-rate :int32))

(cffi:defcfun (stream-builder-set-channel-count "AAudioStreamBuilder_setChannelCount") :void
  (builder :pointer)
  (channel-count :int32))

(cffi:defcfun (stream-builder-set-samples-per-frame "AAudioStreamBuilder_setSamplesPerFrame") :void
  (builder :pointer)
  (samples-per-frame :int32))

(cffi:defcfun (stream-builder-set-format "AAudioStreamBuilder_setFormat") :void
  (builder :pointer)
  (format format))

(cffi:defcfun (stream-builder-set-sharing-mode "AAudioStreamBuilder_setSharingMode") :void
  (builder :pointer)
  (sharing-mode sharing-mode))

(cffi:defcfun (stream-builder-set-direction "AAudioStreamBuilder_setDirection") :void
  (builder :pointer)
  (direction direction))

(cffi:defcfun (stream-builder-set-buffer-capacity "AAudioStreamBuilder_setBufferCapacityInFrames") :void
  (builder :pointer)
  (frames :int32))

(cffi:defcfun (stream-builder-set-performance-mode "AAudioStreamBuilder_setPerformanceMode") :void
  (builder :pointer)
  (performance-mode performance-mode))

(cffi:defcfun (stream-builder-set-usage "AAudioStreamBuilder_setUsage") :void
  (builder :pointer)
  (usage usage))

(cffi:defcfun (stream-builder-set-content-type "AAudioStreamBuilder_setContentType") :void
  (builder :pointer)
  (content-type content-type))

(cffi:defcfun (stream-builder-set-spatialization-behavior "AAudioStreamBuilder_setSpatializationBehavior") :void
  (builder :pointer)
  (behavior spatialization-behavior))

(cffi:defcfun (stream-builder-set-content-spatialized "AAudioStreamBuilder_setIsContentSpatialized") :void
  (builder :pointer)
  (is-spatialized :bool))

(cffi:defcfun (stream-builder-set-input-preset "AAudioStreamBuilder_setInputPreset") :void
  (builder :pointer)
  (input-preset input-preset))

(cffi:defcfun (stream-builder-set-capture-policy "AAudioStreamBuilder_setAllowedCapturePolicy") :void
  (builder :pointer)
  (capture-policy capture-policy))

(cffi:defcfun (stream-builder-set-session-id "AAudioStreamBuilder_setSessionId") :void
  (builder :pointer)
  (session-id session-id))

(cffi:defcfun (stream-builder-set-privacy-sensitive "AAudioStreamBuilder_setPrivacySensitive") :void
  (builder :pointer)
  (privacy-sensitive :bool))

(cffi:defcfun (stream-builder-set-data-callback "AAudioStreamBuilder_setFormat") :void
  (builder :pointer)
  (callback :pointer)
  (user-data :pointer))

(cffi:defcfun (stream-builder-set-frames-per-data-callback "AAudioStreamBuilder_setFramesPerDataCallback") :void
  (builder :pointer)
  (frames :int32))

(cffi:defcfun (stream-builder-set-error-callback "AAudioStreamBuilder_setErrorCallback") :void
  (builder :pointer)
  (callback :pointer)
  (user-data :pointer))

(cffi:defcfun (stream-builder-open-stream "AAudioStreamBuilder_openStream") result
  (builder :pointer)
  (stream :pointer))

(cffi:defcfun (stream-builder-delete "AAudioStreamBuilder_delete") result
  (builder :pointer))

(cffi:defcfun (stream-builder-set-channel-mask "AAudioStreamBuilder_setChannelMask") :void
  (builder :pointer)
  (mask channel-mask))

(cffi:defcfun (stream-release "AAudioStream_release") result
  (stream :pointer))

(cffi:defcfun (stream-close "AAudioStream_close") result
  (stream :pointer))

(cffi:defcfun (stream-request-start "AAudioStream_requestStart") result
  (stream :pointer))

(cffi:defcfun (stream-request-pause "AAudioStream_requestPause") result
  (stream :pointer))

(cffi:defcfun (stream-request-flush "AAudioStream_requestFlush") result
  (stream :pointer))

(cffi:defcfun (stream-request-stop "AAudioStream_requestStop") result
  (stream :pointer))

(cffi:defcfun (stream-get-state "AAudioStream_getState") stream-state
  (stream :pointer))

(cffi:defcfun (stream-wait-for-state-change "AAudioStream_waitForStateChange") :void
  (stream :pointer)
  (input stream-state)
  (next-state :pointer)
  (timeout :int64))

(cffi:defcfun (stream-read "AAudioStream_read") result
  (stream :pointer)
  (buffer :pointer)
  (frames :int32)
  (timeout :int64))

(cffi:defcfun (stream-write "AAudioStream_write") :void
  (stream :pointer)
  (buffer :pointer)
  (frames :int32)
  (timeout :int64))

(cffi:defcfun (stream-set-buffer-size "AAudioStream_setBufferSizeInFrames") result
  (stream :pointer)
  (num-frames :int32))

(cffi:defcfun (stream-get-buffer-size "AAudioStream_getBufferSizeInFrames") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-frames-per-burst "AAudioStream_getFramesPerBurst") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-buffer-capacity "AAudioStream_getBufferCapacityInFrames") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-frames-per-data-callback "AAudioStream_getFramesPerDataCallback") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-xrun-count "AAudioStream_getXRunCount") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-sample-rate "AAudioStream_getSampleRate") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-channel-count "AAudioStream_getChannelCount") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-samples-per-frame "AAudioStream_getSamplesPerFrame") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-device-id "AAudioStream_getDeviceId") :int32
  (stream :pointer))

(cffi:defcfun (stream-get-format "AAudioStream_getFormat") format
  (stream :pointer))

(cffi:defcfun (stream-get-sharing-mode "AAudioStream_getSharingMode") sharing-mode
  (stream :pointer))

(cffi:defcfun (stream-get-performance-mode "AAudioStream_getPerformanceMode") performance-mode
  (stream :pointer))

(cffi:defcfun (stream-get-direction "AAudioStream_getDirection") direction
  (stream :pointer))

(cffi:defcfun (stream-get-frames-written "AAudioStream_getFramesWritten") :int64
  (stream :pointer))

(cffi:defcfun (stream-get-frames-read "AAudioStream_getFramesRead") :int64
  (stream :pointer))

(cffi:defcfun (stream-get-session-id "AAudioStream_getSessionId") session-id
  (stream :pointer))

(cffi:defcfun (stream-get-timestamp "AAudioStream_getTimestamp") result
  (stream :pointer)
  (clockid :size)
  (frame-position :pointer)
  (time :pointer))

(cffi:defcfun (stream-get-usage "AAudioStream_getUsage") usage
  (stream :pointer))

(cffi:defcfun (stream-get-content-type "AAudioStream_getContentType") content-type
  (stream :pointer))

(cffi:defcfun (stream-get-spatialization-behavior "AAudioStream_getSpatializationBehavior") spatialization-behavior
  (stream :pointer))

(cffi:defcfun (stream-get-content-spatialized "AAudioStream_isContentSpatialized") :bool
  (stream :pointer))

(cffi:defcfun (stream-get-input-preset "AAudioStream_getInputPreset") input-preset
  (stream :pointer))

(cffi:defcfun (stream-get-capture-policy "AAudioStream_getAllowedCapturePolicy") capture-policy
  (stream :pointer))

(cffi:defcfun (stream-get-privacy-sensitive "AAudioStream_getPrivacySensitive") :bool
  (stream :pointer))

(cffi:defcfun (stream-get-channel-mask "AAudioStream_getChannelMask") channel-mask
  (stream :pointer))
