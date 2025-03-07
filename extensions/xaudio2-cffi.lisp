(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.xaudio2.cffi
  (:use #:cl)
  (:local-nicknames
   (#:com #:org.shirakumo.com-on)
   (#:com-cffi #:org.shirakumo.com-on.cffi))
  (:export
   #:xaudio2.9
   #:xaudio2.7
   #:CLSID-XAUDIO2
   #:IID-IXAUDIO2
   #:KSDATAFORMAT-SUBTYPE-PCM
   #:KSDATAFORMAT-SUBTYPE-IEEE-FLOAT
   #:WAVE-FORMAT-EXTENSIBLE
   #:hardware-thread
   #:processor
   #:device-role
   #:debug-mask
   #:filter-type
   #:stream-category
   #:channel-configuration
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
   #:device-details
   #:device-details-device-id
   #:device-details-display-name
   #:device-details-role
   #:device-details-output-format
   #:voice-details
   #:voice-details-creation-flags
   #:voice-details-input-channels
   #:voice-details-input-samplerate
   #:send-descriptor
   #:send-descriptor-flags
   #:send-descriptor-output-voice
   #:voice-sends
   #:voice-sends-send-count
   #:voice-sends-descriptors
   #:effect-descriptor
   #:effect-descriptor-effect
   #:effect-descriptor-initial-state
   #:effect-descriptor-output-channels
   #:effect-chain
   #:effect-chain-effect-count
   #:effect-chain-descriptors
   #:filter-parameters
   #:filter-parameters-type
   #:filter-parameters-frequency
   #:filter-parameters-1/q
   #:buffer
   #:buffer-flags
   #:buffer-size
   #:buffer-audio-data
   #:buffer-play-start
   #:buffer-play-length
   #:buffer-loop-start
   #:buffer-loop-length
   #:buffer-loop-count
   #:buffer-context
   #:buffer-wma
   #:buffer-wma-cumulative-bytes
   #:buffer-wma-packet-count
   #:voice-state
   #:voice-state-context
   #:voice-state-buffers-queued
   #:voice-state-samples-played
   #:performance-data
   #:performance-data-audio-cycles-since-last-query
   #:performance-data-total-cycles-since-last-query
   #:performance-data-minimum-cycles-per-quantum
   #:performance-data-maximum-cycles-per-quantum
   #:performance-data-memory-usage-in-bytes
   #:performance-data-current-latency-in-samples
   #:performance-data-glitches-since-engine-started
   #:performance-data-active-source-voice-count
   #:performance-data-total-source-voice-count
   #:performance-data-active-submix-voice-count
   #:performance-data-active-resampler-count
   #:performance-data-active-matrix-mix-count
   #:performance-data-active-xma-source-voices
   #:performance-data-active-xma-streams
   #:debug-configuration
   #:debug-configuration-trace-mask
   #:debug-configuration-break-mask
   #:debug-configuration-log-thread-id
   #:debug-configuration-log-file-line
   #:debug-configuration-log-function-name
   #:debug-configuration-log-timing
   #:create
   #:2.9-interface
   #:2.9-interface-register-for-callbacks
   #:2.9-interface-unregister-for-callbacks
   #:2.9-interface-create-source-voice
   #:2.9-interface-create-submix-voice
   #:2.9-interface-create-mastering-voice
   #:2.9-interface-start-engine
   #:2.9-interface-stop-engine
   #:2.9-interface-commit-changes
   #:2.9-interface-get-performance-data
   #:2.9-interface-set-debug-configuration
   #:2.7-interface
   #:2.7-interface-get-device-count
   #:2.7-interface-get-device-details
   #:2.7-interface-initialize
   #:2.7-interface-register-for-callbacks
   #:2.7-interface-unregister-for-callbacks
   #:2.7-interface-create-source-voice
   #:2.7-interface-create-submix-voice
   #:2.7-interface-create-mastering-voice
   #:2.7-interface-start-engine
   #:2.7-interface-stop-engine
   #:2.7-interface-commit-changes
   #:2.7-interface-get-performance-data
   #:2.7-interface-set-debug-configuration
   #:voice
   #:voice-get-voice-details
   #:voice-set-output-voices
   #:voice-set-effect-chain
   #:voice-enable-effect
   #:voice-disable-effect
   #:voice-get-effect-state
   #:voice-set-effect-parameters
   #:voice-get-effect-parameters
   #:voice-set-filter-parameters
   #:voice-get-filter-parameters
   #:voice-set-output-filter-parameters
   #:voice-get-output-filter-parameters
   #:voice-set-volume
   #:voice-get-volume
   #:voice-set-channel-volumes
   #:voice-get-channel-volumes
   #:voice-set-output-matrix
   #:voice-get-output-matrix
   #:voice-destroy
   #:voice-start
   #:voice-stop
   #:voice-submit-source-buffer
   #:voice-flush-source-buffers
   #:voice-discontinuit
   #:voice-exit-loop
   #:voice-get-state
   #:voice-set-frequency-ratio
   #:voice-get-frequency-ratio
   #:voice-set-source-samplerate
   #:voice-callback
   #:voice-callback-processing-pass-start
   #:voice-callback-processing-pass-end
   #:voice-callback-stream-end
   #:voice-callback-buffer-start
   #:voice-callback-buffer-end
   #:voice-callback-loop-end
   #:voice-callback-error
   #:encode-wave-format
   #:decode-wave-format))
(in-package #:org.shirakumo.fraf.mixed.xaudio2.cffi)

(cffi:define-foreign-library xaudio2.9
  (:windows (:or "xaudio2_9redist.dll" "xaudio2_9.dll" "xaudio2_8.dll" "Windows.Media.Audio.dll")))

(cffi:define-foreign-library xaudio2.7
  (:windows (:or "xaudio2_7.dll" "xaudio2.dll")))

(set 'cl-user::*foreign-system-libraries*
     (union (when (boundp 'cl-user::*foreign-system-libraries*)
              (symbol-value 'cl-user::*foreign-system-libraries*))
            '(xaudio2.7 xaudio2.9)))

(defvar CLSID-XAUDIO2 ;; XAudio 2.7
  (com:guid #x5a508685 #xa254 #x4fba #x9b #x82 #x9a #x24 #xb0 #x03 #x06 #xaf))
(defvar IID-IXAUDIO2
  (com:guid #x8bcf1f58 #x9fe7 #x4583 #x8a #xc6 #xe2 #xad #xc4 #x65 #xc8 #xbb))
(defvar KSDATAFORMAT-SUBTYPE-PCM
  (com:guid #x00000001 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))
(defvar KSDATAFORMAT-SUBTYPE-IEEE-FLOAT
  (com:guid #x00000003 #x0000 #x0010 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71))
(defconstant WAVE-FORMAT-PCM        #x00000001)
(defconstant WAVE-FORMAT-IEEE-FLOAT #x00000003)
(defconstant WAVE-FORMAT-EXTENSIBLE #x0000FFFE)

(cffi:defctype word :uint16)
(cffi:defctype dword :uint32)
(cffi:defctype wchar :uint16)

(cffi:defbitfield hardware-thread
  (:0 #x1)
  (:1 #x2)
  (:2 #x4)
  (:3 #x8)
  (:4 #x10)
  (:5 #x20)
  (:any #x10)
  (:default #x10))

(cffi:defbitfield processor
  (:1 #x00000001)
  (:2 #x00000002)
  (:3 #x00000004)
  (:4 #x00000008)
  (:5 #x00000010)
  (:6 #x00000020)
  (:7 #x00000040)
  (:8 #x00000080)
  (:9 #x00000100)
  (:10 #x00000200)
  (:11 #x00000400)
  (:12 #x00000800)
  (:13 #x00001000)
  (:14 #x00002000)
  (:15 #x00004000)
  (:16 #x00008000)
  (:17 #x00010000)
  (:18 #x00020000)
  (:19 #x00040000)
  (:20 #x00080000)
  (:21 #x00100000)
  (:22 #x00200000)
  (:23 #x00400000)
  (:24 #x00800000)
  (:25 #x01000000)
  (:26 #x02000000)
  (:27 #x04000000)
  (:28 #x08000000)
  (:29 #x10000000)
  (:30 #x20000000)
  (:31 #x40000000)
  (:32 #x80000000)
  (:any #xFFFFFFFF)
  (:default #xFFFFFFFF))

(cffi:defbitfield device-role
  (:not-default #x0)
  (:default-console-device #x1)
  (:default-multimedia-device #x2)
  (:default-communications-device #x4)
  (:default-game-device #x8)
  (:global-default-device #xF))

(cffi:defbitfield debug-mask
  (:errors #x1)
  (:warnings #x2)
  (:info #x4)
  (:defail #x8)
  (:api-calls #x10)
  (:function-calls #x20)
  (:timing #x40)
  (:locks #x80)
  (:memory #x100)
  (:streaming #x1000))

(cffi:defcenum filter-type
  :low-pass
  :band-pass
  :high-pass
  :notch)

(cffi:defcenum stream-category
  :other
  :foreground-only-media
  :background-capable-media
  :communications
  :alerts
  :sound-effects
  :game-effects
  :game-media
  :game-chat
  :speech
  :movie
  :media)

(cffi:defbitfield channel-mask
  (:left-front             #x1)
  (:right-front            #x2)
  (:center-front           #x4)
  (:low-frequency          #x8)
  (:left-rear              #x10)
  (:right-rear             #x20)
  (:left-front-center      #x40)
  (:right-front-center     #x80)
  (:center-rear            #x100)
  (:left-side              #x200)
  (:right-side             #x400)
  (:top-center             #x800)
  (:left-front-top         #x1000)
  (:center-front-top       #x2000)
  (:right-front-top        #x4000)
  (:left-rear-top          #x8000)
  (:center-rear-top        #x10000)
  (:right-rear-top         #x20000)
  (:reserved               #x80000000))

(com:define-hresult
  (:invalid-call #x88960001)
  (:xma-decoder-error #x88960002)
  (:xapo-creation-failed #x88960003)
  (:device-ivalidated #x88960004))

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
  (channel-mask dword)
  (sub-format com:guid))

(cffi:defcstruct (device-details :conc-name device-details-)
  (device-id wchar :count 256)
  (display-name wchar :count 256)
  (role device-role)
  (output-format (:struct waveformat-extensible)))

(cffi:defcstruct (voice-details :conc-name voice-details-)
  (creation-flags :uint32)
  (active-flags :uint32)
  (input-channels :uint32)
  (input-samplerate :uint32))

(cffi:defcstruct (send-descriptor :conc-name send-descriptor-)
  (flags :uint32)
  (output-voice :pointer))

(cffi:defcstruct (voice-sends :conc-name voice-sends-)
  (send-count :uint32)
  (descriptors :pointer))

(cffi:defcstruct (effect-descriptor :conc-name effect-descriptor-)
  (effect :pointer)
  (initial-state :bool)
  (output-channels :uint32))

(cffi:defcstruct (effect-chain :conc-name effect-chain-)
  (effect-count :uint32)
  (descriptors :pointer))

(cffi:defcstruct (filter-parameters :conc-name filter-parameters-)
  (type filter-type)
  (frequency :float)
  (1/q :float))

(cffi:defcstruct (buffer :conc-name buffer-)
  (flags :uint32)
  (size :uint32)
  (audio-data :pointer)
  (play-start :uint32)
  (play-length :uint32)
  (loop-start :uint32)
  (loop-length :uint32)
  (loop-count :uint32)
  (context :uint64))

(cffi:defcstruct (buffer-wma :conc-name buffer-wma-)
  (cumulative-bytes :pointer)
  (packet-count :uint32))

(cffi:defcstruct (voice-state :conc-name voice-state-)
  (context :pointer)
  (buffers-queued :uint32)
  (samples-played :uint64))

(cffi:defcstruct (performance-data :conc-name performance-data-)
  (audio-cycles-since-last-query :uint64)
  (total-cycles-since-last-query :uint64)
  (minimum-cycles-per-quantum :uint32)
  (maximum-cycles-per-quantum :uint32)
  (memory-usage-in-bytes :uint32)
  (current-latency-in-samples :uint32)
  (glitches-since-engine-started :uint32)
  (active-source-voice-count :uint32)
  (total-source-voice-count :uint32)
  (active-submix-voice-count :uint32)
  (active-resampler-count :uint32)
  (active-matrix-mix-count :uint32)
  (active-xma-source-voices :uint32)
  (active-xma-streams :uint32))

(cffi:defcstruct (debug-configuration :conc-name debug-configuration-)
  (trace-mask debug-mask)
  (break-mask debug-mask)
  (log-thread-id :bool)
  (log-file-line :bool)
  (log-function-name :bool)
  (log-timing :bool))

(cffi:defcfun (create "XAudio2Create") com:hresult
  (interface :pointer)
  (flags :uint32)
  (processor processor))

(com:define-comstruct 2.7-interface
  (get-device-count (count :pointer))
  (get-device-details (device :uint32) (details :pointer))
  (initialize (flags :uint32) (processor processor))
  (register-for-callbacks (callback :pointer))
  (unregister-for-callbacks (callback :pointer))
  (create-source-voice (voice :pointer) (format :pointer) (flags :uint32) (max-frequency-ratio :float) (callback :pointer) (send-list :pointer) (effect-chain :pointer))
  (create-submix-voice (voice :pointer) (input-channels :uint32) (input-samplerate :uint32) (flags :uint32) (stage :uint32) (send-list :pointer) (effect-chain :pointer))
  (create-mastering-voice (voice :pointer) (input-channels :uint32) (input-samplerate :uint32) (flags :uint32) (device-id :uint32) (effect-chain :pointer) (stream-category stream-category))
  (start-engine)
  (stop-engine)
  (commit-changes (operation-set :uint32))
  (get-performance-data (data :pointer))
  (set-debug-configuration (configuration :pointer) (reserved :pointer)))

(com:define-comstruct 2.9-interface
  (register-for-callbacks (callback :pointer))
  (unregister-for-callbacks (callback :pointer))
  (create-source-voice (voice :pointer) (format :pointer) (flags :uint32) (max-frequency-ratio :float) (callback :pointer) (send-list :pointer) (effect-chain :pointer))
  (create-submix-voice (voice :pointer) (input-channels :uint32) (input-samplerate :uint32) (flags :uint32) (stage :uint32) (send-list :pointer) (effect-chain :pointer))
  (create-mastering-voice (voice :pointer) (input-channels :uint32) (input-samplerate :uint32) (flags :uint32) (device-id :pointer) (effect-chain :pointer) (stream-category stream-category))
  (start-engine)
  (stop-engine)
  (commit-changes (operation-set :uint32))
  (get-performance-data (data :pointer))
  (set-debug-configuration (configuration :pointer) (reserved :pointer)))

(com:define-comstruct (voice :include NIL)
  (get-voice-details (details :pointer))
  (set-output-voices (send-list :pointer))
  (set-effect-chain (effect-chain :pointer))
  (enable-effect (index :uint32) (operation-set :uint32))
  (disable-effect (index :uint32) (operation-set :uint32))
  (get-effect-state (index :uint32) (enabled :pointer))
  (set-effect-parameters (index :uint32) (parameters :pointer) (byte-size :uint32))
  (get-effect-parameters (index :uint32) (parameters :pointer) (byte-size :uint32))
  (set-filter-parameters (parameters :pointer) (operation-set :uint32))
  (get-filter-parameters (parameters :pointer))
  (set-output-filter-parameters (destination-voice :pointer) (parameters :pointer) (operation-set :uint32))
  (get-output-filter-parameters (destination-voice :pointer) (parameters :pointer))
  (set-volume (volume :float) (operation-set :uint32))
  (get-volume (volume :pointer))
  (set-channel-volumes (channels :uint32) (volumes :pointer) (operation-set :uint32))
  (get-channel-volumes (channels :uint32) (volumes :pointer))
  (set-output-matrix (destination-voice :pointer) (source-channels :uint32) (destination-channels :uint32) (level-matrix :pointer) (operation-set :uint32))
  (get-output-matrix (destination-voice :pointer) (source-channels :uint32) (destination-channels :uint32) (level-matrix :pointer))
  (destroy)
  ;; Exclusive to source-voice
  (start (flags :uint32) (operation-set :uint32))
  (stop (flags :uint32) (operation-set :uint32))
  (submit-source-buffer (buffer :pointer) (buffer-wma :pointer))
  (flush-source-buffers)
  (discontinuity)
  (exit-loop (operation-set :uint32))
  (get-state (state :pointer))
  (set-frequency-ratio (ratio :float) (operation-set :uint32))
  (get-frequency-ratio (ratio :pointer))
  (set-source-samplerate (samplerate :uint32)))

(com:define-comstruct (voice-callback :include NIL)
  (processing-pass-start :void (bytes-required :uint32))
  (processing-pass-end :void)
  (stream-end :void)
  (buffer-start :void (context :pointer))
  (buffer-end :void (context :pointer))
  (loop-end :void (context :pointer))
  (error :void (context :pointer) (error com:hresult)))

(defun channel-mask-for-channel-count (channels)
  (case channels
    (1 (cffi:foreign-bitfield-value 'channel-mask '(:center-front)))
    (2 (cffi:foreign-bitfield-value 'channel-mask '(:left-front :right-front)))
    (3 (cffi:foreign-bitfield-value 'channel-mask '(:left-front :center-front :right-front)))
    (4 (cffi:foreign-bitfield-value 'channel-mask '(:left-front :right-front :left-rear :right-rear)))
    (5 (cffi:foreign-bitfield-value 'channel-mask '(:left-front :right-front :left-rear :right-rear :low-frequency)))
    (6 (cffi:foreign-bitfield-value 'channel-mask '(:left-front :center-front :right-front :left-rear :right-rear :low-frequency)))
    (T (cffi:foreign-bitfield-value 'channel-mask '()))))

(defun encode-wave-format (ptr samplerate channels format)
  (let ((bit-depth (ecase format
                     ((:double :int64) 64)
                     ((:float :int32) 32)
                     ((:int24) 24)
                     ((:int16) 16)
                     ((:uint8) 8)))
        (format (case format
                  ((:double :float) KSDATAFORMAT-SUBTYPE-IEEE-FLOAT)
                  (T KSDATAFORMAT-SUBTYPE-PCM))))
    ;; Clear the data.
    (loop for i from 0 below (cffi:foreign-type-size '(:struct waveformat-extensible))
          do (setf (cffi:mem-ref ptr :uchar i) 0))
    ;; The EX struct is at the beginning, so we can reuse the pointer.
    (setf (waveformat-ex-format-tag ptr) WAVE-FORMAT-EXTENSIBLE)
    (setf (waveformat-ex-size ptr) 22)
    (setf (waveformat-ex-channels ptr) channels)
    (setf (waveformat-ex-samples-per-sec ptr) samplerate)
    (setf (waveformat-ex-bits-per-sample ptr) (if (= 24 bit-depth) 32 bit-depth))
    (setf (waveformat-ex-block-align ptr) (/ (* channels (waveformat-ex-bits-per-sample ptr)) 8))
    (setf (waveformat-ex-avg-bytes-per-sec ptr) (* samplerate (waveformat-ex-block-align ptr)))
    (setf (waveformat-extensible-samples ptr) bit-depth)
    (setf (waveformat-extensible-channel-mask ptr) (channel-mask-for-channel-count channels))
    (setf (waveformat-extensible-sub-format ptr) format))
  ptr)

(defun decode-wave-format (ptr)
  (values (waveformat-ex-samples-per-sec ptr)
          (waveformat-ex-channels ptr) 
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
