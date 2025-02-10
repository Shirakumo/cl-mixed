(defpackage #:org.shirakumo.fraf.mixed.coreaudio.cffi
  (:use #:cl)
  (:export
   #:audio-unit
   #:audio-toolbox
   #:kAudioUnitType_Output
   #:kAudioUnitSubType_DefaultOutput
   #:kAudioUnitManufacturer_Apple
   #:kAudioFormatLinearPCM
   #:no-err
   #:os-type
   #:os-status
   #:audio-component
   #:component-instance
   #:component-result
   #:audio-component-instance
   #:audio-unit
   #:audio-unit-property-id
   #:audio-unit-scope
   #:audio-unit-element
   #:audio-format-id
   #:audio-format-flags
   #:render-action-flags
   #:component-instance-record
   #:component-instance-record-data
   #:audio-component-description
   #:audio-component-description-component-type
   #:audio-component-description-component-sub-type
   #:audio-component-description-component-manufacturer
   #:audio-component-description-component-flags
   #:audio-component-description-component-flags-mask
   #:audio-stream-basic-description
   #:audio-stream-basic-description-sample-rate
   #:audio-stream-basic-description-format-id
   #:audio-stream-basic-description-format-flags
   #:audio-stream-basic-description-bytes-per-packet
   #:audio-stream-basic-description-frames-per-packet
   #:audio-stream-basic-description-bytes-per-frame
   #:audio-stream-basic-description-channels-per-frame
   #:audio-stream-basic-description-bits-per-channel
   #:audio-stream-basic-description-reserved
   #:au-render-callback-struct
   #:au-render-callback-struct-input-proc
   #:au-render-callback-struct-input-proc-ref-con
   #:smpte-time
   #:smpte-time-subframes
   #:smpte-time-subframe-dicisor
   #:smpte-time-counter
   #:smpte-time-type
   #:smpte-time-flags
   #:smpte-time-hours
   #:smpte-time-minutes
   #:smpte-time-seconds
   #:smpte-time-frames
   #:audio-time-stamp
   #:audio-time-stamp-sample-time
   #:audio-time-stamp-host-time
   #:audio-time-stamp-rate-scalar
   #:audio-time-stamp-word-clock-time
   #:audio-time-stamp-smpte-time
   #:audio-time-stamp-flags
   #:audio-time-stamp-reserved
   #:audio-buffer
   #:audio-buffer-number-channels
   #:audio-buffer-data-byte-size
   #:audio-buffer-data
   #:audio-buffer-list
   #:audio-buffer-list-number-buffers
   #:audio-buffer-list-buffers
   #:audio-component-find-next
   #:audio-component-instance-new
   #:audio-component-instance-dispose
   #:audio-unit-set-property
   #:audio-unit-get-property
   #:audio-unit-initialize
   #:audio-unit-uninitialize
   #:audio-output-unit-start
   #:audio-output-unit-stop))
(in-package #:org.shirakumo.fraf.mixed.coreaudio.cffi)

;; https://github.com/rweichler/coreaudio-examples/blob/master/CH07_AUGraphSineWave/main.c
(cffi:define-foreign-library audio-unit
  (:darwin (:framework "AudioUnit")))

(cffi:define-foreign-library audio-toolbox
  (:darwin (:framework "AudioToolbox")))

(set 'cl-user::*foreign-system-libraries*
     (union (when (boundp 'cl-user::*foreign-system-libraries*)
              (symbol-value 'cl-user::*foreign-system-libraries*))
            '(audio-unit audio-toolbox)))

;; Constants
(alexandria:define-constant kAudioUnitType_Output "auou" :test 'equal)
(alexandria:define-constant kAudioUnitSubType_DefaultOutput "def " :test 'equal)
(alexandria:define-constant kAudioUnitManufacturer_Apple "appl" :test 'equal)
(alexandria:define-constant kAudioFormatLinearPCM "lpcm" :test 'equal)
(defconstant no-err 0)

;; Types
(cffi:define-foreign-type os-type () ()
  (:actual-type :int32))

(cffi:define-parse-method os-type ()
  (make-instance 'os-type))

(defmethod cffi:translate-to-foreign (string (type os-type))
  (let ((int 0))
    (dotimes (i 4 int)
      (setf (ldb (byte 8 (* (- 3 i) 8)) int) (char-code (aref string i))))))

(defmethod cffi:translate-from-foreign (integer (type os-type))
  (let ((string (make-string 4)))
    (dotimes (i 4 string)
      (setf (aref string i) (code-char (ldb (byte 8 (* (- 3 i) 8)) integer))))))

(defmethod cffi:free-translated-object (pointer (type os-type) param)
  (declare (ignore param))
  (cffi:foreign-string-free pointer))

(cffi:defctype os-status :int32)
(cffi:defctype audio-component :pointer)
(cffi:defctype component-instance :pointer)
(cffi:defctype component-result :int32)
(cffi:defctype audio-component-instance :pointer)
(cffi:defctype audio-unit component-instance)
(cffi:defctype audio-unit-property-id :uint32)
(cffi:defctype audio-unit-element :uint32)
(cffi:defctype audio-format-id os-type)

;; Enums
(cffi:defcenum render-action-flags
  (:pre-render #.(ash 1 2))
  (:post-render #.(ash 1 3))
  (:output-is-silence #.(ash 1 4))
  (:preflight #.(ash 1 5))
  (:render #.(ash 1 6))
  (:complete #.(ash 1 7))
  (:post-render-error #.(ash 1 8))
  (:do-not-check-render-args #.(ash 1 9)))

(cffi:defcenum (audio-property :uint32)
  (:class-info 0)
  (:make-connection 1)
  (:samplerate 2)
  (:parameter-list 3)
  (:parameter-info 4)
  (:fast-dispatch 5)
  (:cpu-load 6)
  (:stream-format 8)
  (:element-count 11)
  (:latency 12)
  (:supported-num-channels 13)
  (:maximum-frames-per-slice 14)
  (:set-external-buffer 15)
  (:parameter-value-strings 16)
  (:get-ui-component-list 18)
  (:audio-channel-layout 19)
  (:tail-time 20)
  (:bypass-effect 21)
  (:last-render-error 22)
  (:render-callback 23)
  (:factory-presets 24)
  (:context-name 25)
  (:render-quality 26)
  (:host-callbacks 27)
  (:in-place-processing 29)
  (:element-name 30)
  (:cocoa-ui 31)
  (:supported-channel-layout-tags 32)
  (:parameter-string-from-value 33)
  (:parameter-id-name 34)
  (:parameter-clump-name 35)
  (:present-preset 36)
  (:offline-render 37)
  (:parameter-value-from-string 38)
  (:icon-location 39)
  (:presentation-latency 40)
  (:dependent-parameters 45)
  (:au-host-identifier 46)
  (:midi-output-callback-info 47)
  (:midi-output-callback 48)
  (:input-samples-in-output 49)
  (:class-info-from-document 50)
  (:should-allocate-buffer 51)
  (:parameter-history-info 53)
  (:nick-name 54)
  (:request-view-controller 56)
  (:parameters-for-overview 57))

(cffi:defcenum (audio-scope :uint32)
  (:global 0)
  (:in 1)
  (:out 2))

(cffi:defbitfield (audio-format :uint32)
  (:native 0)
  (:float #x1)
  (:signed #x4)
  (:packed #x8))

(cffi:defcenum (run-loop-result :int32)
  (:finished 1)
  (:stopped 2)
  (:timed-out 3)
  (:handled-source 4))

;; Structs
(cffi:defcstruct (component-instance-record :conc-name component-instance-record-)
  (data :long :count 1))

(cffi:defcstruct (audio-component-description :conc-name audio-component-description-)
  (component-type os-type)
  (component-sub-type os-type)
  (component-manufacturer os-type)
  (component-flags :uint32)
  (component-flags-mask :uint32))

(cffi:defcstruct (audio-stream-basic-description :conc-name audio-stream-basic-description-)
  (sample-rate :double)
  (format-id audio-format-id)
  (format-flags audio-format)
  (bytes-per-packet :uint32)
  (frames-per-packet :uint32)
  (bytes-per-frame :uint32)
  (channels-per-frame :uint32)
  (bits-per-channel :uint32)
  (reserved :uint32))

(cffi:defcstruct (au-render-callback-struct :conc-name au-render-callback-struct-)
  (input-proc :pointer)
  (input-proc-ref-con :pointer))

(cffi:defcstruct (smpte-time :conc-name smpte-time-)
  (subframes :int16)
  (subframe-divisor :int16)
  (counter :uint32)
  (type :uint32)
  (flags :uint32)
  (hours :int16)
  (minutes :int16)
  (seconds :int16)
  (frames :int16))

(cffi:defcstruct (audio-time-stamp :conc-name audio-time-stamp-)
  (sample-time :double)
  (host-time :uint64)
  (rate-scalar :double)
  (word-clock-time :uint64)
  (smpte-time (:struct smpte-time))
  (flags :uint32)
  (reserved :uint32))

(cffi:defcstruct (audio-object-property-address :conc-name audio-object-property-address-)
  (selector :uint32)
  (scope :uint32)
  (element :uint32))

(cffi:defcstruct (audio-buffer :conc-name audio-buffer-)
  (capacity :uint32)
  (data :pointer)
  (size :uint32)
  (user-data :pointer)
  (packet-description-capacity :uint32)
  (packet-descriptions :pointer)
  (packet-description-count :uint32))

;; Funcs
(cffi:defcfun (audio-object-get-property-data "AudioObjectGetPropertyData") os-status
  (id :uint32)
  (object :pointer)
  (qualifier-size :uint32)
  (qualifier :pointer)
  (data-size :pointer)
  (data :pointer))

(cffi:defcfun (audio-component-find-next "AudioComponentFindNext") audio-component
  (component audio-component)
  (description :pointer))

(cffi:defcfun (audio-component-instance-new "AudioComponentInstanceNew") os-status
  (component audio-component)
  (output :pointer))

(cffi:defcfun (audio-component-instance-dispose "AudioComponentInstanceDispose") os-status
  (component audio-component-instance))

(cffi:defcfun (audio-unit-set-property "AudioUnitSetProperty") os-status
  (unit audio-unit)
  (property audio-property)
  (scope audio-scope)
  (element audio-unit-element)
  (data :pointer)
  (size :uint32))

(cffi:defcfun (audio-unit-get-property "AudioUnitGetProperty") os-status
  (unit audio-unit)
  (property audio-property)
  (scope audio-scope)
  (element audio-unit-element)
  (data :pointer)
  (size :pointer))

(cffi:defcfun (audio-unit-initialize "AudioUnitInitialize") os-status
  (unit audio-unit))

(cffi:defcfun (audio-unit-uninitialize "AudioUnitUninitialize") os-status
  (unit audio-unit))

(cffi:defcfun (audio-output-unit-start "AudioOutputUnitStart") os-status
  (unit audio-unit))

(cffi:defcfun (audio-output-unit-stop "AudioOutputUnitStop") os-status
  (unit audio-unit))

(cffi:defcfun (audio-queue-new-output "AudioQueueNewOutput") os-status
  (description :pointer)
  (callback :pointer)
  (data :pointer)
  (run-loop :pointer)
  (mode :pointer)
  (flags :uint32)
  (queue :pointer))

(cffi:defcfun (audio-queue-dispose "AudioQueueDispose") os-status
  (queue :pointer)
  (immediate :boolean))

(cffi:defcfun (audio-queue-allocate-buffer "AudioQueueAllocateBuffer") os-status
  (queue :pointer)
  (byte-size :uint32)
  (buffer :pointer))

(cffi:defcfun (audio-queue-free-buffer "AudioQueueFreeBuffer") os-status
  (queue :pointer)
  (buffer :pointer))

(cffi:defcfun (audio-queue-set-property "AudioQueueSetProperty") os-status
  (queue :pointer)
  (property :uint32)
  (address :pointer)
  (size :uint32))

(cffi:defcfun (audio-queue-enqueue-buffer "AudioQueueEnqueueBuffer") os-status
  (queue :pointer)
  (buffer :pointer)
  (packets :uint32)
  (descriptions :pointer))

(cffi:defcfun (audio-queue-enqueue-buffer* "AudioQueueEnqueueBufferWithParameters") os-status
  (queue :pointer)
  (buffer :pointer)
  (packets :uint32)
  (descriptions :pointer)
  (start :uint32)
  (remaining :uint32)
  (param-count :uint32)
  (param-values :pointer)
  (start-time :pointer)
  (time-stamp :pointer))

(cffi:defcfun (audio-queue-start "AudioQueueStart") os-status
  (queue :pointer)
  (start-time :pointer))

(cffi:defcfun (audio-queue-stop "AudioQueueStop") os-status
  (queue :pointer)
  (immediate :boolean))

(cffi:defcfun (run-loop "CFRunLoopRunInMode") run-loop-result
  (mode :pointer)
  (seconds :double)
  (return-after-source-handled :bool))

(cffi:defcfun (get-current-run-loop "CFRunLoopGetCurrent") :pointer)

(cffi:defcfun (cfstr "__CFStringMakeConstantString") :pointer
  (string :string))
