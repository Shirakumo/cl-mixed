#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.cffi
  (:use #:cl #:cffi)
  (:shadow #:error)
  ;; low-level.lisp
  (:export
   #:*static*
   #:libmixed
   #:size_t
   #:error
   #:encoding
   #:layout
   #:field
   #:attenuation
   #:fade-type
   #:generator-type
   #:noise-type
   #:repeat-mode
   #:frequency-pass
   #:field-type
   #:info-flags
   #:location
   #:buffer
   #:buffer-data
   #:buffer-size
   #:buffer-r1-start
   #:buffer-r1-size
   #:buffer-r2-start
   #:buffer-r2-size
   #:buffer-reserved-start
   #:buffer-reserved-size
   #:buffer-virtual-p
   #:pack
   #:pack-data
   #:pack-size
   #:pack-encoding
   #:pack-channels
   #:pack-samplerate
   #:pack-r1-start
   #:pack-r1-size
   #:pack-r2-start
   #:pack-r2-size
   #:pack-reserved-start
   #:pack-reserved-size
   #:field-info
   #:field-info-field
   #:field-info-description
   #:field-info-flags
   #:field-info-type
   #:field-info-type-count
   #:segment-info
   #:segment-info-name
   #:segment-info-description
   #:segment-info-flags
   #:segment-info-min-inputs
   #:segment-info-max-inputs
   #:segment-info-outputs
   #:segment-info-fields
   #:segment
   #:direct-segment-free
   #:direct-segment-info
   #:direct-segment-start
   #:direct-segment-mix
   #:direct-segment-end
   #:direct-segment-set-in
   #:direct-segment-set-out
   #:direct-segment-get-in
   #:direct-segment-get-out
   #:direct-segment-set
   #:direct-segment-get
   #:direct-segment-data
   #:segment-sequence
   #:segment-sequence-segments
   #:segment-sequence-count
   #:segment-sequence-size
   #:make-pack
   #:free-pack
   #:clear-pack
   #:pack-available-write
   #:pack-available-read
   #:pack-request-write
   #:pack-finish-write
   #:pack-request-read
   #:pack-finish-read
   #:make-buffer
   #:free-buffer
   #:buffer-from-pack
   #:buffer-to-pack
   #:transfer-buffer
   #:copy-buffer
   #:clear-buffer
   #:resize-buffer
   #:buffer-available-write
   #:buffer-available-read
   #:buffer-request-write
   #:buffer-finish-write
   #:buffer-request-read
   #:buffer-finish-read
   #:resample-nearest
   #:resample-linear
   #:resample-cubic
   #:free-segment
   #:segment-start
   #:segment-mix
   #:segment-end
   #:segment-set-in
   #:segment-set-out
   #:segment-get-in
   #:segment-get-out
   #:segment-set
   #:segment-get
   #:make-segment-basic-mixer
   #:make-segment-channel-convert
   #:make-segment-delay
   #:make-segment-distribute
   #:make-segment-fade
   #:make-segment-frequency-pass
   #:make-segment-gate
   #:make-segment-generator
   #:make-segment-ladspa
   #:make-segment-noise
   #:make-segment-packer
   #:make-segment-pitch
   #:make-segment-quantize
   #:make-segment-quantize
   #:make-segment-queue
   #:make-segment-repeat
   #:make-segment-space-mixer
   #:make-segment-speed-change
   #:make-segment-unpacker
   #:make-segment-void
   #:make-segment-volume-control
   #:make-segment-zero
   #:queue-add
   #:queue-remove
   #:queue-remove-at
   #:queue-clear
   #:free-segment-sequence
   #:segment-sequence-add
   #:segment-sequence-remove
   #:segment-sequence-start
   #:segment-sequence-mix
   #:segment-sequence-end
   #:load-plugin
   #:close-plugin
   #:register-segment
   #:deregister-segment
   #:list-segments
   #:make-segment-info
   #:make-segment
   #:samplesize
   #:translator-from
   #:translator-to
   #:error
   #:error-string
   #:type-string
   #:version))

(defpackage #:org.shirakumo.fraf.mixed
  (:use #:cl #:cffi)
  (:import-from #:org.shirakumo.fraf.mixed.cffi #:size_t)
  (:shadow #:space #:byte-position)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed.cffi))
  ;; bip-buffer.lisp
  (:export
   #:available-read
   #:available-write
   #:request-write
   #:finish-write
   #:request-read
   #:finish-read
   #:data-ptr
   #:with-buffer-tx
   #:finish
   #:with-buffer-transfer)
  ;; buffer.lisp
  (:export
   #:buffer
   #:make-buffer
   #:data
   #:size
   #:clear
   #:with-buffers
   #:transfer)
  ;; c-object.lisp
  (:export
   #:handle
   #:c-object
   #:free
   #:pointer->object)
  ;; drain.lisp
  (:export
   #:drain
   #:program-name
   #:pack)
  ;; mixer.lisp
  (:export
   #:mixer
   #:sources
   #:source)
  ;; pack.lisp
  (:export
   #:pack
   #:make-pack
   #:data
   #:size
   #:encoding
   #:channels
   #:samplerate
   #:transfer
   #:clear
   #:framesize)
  ;; segment-sequence.lisp
  (:export
   #:segment-sequence
   #:make-segment-sequence
   #:segments
   #:add
   #:withdraw
   #:start
   #:mix
   #:end
   #:size)
  ;; segment.lisp
  (:export
   #:segment
   #:inputs
   #:outputs
   #:info
   #:start
   #:mix
   #:end
   #:input-field
   #:output-field
   #:field
   #:input
   #:output
   #:connect)
  ;; source.lisp
  (:export
   #:source
   #:pack
   #:byte-position
   #:seek
   #:seek-to-frame
   #:frame-count)
  ;; toolkit.lisp
  (:export
   #:*default-samplerate*
   #:mixed-error
   #:error-code
   #:samplesize)
  ;; segments/basic-mixer.lisp
  (:export
   #:basic-mixer
   #:channels
   #:make-basic-mixer
   #:volume)
  ;; segments/channel-convert.lisp
  (:export
   #:channel-convert
   #:make-channel-convert)
  ;; segments/delay.lisp
  (:export
   #:delay
   #:make-delay
   #:duration
   #:samplerate
   #:bypass)
  ;; segments/distribute.lisp
  (:export
   #:distributor
   #:make-distributor)
  ;; segments/fader.lisp
  (:export
   #:fader
   #:make-fader
   #:from
   #:to
   #:duration
   #:fade-type
   #:bypass)
  ;; segments/frequency-pass.lisp
  (:export
   #:frequency-pass
   #:make-frequency-pass
   #:cutoff
   #:frequency-pass
   #:samplerate
   #:bypass)
  ;; segments/gate.lisp
  (:export
   #:gate
   #:make-gate
   #:samplerate
   #:open-threshold
   #:close-threshold
   #:attack
   #:hold
   #:release
   #:bypass)
  ;; segments/generator.lisp
  (:export
   #:generator
   #:make-generator
   #:volume
   #:wave-type
   #:frequency)
  ;; segments/ladspa.lisp
  (:export
   #:ladspa
   #:make-ladspa
   #:field)
  ;; segments/noise.lisp
  (:export
   #:noise
   #:make-noise
   #:volume
   #:noise-type)
  ;; segments/null.lisp
  (:export
   #:void
   #:make-void
   #:zero
   #:make-zero)
  ;; segments/packer.lisp
  (:export
   #:packer
   #:pack
   #:make-packer
   #:data
   #:size
   #:encoding
   #:channels
   #:layout
   #:samplerate
   #:volume
   #:bypass)
  ;; segments/pitch.lisp
  (:export
   #:pitch
   #:make-pitch
   #:pitch
   #:samplerate
   #:bypass)
  ;; segments/quantize.lisp
  (:export
   #:quantize
   #:make-quantize
   #:steps
   #:bypass)
  ;; segments/queue.lisp
  (:export
   #:queue
   #:make-queue
   #:current-segment
   #:in-count
   #:out-count
   #:bypass
   #:add
   #:withdraw
   #:clear)
  ;; segments/repeat.lisp
  (:export
   #:repeat
   #:make-repeat
   #:duration
   #:repeat-mode
   #:samplerate
   #:bypass)
  ;; segments/space-mixer.lisp
  (:export
   #:space-mixer
   #:make-space-mixer
   #:location
   #:velocity
   #:direction
   #:up
   #:input-location
   #:input-velocity
   #:soundspeed
   #:doppler-factor
   #:min-distance
   #:max-distance
   #:rolloff
   #:volume
   #:field
   #:attenutation)
  ;; segments/speed-change.lisp
  (:export
   #:speed-change
   #:make-speed-change
   #:speed-factor
   #:bypass)
  ;; segments/unpacker.lisp
  (:export
   #:unpacker
   #:pack
   #:make-unpacker
   #:data
   #:size
   #:encoding
   #:channels
   #:layout
   #:samplerate
   #:volume
   #:bypass)
  ;; segments/virtual.lisp
  (:export
   #:virtual
   #:info
   #:start
   #:mix
   #:end
   #:input-field
   #:output-field
   #:field)
  ;; segments/volume-control.lisp
  (:export
   #:volume-control
   #:make-volume-control
   #:volume
   #:pan
   #:bypass))
