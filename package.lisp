#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-mixed-cffi
  (:nicknames #:org.shirakumo.fraf.mixed.cffi)
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
   #:packed-audio
   #:packed-audio-data
   #:packed-audio-size
   #:packed-audio-encoding
   #:packed-audio-channels
   #:packed-audio-layout
   #:packed-audio-samplerate
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
   #:make-buffer
   #:free-buffer
   #:buffer-from-packed-audio
   #:buffer-to-packed-audio
   #:copy-buffer
   #:clear-buffer
   #:resize-buffer
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
   #:make-segment-unpacker
   #:make-segment-packer
   #:make-segment-basic-mixer
   #:make-segment-volume-control
   #:make-segment-fade
   #:make-segment-generator
   #:make-segment-ladspa
   #:make-segment-space-mixer
   #:make-segment-delay
   #:make-segment-repeat
   #:make-segment-pitch
   #:make-segment-noise
   #:make-segment-frequency-pass
   #:make-segment-queue
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
   #:samplesize
   #:error
   #:error-string
   #:version))

(defpackage #:cl-mixed
  (:nicknames #:org.shirakumo.fraf.mixed)
  (:use #:cl #:cffi)
  (:import-from #:cl-mixed-cffi #:size_t #:samplesize)
  (:shadow #:space)
  ;; buffer.lisp
  (:export
   #:buffer
   #:make-buffer
   #:data
   #:size
   #:clear
   #:with-buffers)
  ;; c-object.lisp
  (:export
   #:handle
   #:c-object
   #:free
   #:pointer->object)
  ;; mixer.lisp
  (:export
   #:mixer
   #:sources
   #:source)
  ;; packed-audio.lisp
  (:export
   #:packed-audio
   #:make-packed-audio
   #:data
   #:size
   #:encoding
   #:channels
   #:layout
   #:samplerate)
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
  ;; segments/delay.lisp
  (:export
   #:delay
   #:make-delay
   #:duration
   #:samplerate
   #:bypass)
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
  ;; segments/packer.lisp
  (:export
   #:packer
   #:packed-audio
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
  ;; segments/queue.lisp
  (:export
   #:queue
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
  ;; segments/unpacker.lisp
  (:export
   #:unpacker
   #:packed-audio
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
