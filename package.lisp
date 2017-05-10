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
   #:uint8_t
   #:error
   #:encoding
   #:layout
   #:field
   #:attenuation
   #:fade-type
   #:generator-type
   #:info-flags
   #:location
   #:buffer
   #:buffer-data
   #:buffer-size
   #:channel
   #:channel-data
   #:channel-size
   #:channel-encoding
   #:channel-channels
   #:channel-layout
   #:channel-samplerate
   #:field-info
   #:field-info-field
   #:field-info-description
   #:field-info-flags
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
   #:mixer
   #:mixer-segments
   #:mixer-count
   #:mixer-size
   #:make-buffer
   #:free-buffer
   #:buffer-from-channel
   #:buffer-to-channel
   #:copy-buffer
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
   #:make-segment-source
   #:make-segment-drain
   #:make-segment-mixer
   #:make-segment-general
   #:make-segment-fade
   #:make-segment-generator
   #:make-segment-ladspa
   #:make-segment-space
   #:free-mixer
   #:mixer-add
   #:mixer-remove
   #:mixer-start
   #:mixer-mix
   #:mixer-end
   #:samplesize
   #:error
   #:error-string))

(defpackage #:cl-mixed
  (:nicknames #:org.shirakumo.fraf.mixed)
  (:use #:cl #:cffi)
  (:import-from #:cl-mixed-cffi #:size_t)
  (:shadow #:space)
  ;; buffer.lisp
  (:export
   #:buffer
   #:data
   #:size)
  ;; c-object.lisp
  (:export
   #:handle
   #:c-object
   #:free
   #:pointer->object)
  ;; channel.lisp
  (:export
   #:channel
   #:data
   #:size
   #:encoding
   #:channels
   #:layout
   #:samplerate)
  ;; mixer.lisp
  (:export
   #:mixer
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
   #:many-inputs-segment
   #:add
   #:withdraw
   #:source
   #:drain
   #:linear-mixer
   #:general
   #:volume
   #:pan
   #:fade
   #:from
   #:to
   #:duration
   #:fade-type
   #:generator
   #:wave-type
   #:frequency
   #:ladspa
   #:space
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
   #:virtual)
  ;; toolkit.lisp
  (:export
   #:*default-samplerate*
   #:mixed-error
   #:error-code))
