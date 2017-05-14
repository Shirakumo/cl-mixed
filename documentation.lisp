#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.cffi)

;; low-level.lisp
(docs:define-docs
  (variable *here*
    ""
    "Variable containing the path to the low-level.lisp file.")
  
  
  (variable *static*
    ""
    "Variable containing the path to the static directory.
That directory contains the precompiled library binaries."))

(in-package #:org.shirakumo.fraf.mixed)

;; buffer.lisp
(docs:define-docs
  (type buffer
    "Buffers encapsulate raw audio data between segments.

A buffer stores a C-array of floats. This array represents
a sample buffer for a single audio channel. The data is not
encoded in any way and consists solely of single-floats.

Upon construction the foreign array of floats is automatically
allocated based on the given size.

See C-OBJECT
See DATA
See SIZE")
  
  (function make-buffer
    "Create a new buffer capable of storing SIZE floats.

See BUFFER")
  
  (function data
    "Accessor to the raw data array contained in the object.

See BUFFER
See CHANNEL")
  
  (function size
    "Accessor to the size of the data contained in the object.

See BUFFER
See CHANNEL
See MIXER")
  
  (function with-buffers
    "Create a number of buffers for the duration of the body.

BUFFERS should be a list of symbols, to each of which a
fresh instance of a BUFFER with a size of SAMPLES will
be bound."))

;; c-object.lisp
(docs:define-docs
  (variable *c-object-table*
    "A weak value table associating pointer addresses to corresponding objects.

This table keeps track of which foreign data is tracked
by which lisp instance.

See POINTER->OBJECT
See C-OBJECT")
  
  (function handle
    "Accessor to the pointer to the foreign data that this object tracks.

See CFFI:FOREIGN-POINTER
See C-OBJECT")
  
  (type c-object
    "Superclass for all objects that track foreign resources.

If no handle is given to the object upon creation, the proper
corresponding foreign data is automatically allocated. The
pointer to this data is then associated with the instance to
allow resolving the pointer to the original Lisp object.
Finalisation of the foreign data upon garbage collection of
the Lisp object is also handled.

The actual foreign allocation and cleanup of the data is
handled by ALLOCATE-HANDLE and FREE-HANDLE respectively. The
subclass in question is responsible for implementing
appropriate methods for them.

See HANDLE
See ALLOCATE-HANDLE
See FREE-HANDLE
See FREE
See POINTER->OBJECT")
  
  (function allocate-handle
    "Allocate space for the foreign resource of this object.

See C-OBJECT")
  
  (function free-handle
    "Return a function that cleans up the given handle and frees its foreign resources.

Instead of calling this function directly yourself, you should
use FREE.

See FREE
See C-OBJECT")
  
  (function free
    "Free the foreign data associated with this object.

This makes sure that the data is properly cleaned up and that
the object can't accidentally be double-freed or accessed in
any way after the free.

See C-OBJECT")
  
  (function pointer->object
    "Accessor to the object associated with the given foreign pointer.

See *C-OBJECT-TABLE*"))

;; channel.lisp
(docs:define-docs
  (type channel
    "")
  
  (function own-data
    "")
  
  (function make-channel
    "")
  
  (function encoding
    "")
  
  (function channels
    "")
  
  (function layout
    "")
  
  (function samplerate
    ""))

;; mixer.lisp
(docs:define-docs
  (type mixer
    "")
  
  (function segments
    "")
  
  (function make-mixer
    "")
  
  (function add
    "")
  
  (function withdraw
    "")
  
  (function start
    "")
  
  (function mix
    "")
  
  (function end
    ""))

;; segment.lisp
(docs:define-docs
  (function decode-flags
    "")
  
  (function encode-flags
    "")
  
  (function decode-field-info
    "")
  
  (function encode-field-info
    "")
  
  (type segment
    "")
  
  (function inputs
    "")
  
  (function outputs
    "")
  
  (function info
    "")
  
  (function input-field
    "")
  
  (function output-field
    "")
  
  (function field
    "")
  
  (function input
    "")
  
  (function output
    "")
  
  (function connect
    "")
  
  (type many-inputs-segment
    "")
  
  (function add
    "")
  
  (function withdraw
    "")
  
  (type source
    "")
  
  (function channel
    "")
  
  (function make-source
    "")
  
  (type drain
    "")
  
  (function make-drain
    "")
  
  (type linear-mixer
    "")
  
  (function make-linear-mixer
    "")
  
  (type general
    "")
  
  (function make-general
    "")
  
  (function volume
    "")
  
  (function pan
    "")
  
  (type fade
    "")
  
  (function make-fade
    "")
  
  (function from
    "")
  
  (function to
    "")
  
  (function duration
    "")
  
  (function fade-type
    "")
  
  (type generator
    "")
  
  (function make-generator
    "")
  
  (function wave-type
    "")
  
  (function frequency
    "")
  
  (type ladspa
    "")
  
  (function make-ladspa
    "")
  
  (type space
    "")
  
  (function make-space
    "")
  
  (function location
    "")
  
  (function velocity
    "")
  
  (function direction
    "")
  
  (function up
    "")
  
  (function input-location
    "")
  
  (function input-velocity
    "")
  
  (function soundspeed
    "")
  
  (function doppler-factor
    "")
  
  (function min-distance
    "")
  
  (function max-distance
    "")
  
  (function rolloff
    "")
  
  (type virtual
    ""))

;; toolkit.lisp
(docs:define-docs
  (variable *default-samplerate*
    "")
  
  (type mixed-error
    "")
  
  (function error-code
    "")
  
  (function with-error-on-failure
    "")
  
  (function with-cleanup-on-failure
    "")
  
  (function calloc
    "")
  
  (function define-accessor
    "")
  
  (function define-callback
    "")
  
  (function define-std-callback
    "")
  
  (function define-field-accessor
    "")
  
  (function define-vector-field-accessor
    "")
  
  (function define-input-vector-field-accessor
    "")
  
  (function vector-remove-pos
    "")
  
  (function vector-insert-pos
    "")
  
  (function vector-remove
    "")
  
  (function removef
    ""))
