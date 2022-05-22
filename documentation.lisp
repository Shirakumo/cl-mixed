#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.cffi)

;; low-level.lisp
(docs:define-docs
  (variable *here*
    "Variable containing the path to the low-level.lisp file.")
    
  (variable *static*
    "Variable containing the path to the static directory.
That directory contains the precompiled library binaries.")

  (function samplesize
    "Return the number of bytes required to represent a sample in the given format.

Acceptable values are
  :INT8 :UINT8 :INT16 :UINT16 :INT24 :UINT24 :INT32 :UINT32
  :FLOAT :DOUBLE"))

(in-package #:org.shirakumo.fraf.mixed)

;; bip-buffer.lisp
(docs:define-docs
  (type bip-buffer
    "Base class for all buffer types.

Implements an interface to allow for asynchronous read/write to the
buffer.

See DATA
See AVAILABLE-READ
See AVAILABLE-WRITE
See REQUEST-WRITE
See FINISH-WRITE
See REQUEST-READ
See FINISH-READ
See DATA-PTR
See WITH-BUFFER-TX
See WITH-BUFFER-TRANSFER")

  (function available-read
    "Returns the number of elements available for reading in the buffer.

See BIP-BUFFER
See BUFFER
See PACK")

  (function available-write
    "Returns the number of elements that can be written to the buffer.

See BIP-BUFFER
See BUFFER
See PACK")

  (function request-write
    "Prepares a writing operation on the buffer.

Returns two values:
  START -- The number of elements after which the write may begin.
  SIZE  -- The number of elements that may be written.

After calling this, you **must** call FINISH-WRITE before calling
REQUEST-WRITE again.

See FINISH-WRITE
See BIP-BUFFER
See BUFFER
See PACK")

  (function finish-write
    "Finishes a writing transaction.

You must not call this function without a matching REQUEST-WRITE call
first. The SIZE should be the number of written elements, which may be
less than the number obtained from REQUEST-WRITE, but not more.

See REQUEST-WRITE
See BIP-BUFFER
See BUFFER
See PACK")

  (function request-read
    "Prepares a reading operation on the buffer.

Returns two values:
  START -- The number of elements after which the read may begin.
  SIZE  -- The number of elements that may be read.

After calling this, you **must** call FINISH-READ before calling
REQUEST-WRITE again.

See FINISH-READ
See BIP-BUFFER
See BUFFER
See PACK")

  (function finish-read
    "Finishes a reading transaction.

You must not call this function without a matching REQUEST-READ call
first. The SIZE should be the number of read elements, which may be
less than the number obtained from REQUEST-READ, but not more.

See REQUEST-READ
See BIP-BUFFER
See BUFFER
See PACK")

  (function data-ptr
    "Returns a foreign pointer to the underlying storage of the data array.

See BIP-BUFFER
See BUFFER
See PACK")

  (function with-buffer-tx
    "Convenience macro to handle a buffer transaction.

DATA is bound to the storage array of the buffer.
START is bound to the starting index of the transaction.
SIZE is bound to the number of elements that may be operated on during
the transaction.
BUFFER should be a BIP-BUFFER instance.
DIRECTION can be either :INPUT or :OUTPUT depending on the type of
transaction desired.
INITIAL-SIZE should be the amount of space to request.

During BODY, two functions are available:
  FINISH   --- Completes the transaction, using the passed number of
               elements. Note that this does not cause an unwind.
  DATA-PTR --- Returns a foreign pointer to the start of the
               transaction's valid memory.

This macro ensures that on unwind for any reason, whether after FINISH
or before, the buffer is left in a sealed state where it is safe to
call REQUEST-READ and REQUEST-WRITE again.

See BIP-BUFFER
See BUFFER
See PACK
See WITH-BUFFER-TRANSFER")

  (function with-buffer-transfer
    "Convenience macro to handle a transfer from one buffer to another.

Both FROM and TO may be the same buffer, in which case the transfer
happens from the region available to read to itself.

Otherwise, this is akin to nesting WITH-BUFFER-TX, with the special
exemption that FINISH will complete the transaction on both buffers at
once.

See BIP-BUFFER
See BUFFER
See PACK
See WITH-BUFFER-TRANSFER"))

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
See SIZE
See CLEAR")
  
  (function make-buffer
    "Create a new buffer.

You may pass either an integer denoting the length of the
buffer in samples, or a vector of single-floats denoting the
buffers' initial contents.

See BUFFER")
  
  (function data
    "Accessor to the raw data array contained in the object.

See BUFFER
See PACK")
  
  (function size
    "Accessor to the size of the data contained in the object.

For packed-audio this number is in bytes.
For buffers this number is in floats.

When the size is set on a buffer, the buffer's data array is
resized to fit the requested size.

See BUFFER
See PACK
See SEGMENT-SEQUENCE")

  (function clear
    "Clears the buffer to fill its data array with just zeroes.

See BUFFER")
  
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

;; mixer.lisp
(docs:define-docs
  (type mixer
    "Superclass for segments that mix a variable number of sources together.

See SEGMENT
See ADD
See WITHDRAW
See SOURCES
See SOURCE")

  (function sources
    "Accessor to the hash table mapping input locations to segments.

This map holds which locations have segments attached.
You should use the INPUT-FIELD or SOURCE accessors to
actually attach or detach source segments.

See MIXER
See SOURCE
See INPUT-FIELD")

  (function source
    "Accessor to the source segment attached to an input buffer or location.

Some mixers support attaching a source
segment to an input buffer. The effect being that the
segment is mixed before the corresponding buffer is
used, allowing for dynamic addition and removal of
segments without the need to alter the pipeline.

See MIXER
See SOURCES"))

;; pack.lisp
(docs:define-docs
  (type pack
    "A pack represents an interface to an outside sound source or drain.

The object holds all the necessary information to describe
the audio data present in a raw byte buffer. This includes
how many channels there are, how the samples are laid out
and how the samples are formatted in memory. It also includes
the samplerate of the channel's source so that it can be
converted if necessary.

Note that a pack only supports interleaved channel data. If the data
is sequential in memory, it must be handled by multiple packs.

See MAKE-PACK
See SOURCE
See DRAIN
See C-OBJECT
See DATA
See SIZE
See ENCODING
See CHANNELS
See SAMPLERATE
See FRAMESIZE
See TRANSFER")
  
  (function make-pack
    "Create a new pack object.

See PACK")
  
  (function encoding
    "Accessor to the sample encoding of the raw data buffer in the object.

The encoding has to be one of the following:
 :INT8 :UINT8 :INT16 :UINT16 :INT24 :UINT24 :INT32 :UINT32
 :FLOAT :DOUBLE

See PACK")
  
  (function channels
    "Accessor to the number of channels encoded in the data buffer in the object.

See PACK")
  
  (function samplerate
    "Accessor to the samplerate at which the samples are expected to be.

The sample rate is in Hz.

See PACK
See DELAY
See FADER
See BIQUAD-FILTER
See PITCH
See REPEAT
See SPACE-MIXER"))

;; segment-sequence.lisp
(docs:define-docs
  (type segment-sequence
    "A segment-sequence represents a mixing pipeline.

The sequence takes care of remembering the order in which
segments should be processed and running them in that
order when mixing is done.

See MAKE-SEGMENT-SEQUENCE
See C-OBJECT
See SEGMENTS
See ADD
See WITHDRAW
See START
See MIX
See END")
  
  (function segments
    "Accessor to a vector of segments the sequence contains.

This vector will become out of date if the sequence's
segments are added or removed from the C side
directly, or directly through this vector. Thus you
should never modify this directly and instead always
make sure to go through ADD/WITHDRAW.

See SEGMENT-SEQUENCE
See ADD
See WITHDRAW")
  
  (function make-segment-sequence
    "Create a new segment-sequence instance.

The segments given are added in order.

See SEGMENT-SEQUENCE")
  
  (function add
    "Add the object to the given collection.

For segment-sequences this means adding a segment to
the sequence. For mixers this means adding another
input buffer.

See SEGMENT-SEQUENCE
See MIXER")
  
  (function withdraw
    "Remove the object from the given collection.

For segment-sequences this means removing the segment
from the sequence. For mixers this means removing
the given input buffer.

See SEGMENT-SEQUENCE
See MIXER")
  
  (function start
    "Start the mixing process.

This method should be called as close as possible
to the next calls to MIX. Calling MIX before
START is called or after END is called is an error.
After START has been called, changing some segments'
fields may result in undefined behaviour and might
even lead to crashes.

See SEGMENT-SEQUENCE
See SEGMENT
See END
See MIX")
  
  (function mix
    "Perform the actual mixing.

This processes the given number of samples through
the pipeline. It is your job to make sure that
sources provide enough fresh samples and drains
will consume enough samples. Calling MIX with more
samples specified than any one buffer connected to
the segments in the sequence can hold is an error and
may crash your system. No checks for this problem
are done.

Calling MIX before START has been called or after
END has been called is an error and may result in
crashes. No checks for this problem are done.

If you want to ensure that the sequence is complete
and able to process the requested number of samples,
you should call CHECK-COMPLETE after running START.

When adding methods to MIX for virtual segments, you
should make sure to return true, unless your segment
has somehow ended and exhausted the samples it wants
to process, in which case you should return NIL.

See SEGMENT-SEQUENCE
See SEGMENT
See START
See END")
  
  (function end
    "End the mixing process.

This method should be called as close as possible
after all desired calls to MIX are done. Calling
MIX after END is called is an error. Some segments
may require END to be called before their fields
can be set freely. Thus mixing might need to be
'paused' to change settings.

See SEGMENT-SEQUENCE
See SEGMENT
See START
See MIX"))

;; segment.lisp
(docs:define-docs
  (function decode-flags
    "Decode an OR combined integer of INFO-FLAGS to a list of keywords for the flags.

See MIXED:INFO-FLAGS")
  
  (function encode-flags
    "Encode the list of keywords for the INFO-FLAGS to a an OR combined integer.

See MIXED:INFO-FLAGS")
  
  (function decode-field-info
    "Decode the field info contained in the pointer to a segment-info struct.

See MIXED:SEGMENT-INFO
See MIXED:FIELD-INFO")
  
  (function encode-field-info
    "Encode the field info into the segment-info struct pointed to by the pointer.

See MIXED:SEGMENT-INFO
See MIXED:FIELD-INFO")
  
  (type segment
    "Superclass for all mixing pipeline segments.

A segment is responsible for producing, consuming,
combining, splitting, or just in general somehow
processing audio data within a pipeline.

A segment is connected to several buffers at its
inputs and outputs. Each input, output, and the segment
itself may have a number of fields that can be accessed
to change the properties of the segment's behaviour.

Some of these properties may not be changed in real
time and instead might require a ending the mixing
first.

See C-OBJECT
See INPUTS
See OUTPUTS
See INFO
See START
See MIX
See END
See INPUT-FIELD
See OUTPUT-FIELD
See FIELD
See INPUT
See OUTPUT
See CONNECT")
  
  (function inputs
    "Accessor to the vector of input buffers connected to the segment.

This vector will become out of date if the segment's
buffers are added or removed from the C side directly,
or directly through this vector. Thus you should never
modify this directly and instead always
make sure to go through INPUT.

See SEGMENT
See INPUT")
  
  (function outputs
    "Accessor to the vector of output buffers connected to the segment.

This vector will become out of date if the segment's
buffers are added or removed from the C side directly,
or directly through this vector. Thus you should never
modify this directly and instead always
make sure to go through OUTPUT

See SEGMENT
See OUTPUT")

  (function direct-info
    "Direct accessor to the info slot on the segment.

See SEGMENT
See INFO")
  
  (function info
    "Fetch metadata information about the segment.

Returns a plist with the following entries:
:NAME         --- A string denoting the name of the
                  type of segment this is.
:DESCRIPTION  --- A string denoting a human-readable
                  description of the segment.
:FLAGS        --- A list of flags for the segment.
                  Should be one of:
  :INPLACE      --- Output and input buffers may be
                    identical as processing is
                    in-place.
  :MODIFIES-INPUT --- The data in the input buffer
                    is modified.
:MIN-INPUTS   --- The minimal number of inputs that
                  needs to be connected to this
                  segment.
:MAX-INPUTS   --- The maximal number of inputs that
                  may be connected to this segment.
:OUTPUTS      --- The number of outputs that this
                  segment provides.
:FIELDS       --- A list of plists describing the
                  possible flags. Each plist has the
                  following entries:
  :FIELD        --- The keyword or integer denoting
                    the field.
  :DESCRIPTION  --- A string for a human-readable
                    description of what the field
                    does.
  :FLAGS        --- A list of keywords describing the
                    applicability of the field. Must
                    be one of:
    :IN           --- This field is for inputs.
    :OUT          --- This field is for outputs.
    :SEGMENT      --- This field is for the segment.
    :SET          --- This field may be written to.
    :GET          --- This field may be read.

Note that this value is cached after the first
retrieval. You are thus not allowed to modify the
return value.

See SEGMENT")

  (function bypass
    "Accessor to whether the segment is bypassed or not.

A bypassed segment does not perform any operations when
mixed. The exact effects of this varies per segment, but
usually for a segment that transforms its inputs
somehow this will mean that it just copies the input to
the output verbatim.

Note that not all segments support bypassing. Check the
:FIELDS value in the field's info plist.

See SEGMENT
See INFO")
  
  (function input-field
    "Access the field of an input of the segment.

Which fields are supported depends on the segment in
question. Usually the :BUFFER field will be recognised
for which the value should be a BUFFER instance.

Some fields may only be read and not written to or
vice-versa.

See SEGMENT
See INPUT")
  
  (function output-field
    "Access the field of an output of the segment.

Which fields are supported depends on the segment in
question. Usually the :BUFFER field will be recognised
for which the value should be a BUFFER instance.

Some fields may only be read and not written to or
vice-versa.

See SEGMENT
See OUTPUT")
  
  (function field
    "Access the field of the segment.

Which fields are supported depends on the segment in
question.

Some fields may only be read and not written to or
vice-versa.

See SEGMENT")
  
  (function input
    "Accessor to the input buffer at the specified location of the segment.

See INPUT-FIELD
See SEGMENT")
  
  (function output
    "Accessor to the output buffer at the specified location of the segment.

See OUTPUT-FIELD
See SEGMENT")
  
  (function connect
    "Connect two segments together by connecting their respective input and output to a specific buffer.

See INPUT
See OUTPUT
See SEGMENT"))

;; toolkit.lisp
(docs:define-docs
  (variable *default-samplerate*
    "This variable holds the default sample rate used throughout.

This is set to 44100 for 44.1 kHz, which is
the standard sample rate for CD audio and should
thus be of sufficient quality for most purposes.")
  
  (type mixed-error
    "Condition class for errors related to the mixed library.

See ERROR-CODE")
  
  (function error-code
    "Accessor for the error code contained in the condition instance.

See MIXED:ERROR
See MIXED:ERROR-STRING")
  
  (function with-error-on-failure
    "Shorthand to handle return values of C functions.

If the last form in the body returns a zero, an
error of type MIXED-ERROR is signalled.

See MIXED-ERROR")
  
  (function with-cleanup-on-failure
    "If the body unwinds abnormally, CLEANUP is run.")
  
  (function calloc
    "Allocate a region of foreign data on the heap.

This is like CFFI:FOREIGN-ALLOC, except that the
memory region is zeroed out before the pointer to it
is returned.")
  
  (function define-accessor
    "Define a new accessor wrapper for a CFFI struct function.")
  
  (function define-callback
    "Defines a new callback that handles errors automatically.

If an error occurs within BODY, the ERROR-RETURN form
is instead evaluated and its value returned.

See CFFI:DEFCALLBACK")
  
  (function define-std-callback
    "Define a standard callback function.

Standard means that the function will return 1 on
success and 0 on abnormal exit.

See DEFINE-CALLBACK")

  (function coerce-ctype
    "Coerce the given value to the type and range appropriate for the given C-type.

Coercion is done for:
:FLOAT
:DOUBLE
:INT
:UINT")
  
  (function define-field-accessor
    "Define an accessor for a segment's field.

Generates the necessary methods on FIELD as well as
convenience wrapper methods.")

  (function ptr->vec
    "Convert a pointer to a C array of three floats to a vector.")

  (function vec->ptr
    "Convert the vector into a C array of three floats")
  
  (function define-vector-field-accessor
    "Define an accessor for a segment's vector value field.

Generates the necessary methods on FIELD as well as
convenience wrapper methods. The values should be
lists or vectors of three floats.")
  
  (function define-input-vector-field-accessor
    "Define an accessor for a segment's input vector value field.

Generates the necessary methods on FIELD as well as
convenience wrapper methods. The values should be
lists or vectors of three floats.")

  (function define-delegated-slot-accessor
    "Define an accessor that delegates its call to a slot of the instance.

Generates the necessary accessor methods to wrap
the access.")
  
  (function vector-remove-pos
    "Remove the element at the specified index in the vector.

Elements above it are shifted down and the vector's
size is adjusted.")
  
  (function vector-insert-pos
    "Set the value at the given position in the vector.

If the position is beyond the vector's length, it is
first adjusted to that length.")
  
  (function vector-remove
    "Remove the element from the vector.

Elements above it are shifted down and the vector's
size is adjusted. Only the first occurrence is
removed.

See VECTOR-REMOVE-POS")
  
  (function removef
    "Remove the given key/value pairs from the plist.

Returns a fresh list."))

;; segments/basic-mixer.lisp
(docs:define-docs
  (type basic-mixer
    "This segment additively mixes an arbitrary number of inputs to a single output.

Linear mixing means that all the inputs are summed
up and the resulting number is divided by the number
of inputs. This is equivalent to having all the
inputs play as \"individual speakers\" in real life.

See MIXER
See MAKE-BASIC-MIXER")
  
  (function make-basic-mixer
    "Create a new basic mixer, adding the given buffers as inputs.

See BASIC-MIXER
See ADD
See WITHDRAW"))

;; segments/delay.lisp
(docs:define-docs
  (type delay
    "A simple delay segment.

This just delays the input to the output by a given
number of seconds. Note that it will require an
internal buffer to hold the samples for the required
length of time, which might become expensive for very
long durations.

See SEGMENT
See MAKE-DELAY
See DURATION
See SAMPLERATE
See BYPASS")

  (function make-delay
    "Create a new delay segment.

See DELAY"))

;; segments/fader.lisp
(docs:define-docs
  (type fader
    "A volume fading segment.

This allows you to smoothly fade in and out an input.

The from and to are given in relative volume, meaning
in the range of [0.0, infinity[. The duration is given
in seconds. The fade type must be one of the following:
:LINEAR :CUBIC-IN :CUBIC-OUT :CUBIC-IN-OUT, each
referring to the respective easing function.
The time is measured from the last call to START out.

See SEGMENT
See MAKE-FADER
See FROM
See TO
See DURATION
See FADE-TYPE")
  
  (function make-fader
    "Create a new volume fader segment.

See FADER")
  
  (function from
    "Accessor to the starting volume of the fading segment.

Must be in the range of [0.0, infinity[.

See FADER")
  
  (function to
    "Accessor to the ending volume of the fading segment.

Must be in the range of [0.0, infinity[.

See FADER")
  
  (function duration
    "Accessor to the duration of the segment's effect.

Must be measured in seconds.

See FADER
See DELAY
See REPEAT")
  
  (function fade-type
    "Accessor to the type of easing function used for the fade.

Must be one of :LINEAR :CUBIC-IN :CUBIC-OUT :CUBIC-IN-OUT

See FADER"))

;; segments/biquad-filter.lisp
(docs:define-docs
  (type biquad-filter
    "A biquad filter for simple frequency filtering operations.

See SEGMENT
See MAKE-BIQUAD-FILTER
See FREQUENCY
See FILTER
See SAMPLERATE
See BYPASS
See Q
See GAIN")

  (function make-biquad-filter
    "Create a new frequency pass segment.

PASS can be either :high or :low, which will cause
high and low frequencies to pass through the filter
respectively.

See BIQUAD-FILTER")

  (function frequency
    "Accessor to the frequency around which the filter is tuned.

See BIQUAD-FILTER")

  (function biquad-filter
    "Accessor to whether the biquad-filter segment passes high or low frequencies.

Value must be either :HIGH or :LOW.

See BIQUAD-FILTER"))

;; segments/generator.lisp
(docs:define-docs
  (type generator
    "A primitive tone generator segment.

This segment can generate sine, square, sawtooth, and
triangle waves at a requested frequency. You can even
change the frequency and type on the fly.

See SEGMENT
See *DEFAULT-SAMPLERATE*
See WAVE-TYPE
See FREQUENCY")
  
  (function make-generator
    "Create a new tone generator.

See GENERATOR")
  
  (function wave-type
    "Accessor to the wave type the generator generates.

Must be one of :SINE :SQUARE :SAWTOOTH :TRIANGLE

See GENERATOR")
  
  (function frequency
    "Accessor to the frequency of the wave.

Must be in the range [0.0, samplerate].

See GENERATOR"))

;; segments/ladspa.lisp
(docs:define-docs
  (type ladspa
    "This segment invokes a LADSPA plugin.

LADSPA (Linux Audio Developers' Simple Plugin API)
is a standard interface for audio effects. Such effects
are contained in shared library files and can be loaded
in and used with libmixed straight up.

Please refer to the plugin's documentation for necessary
configuration values, and to the libmixed documentation
for how to set them.

See SEGMENT
See *DEFAULT-SAMPLERATE*
See MAKE-LADSPA")
  
  (function make-ladspa
    "Create a new LADSPA segment.

The file must point to a valid shared library and the
index should designate the requested plugin with the
library.

Any additional keys are used to set the corresponding
fields on the segments, allowing you to directly
configure the LADSPA plugin in the same call.

See LADSPA"))

;; segments/noise.lisp
(docs:define-docs
  (type noise
    "A noise generator segment.

This segment produces a single channel of noise,
either white, pink, or brownian noise.

See SEGMENT
See MAKE-NOISE
See VOLUME
See NOISE-TYPE")

  (function make-noise
    "Create a new noise segment.

The type can be one of :WHITE, :PINK, :BROWN.

See NOISE")

  (function noise-type
    "Accessor to the type of noise being generated.

The value must be one of :WHITE, :PINK, :BROWN.

See NOISE"))

;; segments/packer.lisp
(docs:define-docs
  (type packer
    "This segment converts data from individual sample buffers to data for packed-audio.

This is mostly useful at the edges to convert to
something like an audio file library or audio
playback system from the internal buffers as used
by Mixed.

The samplerate argument defines the sample rate
in the input buffers. If it diverges from the
sample rate in the packed-audio, resampling occurs to
account for this.

See mixed.h
See PACK
See SEGMENT
See MAKE-PACKER
See *DEFAULT-SAMPLERATE*")
  
  (function make-packer
    "Create a new packer segment.

This automatically creates a packed-audio object to use.
If you prefer to use a packed-audio object you created
yourself, simply use MAKE-INSTANCE instead.

See PACK
See PACKER"))

;; segments/pitch.lisp
(docs:define-docs
  (type pitch
    "A pitch shifting segment.

This segment allows you to change the pitch of the
incoming audio.

See MAKE-PITCH
See PITCH
See SAMPLERATE
See BYPASS")

  (function make-pitch
    "Create a new pitch shifting segment.

See PITCH")

  (function pitch
    "Accessor to the current pitch shifting value.

The pitch shift is denoted as a float in the range
of ]0,infinity[, where 1 is no change, 0.5 halves the
pitch, and 2 doubles the pitch.

Note that extreme values on either side will result
in heavy distortions and quality loss. Anything
outside the range of [0.5,2.0] will already result in
audible artefacts.

See PITCH"))

;; segments/repeat.lisp
(docs:define-docs
  (type repeat
    "This segment repeats a recorded bit of its input sequence.

The segment is first in record mode when created.
Once you have the bit you want to repeat back, you
can switch the repeat-mode to :PLAY. It will then
ignore the input and instead continuously output the
recorded input bit.

See MAKE-REPEAT
See DURATION
See REPEAT-MODE
See SAMPLERATE
See BYPASS")

  (function make-repeat
    "Create a new repeat segment.

The time designates the size of the internal buffer
that it can repeat to the output.

See REPEAT")

  (function repeat-mode
    "Accessor to the mode the repeat segment is currently in.

The value must be either :RECORD or :PLAY.
When in record mode, the segment will fill its internal
buffer with the samples from the input buffer, and copy
them to the output buffer. While in this mode it is thus
\"transparent\" and does not change anything.
When in play mode, the segment continuously plays back
its internal buffer to the output buffer, ignoring all
samples on the input buffer.

See REPEAT"))

;; segments/space-mixer.lisp
(docs:define-docs
  (type space-mixer
    "A segment to simulate audio effects in 3D space.

Each input represents an individual source in space.
Each such source can have a location and a velocity,
both of which are vectors of three elements. If the
velocity is non-zero, a doppler effect is applied to
the source.

The segment itself also has a :LOCATION and :VELOCITY,
representing the listener's own properties. It has
some additional fields to change the properties of the
3D space. In total, the following fields are available:

  :LOCATION        --- The location of the input or
                       listener. Value should be a list
                       of three floats.
  :VELOCITY        --- The velocity of the input or
                       listener. Value should be a list
                       of three floats.
  :DIRECTION       --- The direction the listener is
                       facing. Value should be a list of
                       three floats. Default is (0 0 1)
  :UP              --- The vector pointing \"up\" in
                       space. Value should be a list of
                       three floats. Default is (0 1 0)
  :SOUNDSPEED      --- The speed of sound in the medium.
                       Default is 34330, meaning \"1\" is
                       measured as 1cm.
  :DOPPLER-FACTOR  --- This can be used to over- or
                       understate the effect of the
                       doppler. Default is 1.0.
  :MIN-DISTANCE    --- The minimal distance under which
                       the source has reached max volume.
  :MAX-DISTANCE    --- The maximal distance over which
                       the source is completely inaudible.
  :ROLLOFF         --- The rolloff factor describing the
                       curvature of the attenuation
                       function.
  :ATTENUATION     --- The attenuation function describing
                       how volume changes over distance.
                       Should be one of :NONE :LINEAR
                       :INVERSE :EXPONENTIAL.

See MIXER
See MAKE-SPACE-MIXER
See LOCATION
See INPUT-LOCATION
See VELOCITY
See INPUT-VELOCITY
See DIRECTION
See UP
See SOUNDSPEED
See DOPPLER-FACTOR
See MIN-DISTANCE
See MAX-DISTANCE
See ROLLOFF
See ATTENUATION
See *DEFAULT-SAMPLERATE*")
  
  (function make-space-mixer
    "Create a new space-mixer segment for 3D audio processing.

See SPACE-MIXER")
  
  (function location
    "Accessor for the location of the listener in space.

To set a vector, the value should be a list or a vector
of three floats. When reading, the returned value is
always a vector of three floats.

See SPACE-MIXER")
  
  (function velocity
    "Accessor for the velocity of the listener in space.

To set a vector, the value should be a list or a vector
of three floats. When reading, the returned value is
always a vector of three floats.

See SPACE-MIXER")
  
  (function direction
    "Accessor for the direction the listener is facing in space.

To set a vector, the value should be a list or a vector
of three floats. When reading, the returned value is
always a vector of three floats.

See SPACE-MIXER")
  
  (function up
    "Accessor for the vector representing \"upwards\" in space.

To set a vector, the value should be a list or a vector
of three floats. When reading, the returned value is
always a vector of three floats.

See SPACE-MIXER")
  
  (function input-location
    "Accessor for the location of the source in space.

To set a vector, the value should be a list or a vector
of three floats. When reading, the returned value is
always a vector of three floats.

See SPACE-MIXER")
  
  (function input-velocity
    "Accessor for the velocity of the source in space.

To set a vector, the value should be a list or a vector
of three floats. When reading, the returned value is
always a vector of three floats.

See SPACE-MIXER")
  
  (function soundspeed
    "Accessor to the speed of sound in space.

This value only influences the strength of
the doppler factor.

See SPACE-MIXER")
  
  (function doppler-factor
    "Accessor to the over-/under-statement factor of the doppler effect.

See SPACE-MIXER")
  
  (function min-distance
    "Accessor to the minimal distance below which the source is at max volume.

See SPACE-MIXER")
  
  (function max-distance
    "Accessor to the maximal distance above which the source is inaudible.

See SPACE-MIXER")
  
  (function rolloff
    "Accessor to the rolloff factor that describes the curvature of the attenuation function.

See SPACE-MIXER")

  (function attenuation
    "Accessor to the attenuation function used to describe the distance volume falloff.

The value should be one of

  :NONE :LINEAR :INVERSE :EXPONENTIAL

The value may also be a pointer to a C function
of the following signature:
  
  float attenuation(float min,
                    float max,
                    float dist,
                    float roll);

See mixed.h
See SPACE-MIXER"))

;; segments/unpacker.lisp
(docs:define-docs
  (type unpacker
    "This segment converts data from packed-audio to individual sample buffers.

This is mostly useful at the edges to convert from
something like an audio file library to the format
needed by Mixed.

The samplerate argument defines the sample rate
in the output buffers. If it diverges from the
sample rate in the packed-audio, resampling occurs to
account for this.

See SEGMENT
See MAKE-UNPACKER
See *DEFAULT-SAMPLERATE*")
  
  (function packed-audio
    "Reader for the packed-audio the un/packer is translating from/to.

See PACK
See UNPACKER
See PACKER")
  
  (function make-unpacker
    "Create a new unpacker segment.

This automatically creates a packed-audi object
to use. If you prefer to use a packed-audio object
you created yourself, simply use MAKE-INSTANCE
instead.

See PACK
See UNPACKER"))

;; segments/virtual.lisp
(docs:define-docs
  (type virtual
    "Superclass for segments implemented in Lisp.

The segment should implement the following
methods according to its need:

INFO
START
MIX
END
INPUT-FIELD
\(SETF INPUT-FIELD)
OUTPUT-FIELD
\(SETF OUTPUT-FIELD)
FIELD

Default methods for INPUT/OUTPUT-FIELD to
handle the recording of the input/output
buffers already exist. Every other method
by default does nothing. You should in the
very least implement a method for MIX on
your subclass.

See SEGMENT
See INFO
See START
See MIX
See END
See INPUT-FIELD
See OUTPUT-FIELD
See FIELD
See INPUTS
See OUTPUTS"))

;; segments/volume-control.lisp
(docs:define-docs
  (type volume-control
    "A volume control segment that can change the volume and pan.

This is a stereo segment, and as such requires two
input and output buffers each. You may use the
location keywords :LEFT and :RIGHT to address them.

The pan goes from -1.0 for left to 1.0 for right.
The volume goes from 0.0 upwards. Values below zero
result in an error, and values above 1.0 will likely
lead to clipping and thus audio distortions.

See SEGMENT
See MAKE-VOLUME-CONTROL
See VOLUME
See PAN")
  
  (function make-volume-control
    "Create a new volume control segment.

See VOLUME-CONTROL")
  
  (function volume
    "Accessor to the outputting volume of the segment.

Must be in the range [0, infinity[.

See VOLUME-CONTROL
See NOISE
See GENERATOR")
  
  (function pan
    "Accessor to the outputting pan of the volume control segment.

Must be in the range [-1,1].

See VOLUME-CONTROL"))
