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

For raw data buffers this number is in bytes.

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
    "Channels represent an interface to an outside sound source or drain.

The channel holds all the necessary information to describe
the audio data present in a raw byte buffer. This includes
how many channels there are, how the samples are laid out
and how the samples are formatted in memory. It also includes
the samplerate of the channel's source so that it can be
converted if necessary.

See MAKE-CHANNEL
See SOURCE
See DRAIN
See C-OBJECT
See OWN-DATA
See DATA
See SIZE
See ENCODING
See CHANNELS
See LAYOUT
See SAMPLERATE")
  
  (function own-data
    "Reader for a cons cell that holds information about the buffer in the channel.

If the cons cell stores NIL in its car, then the data buffer
is not owned by the channel and may not be freed by the
library. If it is T, the cdr must be the pointer to the
buffer data. This data is freed when the channel is GCed.

See CHANNEL")
  
  (function make-channel
    "Create a new channel object.

If DATA is NIL, then a new data buffer is allocated with
SIZE number of bytes. This data buffer will be automatically
freed whenever the channel is freed.

See CHANNEL")
  
  (function encoding
    "Accessor to the sample encoding of the raw data buffer in the object.

The encoding has to be one of the following:
 :INT8 :UINT8 :INT16 :UINT16 :INT24 :UINT24 :INT32 :UINT32
 :FLOAT :DOUBLE

See CHANNEL")
  
  (function channels
    "Accessor to the number of channels encoded in the data buffer in the object.

See CHANNEL")
  
  (function layout
    "Accessor to the channel layout in which the samples are laid out in the buffer of the object.

The layout has to be either :ALTERNATING or :SEQUENTIAL.
Alternating means that the samples are encoded like this:
 (C₁C₂...)ⁿ
Sequential means that the samples are encoded like this:
 C₁ⁿC₂ⁿ...

See CHANNEL")
  
  (function samplerate
    "Accessor to the samplerate at which the samples are expected to be.

The sample rate is in Hz.

See CHANNEL"))

;; mixer.lisp
(docs:define-docs
  (type mixer
    "A mixer represents a mixing pipeline.

The mixer takes care of remembering the order in which
segments should be processed and running them in that
order when mixing is done.

See MAKE-MIXER
See C-OBJECT
See SEGMENTS
See ADD
See WITHDRAW
See START
See MIX
See END")
  
  (function segments
    "Accessor to a vector of segments the mixer contains.

This vector will become out of date if the mixer's
segments are added or removed from the C side
directly, or directly through this vector. Thus you
should never modify this directly and instead always
make sure to go through ADD/WITHDRAW.

See MIXER
See ADD
See WITHDRAW")
  
  (function make-mixer
    "Create a new mixer instance.

The segments given are added to the mixer in order.

See MIXER")
  
  (function add
    "Add the object to the given collection.

For mixers this means adding a segment to the mixer.
For many-inputs-segments this means adding another
input buffer.

See MIXER
See MANY-INPUTS-SEGMENT")
  
  (function withdraw
    "Remove the object from the given collection.

For mixers this means removing the segment from the
mixer. For many-inputs-segments this means removing
the given input buffer.

See MIXER
See MANY-INPUTS-SEGMENT")
  
  (function start
    "Start the mixing process.

This method should be called as close as possible
to the next calls to MIX. Calling MIX before
START is called or after END is called is an error.
After START has been called, changing some segments'
fields may result in undefined behaviour and might
even lead to crashes.

See MIXER
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
the segments in the mixer can hold is an error and
may crash your system. No checks for this problem
are done.

Calling MIX before START has been called or after
END has been called is an error and may result in
crashes. No checks for this problem are done.

If you want to ensure that the mixer is complete
and able to process the requested number of samples,
you should call CHECK-COMPLETE after running START.

See MIXER
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

See MIXER
See SEGMENT
See START
See MIX"))

;; segment.lisp
(docs:define-docs
  (function decode-flags
    "Decode an OR combined integer of INFO-FLAGS to a list of keywords for the flags.

See CL-MIXED-CFFI:INFO-FLAGS")
  
  (function encode-flags
    "Encode the list of keywords for the INFO-FLAGS to a an OR combined integer.

See CL-MIXED-CFFI:INFO-FLAGS")
  
  (function decode-field-info
    "Decode the field info contained in the pointer to a segment-info struct.

See CL-MIXED-CFFI:SEGMENT-INFO
See CL-MIXED-CFFI:FIELD-INFO")
  
  (function encode-field-info
    "Encode the field info into the segment-info struct pointed to by the pointer.

See CL-MIXED-CFFI:SEGMENT-INFO
See CL-MIXED-CFFI:FIELD-INFO")
  
  (type segment
    "Superclass for all mixing pipeline segments.

A segment is responsible for producing, consuming,
combining, splitting, or just in general somehow
processing audio data within a mixer piepeline.

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

See SEGMENT")
  
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
See SEGMENT")
  
  (type many-inputs-segment
    "Superclass for segments that accept a variable number of input segments.

See SEGMENT
See ADD
See WITHDRAW")
  
  (type source
    "This segment converts data from a channel to individual sample buffers.

This is mostly useful at the edges to convert from
something like an audio file library to the format
needed by Mixed.

The samplerate argument defines the sample rate
in the output buffers. If it diverges from the
sample rate in the channel, resampling occurs to
account for this. To change the resampling method,
use the :RESAMPLER method. The value must be a
pointer to a C function of the following signature:

  int resample(struct mixed_buffer *in,
               size_t in_samplerate,
               struct mixed_buffer *out,
               size_t out_samplerate,
               size_t out_samples)

Three such resampling functions are available out
of the box:

- CL-MIXED-CFFI:RESAMPLE-NEAREST
- CL-MIXED-CFFI:RESAMPLE-LINEAR
- CL-MIXED-CFFI:RESAMPLE-CUBIC

See CHANNEL
See SEGMENT
See MAKE-SOURCE
See *DEFAULT-SAMPLERATE*")
  
  (function channel
    "Reader for the channel the source/drain is translating from/to.

See CHANNEL
See SOURCE
See DRAIN")
  
  (function make-source
    "Create a new source segment.

This automatically creates a channel object to use.
If you prefer to use a channel object you created
yourself, simply use MAKE-INSTANCE instead.

See CHANNEL
See SOURCE")
  
  (type drain
    "This segment converts data from individual sample buffers to data for a channel.

This is mostly useful at the edges to convert to
something like an audio file library or audio
playback system from the internal buffers as used
by Mixed.

The samplerate argument defines the sample rate
in the input buffers. If it diverges from the
sample rate in the channel, resampling occurs to
account for this. To change the resampling method,
use the :RESAMPLER method. The value must be a
pointer to a C function of the following signature:

  int resample(struct mixed_buffer *in,
               size_t in_samplerate,
               struct mixed_buffer *out,
               size_t out_samplerate,
               size_t out_samples)

Three such resampling functions are available out
of the box:

- CL-MIXED-CFFI:RESAMPLE-NEAREST
- CL-MIXED-CFFI:RESAMPLE-LINEAR
- CL-MIXED-CFFI:RESAMPLE-CUBIC

See mixed.h
See CHANNEL
See SEGMENT
See MAKE-SOURCE
See *DEFAULT-SAMPLERATE*")
  
  (function make-drain
    "Create a new drain segment.

This automatically creates a channel object to use.
If you prefer to use a channel object you created
yourself, simply use MAKE-INSTANCE instead.

See CHANNEL
See DRAIN")
  
  (type linear-mixer
    "This segment linearly mixes an arbitrary number of inputs to a single output.

Linear mixing means that all the inputs are summed
up and the resulting number is divided by the number
of inputs. This is equivalent to having all the
inputs play as \"individual speakers\" in real life.

See MANY-INPUTS-SEGMENT
See MAKE-LINEAR-MIXER")
  
  (function make-linear-mixer
    "Create a new linear mixer, adding the given buffers as inputs.

See LINEAR-MIXER
See ADD
See WITHDRAW")
  
  (type general
    "A \"general operations\" segment that can change the volume and pan.

This is a stereo segment, and as such requires two
input and output buffers each. You may use the
location keywords :LEFT and :RIGHT to address them.

The pan goes from -1.0 for left to 1.0 for right.
The volume goes from 0.0 upwards. Values below zero
result in an error, and values above 1.0 will likely
lead to clipping and thus audio distortions.

See SEGMENT
See MAKE-GENERAL
See VOLUME
See PAN")
  
  (function make-general
    "Create a new \"general options\" segment.

See GENERAL")
  
  (function volume
    "Accessor to the outputting volume of the general segment.

Must be in the range [0, infinity[.

See GENERAL")
  
  (function pan
    "Accessor to the outputting pan of the general segment.

Must be in the range [-1,1].

See GENERAL")
  
  (type fade
    "A volume fading segment.

This allows you to smoothly fade in and out an input.

The from and to are given in relative volume, meaning
in the range of [0.0, infinity[. The duration is given
in seconds. The fade type must be one of the following:
:LINEAR :CUBIC-IN :CUBIC-OUT :CUBIC-IN-OUT, each
referring to the respective easing function.
The time is measured from the last call to START out.

See SEGMENT
See MAKE-FADE
See FROM
See TO
See DURATION
See FADE-TYPE")
  
  (function make-fade
    "Create a new volume fader segment.

See FADE")
  
  (function from
    "Accessor to the starting volume of the fading segment.

Must be in the range of [0.0, infinity[.

See FADE")
  
  (function to
    "Accessor to the ending volume of the fading segment.

Must be in the range of [0.0, infinity[.

See FADE")
  
  (function duration
    "Accessor to the duration of the fade effect.

Must be measured in seconds.

See FADE")
  
  (function fade-type
    "Accessor to the type of easing function used for the fade.

Must be one of :LINEAR :CUBIC-IN :CUBIC-OUT :CUBIC-IN-OUT

See FADE")
  
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

See GENERATOR")
  
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

See LADSPA")
  
  (type space
    "A segment to simulate audio effects in a 3D space.

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

See MANY-INPUTS-SEGMENT
See MAKE-SPACE
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
  
  (function make-space
    "Create a new space segment for 3D audio processing.

See SPACE")
  
  (function location
    "Accessor for the location of the listener in space.

The value should be a list of three floats.

See SPACE")
  
  (function velocity
    "Accessor for the velocity of the listener in space.

The value should be a list of three floats.

See SPACE")
  
  (function direction
    "Accessor for the direction the listener is facing in space.

The value should be a list of three floats.

See SPACE")
  
  (function up
    "Accessor for the vector representing \"upwards\" in space.

The value should be a list of three floats.

See SPACE")
  
  (function input-location
    "Accessor for the location of the source in space.

The value should be a list of three floats.

See SPACE")
  
  (function input-velocity
    "Accessor for the velocity of the source in space.

The value should be a list of three floats.

See SPACE")
  
  (function soundspeed
    "Accessor to the speed of sound in space.

This value only influences the strength of
the doppler factor.

See SPACE")
  
  (function doppler-factor
    "Accessor to the over-/under-statement factor of the doppler effect.

See SPACE")
  
  (function min-distance
    "Accessor to the minimal distance below which the source is at max volume.

See SPACE")
  
  (function max-distance
    "Accessor to the maximal distance above which the source is inaudible.

See SPACE")
  
  (function rolloff
    "Accessor to the rolloff factor that describes the curvature of the attenuation function.

See SPACE")

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
See SPACE")
  
  (type virtual
    "Superclass for segments implemented in Lisp.

The segment should implement the following
methods according to its need:

INFO
START
MIX
END
INPUT-FIELD
OUTPUT-FIELD
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

See CL-MIXED-CFFI:ERROR
See CL-MIXED-CFFI:ERROR-STRING")
  
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
  
  (function define-field-accessor
    "Define an accessor for a segment's field.

Generates the necessary methods on FIELD as well as
convenience wrapper methods.")
  
  (function define-vector-field-accessor
    "Define an accessor for a segment's vector value field.

Generates the necessary methods on FIELD as well as
convenience wrapper methods. The values should be
lists of three floats.")
  
  (function define-input-vector-field-accessor
    "Define an accessor for a segment's input vector value field.

Generates the necessary methods on FIELD as well as
convenience wrapper methods. The values should be
lists of three floats.")
  
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
