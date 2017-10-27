## About cl-mixed
This is a bindings library to [libmixed](https://github.com/Shirakumo/libmixed), an audio mixing and processing library.

## How To
Precompiled versions of the underlying library are included in this. If you want to build it manually however, refer to the [libmixed](https://github.com/Shirakumo/libmixed) repository.

First, set up your input and output parts such as file decoders and audio systems. Handling this is not a part of libmixed. To see an example of how to incorporate them, see the [test.lisp](test.lisp) file.

Load the system through ASDF or Quicklisp:

    (ql:quickload :cl-mixed)

Now you'll need to integrate your inputs and outputs with the Mixed system. In order to do that, you'll want an unpacker and a packer segment. Both of those create "packed-audio", which holds all the information about how the raw audio data is encoded in a byte buffer.

    (cl-mixed:make-unpacker c-buffer bytes sample-encoding channel-count channel-layout samplerate)

If you don't already have a byte buffer from your input or output implementation, passing NIL for the `c-buffer` will automatically create one for you, which you can then access with `data`. An example call might look like this:

    (cl-mixed:make-unpacker NIL 4096 :int16 2 :alternating 44100)

An optional third parameter designates the sample rate of the buffers that the source converts to or the drain converts from. This "buffer sample rate" has to be the same across all segments in a mixer pipeline. It defaults to 44100. Creating a packer looks and works exactly the same as an unpacker.

Next you'll want to create the segments that'll do the actual audio processing you want. For this example, let's create a 3D audio segment (`space-mixer`) and a fade effect (`fade`).

    (cl-mixed:make-space-mixer)
    (cl-mixed:make-fade :duration 5.0)

Next we'll need to create the buffers that are used to manipulate the audio internally and bind them to the appropriate inputs and outputs of our segments.

    (cl-mixed:with-buffers 500 (input left right)
      (cl-mixed:connect unpacker :left fade :mono input)
      (setf (cl-mixed:output :right unpacker) right)
      (cl-mixed:connect fade :mono space-mixer 0 input)
      (cl-mixed:connect space-mixer :left packer :left left)
      (cl-mixed:connect space-mixer :right packer :right right)
      ...)

This here means we create three buffers, `input`, `left`, and `right`, each with a size capable of holding 500 samples. We then connect the source's left output to the fade's single input. Then we connect the right buffer to the unpacker's right output, just so that it has both outputs set. If your unpacker only has one channel, you can leave that out. If it has more, you'll have to repeat it for the other channels as well. Next we connect the fade's output as the space-mixer's first input. Finally we connect the left and right outputs of the space-mixer segment to the left and right inputs of the packer respectively. For the fade segment we can connect the same buffer to both input and output, as it is declared to work "in place". For the space-mixer segment we need distinct buffers, hence the extra `input` buffer.

Now we can create our segment-sequence object, which keeps the order in which to process each segment.

    (cl-mixed:make-segment-sequence source fade space drain)

Finally we can move to the main processing loop, which should look as follows:

    (cl-mixed:start segment-sequence)
    (unwind-protect
        (loop while has-more
              do (process-source)
                 (cl-mixed:mix 500 segment-sequence)
                 (process-drain))
      (cl-mixed:end segment-sequence))

Where `process-source` and `process-drain` are functions that will cause your source to put samples into its buffer and drain to read out the samples from its buffer. Running this now will just give you a fade in effect, which isn't too exciting. Since we haven't actually set or changed any of the 3D audio parameters, that effect remains inaudible. Changing the loop body to read something like the following

    for tt = 0 then (+ tt 0.001)
    for dx = 0 then (- (* 100 (sin tt)) x)
    for dz = 0 then (- (* 50 (cos tt)) z)
    for x = (* 100 (sin tt)) then (+ x dx)
    for z = (* 50 (cos tt)) then (+ z dz)
    do (setf (cl-mixed:input-field :location 0 space-mixer) (list x 0 z))
       (setf (cl-mixed:input-field :velocity 0 space-mixer) (list dx 0 dz))
       (process-source)
       (cl-mixed:mix 500 segment-sequence)
       (process-drain)

Should cause the source to now also circle around the listener as it is fading in. If you change the `tt` change factor from `0.001` to something higher, it will circle faster, and the doppler effect should become more noticeable.

The following segments are included with the standard libmixed distribution:

* `basic-mixer` Linearly mix multiple inputs.
* `fade` Fade the volume of a source in or out.
* `generator` Generate simple wave forms.
* `ladspa` Use a LADSPA plugin.
* `space-mixer` Mix multiple inputs as if they were in 3D space.
* `volume-control` Adapt the volume and pan of a stereo signal.

See the next section on how to make custom segments.

## Creating Custom Segments
While it is perfectly possible to create custom segments in C and load them into your lisp image, you can also write them directly in CL. This may be desired if performance is not crucial or if you want to quickly prototype an effect before translating it to a lower-level language.

In order to create a segment in Lisp, you must subclass `virtual` and in the very least implement the `mix` method for it. Here's an example for a very primitive echo effect:

    (defclass echo (cl-mixed:virtual)
      ((buffer :initform NIL :accessor buffer)
       (offset :initform 0 :accessor offset)
       (delay :initarg :delay :initform 0.2 :accessor delay)
       (falloff :initarg :falloff :initform 0.8 :accessor falloff)
       (samplerate :initarg :samplerate :initform 44100 :accessor samplerate)))
    
    (defmethod cl-mixed:start ((echo echo))
      (setf (buffer echo) (make-array (ceiling (* (delay echo) (samplerate echo)))
                                      :element-type 'single-float
                                      :initial-element 0.0s0)))
    
    (defmethod cl-mixed:mix (samples (echo echo))
      (let ((out (cl-mixed:data (aref (cl-mixed:outputs echo) 0)))
            (in (cl-mixed:data (aref (cl-mixed:inputs echo) 0)))
            (buf (buffer echo))
            (offset (offset echo))
            (falloff (falloff echo)))
        (loop for i from 0 below samples
              for sample = (cffi:mem-aref in :float i)
              for echo = (aref buf offset)
              do (setf (cffi:mem-aref out :float i) (+ sample echo))
                 (setf (aref buf offset) (* (+ sample echo) falloff))
                 (setf offset (mod (1+ offset) (length buf))))
        (setf (offset echo) offset)))

In order to achieve the echo effect we keep samples of a given duration around in a ring buffer and then decrease their potency with each iteration while adding the new samples on top. Of course, a more natural sounding echo effect would need more complicated processing than this. Regardless, this segment can now be integrated just the same as the `fade` segment from the above introductory code.

## Also See

* [CFFI](https://common-lisp.net/project/cffi/manual/cffi-manual.html)
* [libmixed](https://github.com/Shirakumo/libmixed/)
