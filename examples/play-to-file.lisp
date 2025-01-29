(in-package #:org.shirakumo.fraf.mixed.examples)

(defclass raw-file-drain (mixed:file-drain)
  ((stream :initform NIL :initarg :stream :accessor stream)
   (dont-close :initform NIL :initarg :dont-close :accessor dont-close-p)))

(defmethod mixed:start :before ((drain raw-file-drain))
  (with-slots (mixed:file stream) drain
    (setf stream
          (or stream
              (open mixed:file :direction :output
                               :element-type '(unsigned-byte 8))))))

(defmethod mixed:mix ((drain raw-file-drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (< 0 size)
      (write-sequence data (stream drain) :start start :end (+ start size))
      (incf (mixed:frame-position drain)
            (/ size (mixed:framesize (mixed:pack drain))))
      (mixed:finish size))))

(defmethod mixed:end :after ((drain raw-file-drain))
  (when (and (stream drain)
             (not (dont-close-p drain)))
    (close (stream drain))
    (setf (stream drain) NIL)))

(defmethod mixed:free :after ((drain raw-file-drain))
  (when (and (stream drain)
             (not (dont-close-p drain)))
    (close (stream drain) :abort T)
    (setf (stream drain) NIL)))

(defun play-to-file (file &key output output-stream
                               (samplerate 44100) (encoding :float)
                               (input-format :mp3) (output-format :wav))
  (mixed:init)
  (mixed:with-objects ((source (mixed:make-unpacker :samplerate samplerate))
                       (drain (mixed:make-packer :samplerate samplerate :encoding encoding))
                       (in (make-instance (ecase input-format
                                            (:mp3 'org.shirakumo.fraf.mixed.mpg123:source)
                                            (:wav 'org.shirakumo.fraf.mixed.wav:in-memory-source))
                                          :file (pathname file) :pack source))
                       (out (make-instance (ecase output-format
                                             (:wav 'org.shirakumo.fraf.mixed.wav:file-drain)
                                             (:raw 'raw-file-drain))
                                           :file (pathname output) :pack drain
                                           :stream output-stream :dont-close output-stream)))
    (mixed:with-buffers 500 (l r)
      (mixed:connect source :left drain :left l)
      (mixed:connect source :right drain :right r)
      (mixed:with-chain chain (in source drain out)
        (format T "~&Writing a file with ~d channels @ ~dHz, ~a~%"
                (mixed:channels drain) (mixed:samplerate drain) (mixed:encoding drain))
        (loop until (mixed:done-p in)
              do (mixed:mix chain))
        (format T "Wrote ~d frames." (mixed:frame-position out))))))
