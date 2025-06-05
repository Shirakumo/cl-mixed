(defpackage #:org.shirakumo.fraf.mixed.sf3
  (:use #:cl)
  (:shadow #:stream)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:sf3-format-error
   #:bad-header
   #:unsupported-sample-format
   #:file
   #:source
   #:in-memory-source))
(in-package #:org.shirakumo.fraf.mixed.sf3)

(define-condition sf3-format-error (error)
  ((file :initarg :file :initform NIL :accessor file)))

(define-condition bad-header (sf3-format-error)
  ()
  (:report (lambda (c s) (format s "The file~%  ~a~%is not a valid SF3 file: bad header."
                                 (file c)))))

(define-condition unsupported-sample-format (sf3-format-error)
  ((sample-format :initarg :sample-format))
  (:report (lambda (c s) (format s "The file~%  ~a~%contains the sample format ~d which is unsupported."
                                 (file c) (slot-value c 'sample-format)))))

(defun decode-int (stream size)
  (let ((int 0))
    (dotimes (i size int)
      (setf (ldb (byte 8 (* 8 i)) int) (read-byte stream)))))

(defun decode-sf3-header (stream)
  (loop for b across #(#x81 #x53 #x46 #x33 #x00 #xE0 #xD0 #x0D #x0A #x0A #x02)
        do (unless (= b (read-byte stream))
             (error 'bad-header :file (pathname stream))))
  (dotimes (i 4) (read-byte stream))
  (unless (= 0 (read-byte stream))
    (error 'bad-header :file (pathname stream)))
  (let ((samplerate (decode-int stream 4))
        (channels (read-byte stream))
        (sample-format (read-byte stream))
        (frame-count (decode-int stream 8))
        (start (file-position stream)))
    (values channels samplerate
            (case sample-format
              (#x02 :sint16)
              (#x04 :sint32)
              (#x08 :sint64)
              (#x12 :uint16)
              (#x14 :uint32)
              (#x18 :uint64)
              (#x24 :float32)
              (#x28 :float64)
              (T (error 'unsupported-sample-format
                        :file (pathname stream)
                        :sample-format (case sample-format
                                         ((#x01) :alaw)
                                         ((#x11) :ulaw)
                                         (T sample-format)))))
            start (+ end) (* frame-count))))

(defclass source (mixed:pcm-stream-source)
  ((file :initarg :file :accessor file)))

(defmethod initialize-instance :after ((source source) &key file)
  (mixed:start source))

(defmethod mixed:start ((source source))
  (unless (data-stream source)
    (let ((stream (open (file source) :direction :input
                                      :element-type '(unsigned-byte 8))))
      (setf (mixed:data-stream source) stream)
      (multiple-value-bind (channels samplerate encoding start end) (decode-sf3-header stream)
        (setf (mixed:samplerate (mixed:pack source)) samplerate)
        (setf (mixed:channels (mixed:pack source)) channels)
        (setf (mixed:encoding (mixed:pack source)) encoding)
        (setf (mixed:data-start source) start)
        (setf (mixed:data-end source) end)))))

(defclass in-memory-source (mixed:source)
  ())

(defmethod initialize-instance :after ((source in-memory-source) &key file)
  (with-open-file (stream file :direction :input
                               :element-type '(unsigned-byte 8))
    (multiple-value-bind (channels samplerate encoding start end) (decode-sf3-header stream)
      (setf (mixed:samplerate (mixed:pack source)) samplerate)
      (setf (mixed:channels (mixed:pack source)) channels)
      (setf (mixed:encoding (mixed:pack source)) encoding)
      (setf (mixed:framesize source) (mixed:framesize (mixed:pack source)))
      (file-position stream start)
      (let ((buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
        (read-sequence buffer stream)
        (setf (mixed:buffer source) buffer)))))
