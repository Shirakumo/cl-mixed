(in-package #:org.shirakumo.fraf.mixed)

(defclass convolution (segment)
  ()
  (:default-initargs
   :framesize 2048
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((space convolution) &key framesize fir samplerate)
  (with-error-on-failure ()
    (etypecase fir
      ((simple-array single-float (*))
       (cffi:with-pointer-to-vector-data (ptr fir)
         (mixed:make-segment-convolution framesize ptr (length fir) samplerate (handle space))))
      (buffer
       (let ((ptr (static-vectors:static-vector-pointer (data fir))))
         (mixed:make-segment-convolution framesize ptr (size fir) samplerate (handle space)))))))

(defun make-convolution (fir &key (framesize 2048) (samplerate *default-samplerate*))
  (make-instance 'convolution :fir fir :framesize framesize :samplerate samplerate))

(define-field-accessor samplerate convolution :uint32 :samplerate)
(define-field-accessor framesize convolution :uint32 :framesize)

