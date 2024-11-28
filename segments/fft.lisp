(in-package #:org.shirakumo.fraf.mixed)

(defclass fwd-fft (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment fwd-fft) &key samplerate framesize oversampling)
  (with-error-on-failure ()
    (mixed:make-segment-fwd-fft samplerate (handle segment)))
  (when framesize (setf (framesize segment) framesize))
  (when oversampling (setf (oversampling segment) oversampling)))

(defun make-fwd-fft (&rest args &key samplerate framesize oversampling)
  (declare (ignore samplerate framesize oversampling))
  (apply #'make-instance 'fwd-fft args))

(define-field-accessor samplerate fwd-fft :uint32 :samplerate)
(define-field-accessor framesize fwd-fft :uint32 :framesize)
(define-field-accessor oversampling fwd-fft :uint32 :oversampling)

(defclass inv-fft (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment inv-fft) &key samplerate framesize oversampling)
  (with-error-on-failure ()
    (mixed:make-segment-inv-fft samplerate (handle segment)))
  (when framesize (setf (framesize segment) framesize))
  (when oversampling (setf (oversampling segment) oversampling)))

(defun make-inv-fft (&rest args &key samplerate framesize oversampling)
  (declare (ignore samplerate framesize oversampling))
  (apply #'make-instance 'inv-fft args))

(define-field-accessor samplerate inv-fft :uint32 :samplerate)
(define-field-accessor framesize inv-fft :uint32 :framesize)
(define-field-accessor oversampling inv-fft :uint32 :oversampling)
