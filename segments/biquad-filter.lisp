(in-package #:org.shirakumo.fraf.mixed)

(defclass biquad-filter (segment)
  ()
  (:default-initargs
   :filter :lowpass
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment biquad-filter) &key filter samplerate (frequency (1- samplerate)) (bypass NIL bypass-p) q gain)
  (with-error-on-failure ()
    (mixed:make-segment-biquad-filter filter (float frequency 0f0) samplerate (handle segment)))
  (when q (setf (q segment) q))
  (when gain (setf (gain segment) gain))
  (when bypass-p (setf (bypass segment) bypass)))

(defun make-biquad-filter (&rest args &key pass cutoff samplerate)
  (declare (ignore pass cutoff samplerate))
  (apply #'make-instance 'biquad-filter args))

(define-field-accessor frequency biquad-filter :float :frequency)
(define-field-accessor filter biquad-filter mixed:biquad-filter :biquad-filter)
(define-field-accessor q biquad-filter :float :q)
(define-field-accessor gain biquad-filter :float :gain)
(define-field-accessor samplerate biquad-filter :uint32 :samplerate)
(define-field-accessor bypass biquad-filter :bool :bypass)
