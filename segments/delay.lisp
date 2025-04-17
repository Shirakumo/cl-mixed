(in-package #:org.shirakumo.fraf.mixed)

(defclass delay (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment delay) &key time samplerate (bypass NIL bypass-p))
  (with-error-on-failure ()
    (mixed:make-segment-delay (float time 0f0) samplerate (handle segment)))
  (when bypass-p (setf (bypass segment) bypass)))

(defun make-delay (&rest args &key time samplerate)
  (declare (ignore time samplerate))
  (apply #'make-instance 'delay args))

(define-field-accessor duration delay :float :delay-time)
(define-field-accessor samplerate delay :float :samplerate)
(define-field-accessor bypass delay :bool :bypass)
