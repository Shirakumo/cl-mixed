(in-package #:org.shirakumo.fraf.mixed)

(defclass fader (segment)
  ()
  (:default-initargs
   :from 0.0
   :to 1.0
   :time 1.0
   :type :cubic-in-out
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment fader) &key from to time type samplerate bypass)
  (with-error-on-failure ()
    (mixed:make-segment-fade (float from 0f0) (float to 0f0) (float time 0f0) type samplerate (handle segment)))
  (setf (bypass segment) bypass))

(defun make-fader (&rest args &key from to time type samplerate)
  (declare (ignore from to time type samplerate))
  (apply #'make-instance 'fader args))

(define-field-accessor from fader :float :fade-from)
(define-field-accessor to fader :float :fade-to)
(define-field-accessor duration fader :float :fade-time)
(define-field-accessor fade-type fader mixed:fade-type)
(define-field-accessor bypass fader :bool :bypass)
