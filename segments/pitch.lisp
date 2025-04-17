(in-package #:org.shirakumo.fraf.mixed)

(defclass pitch (segment)
  ()
  (:default-initargs
   :pitch 1.0
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment pitch) &key pitch samplerate (bypass NIL bypass-p) wet)
  (with-error-on-failure ()
    (mixed:make-segment-pitch (float pitch 0f0) samplerate (handle segment)))
  (when wet (setf (wet segment) wet))
  (when bypass-p (setf (bypass segment) bypass)))

(defun make-pitch (&rest args &key pitch samplerate)
  (declare (ignore pitch samplerate))
  (apply #'make-instance 'pitch args))

(define-field-accessor pitch pitch :float :pitch-shift)
(define-field-accessor samplerate pitch :uint32 :samplerate)
(define-field-accessor bypass pitch :bool :bypass)
(define-field-accessor wet pitch :float :mix)
