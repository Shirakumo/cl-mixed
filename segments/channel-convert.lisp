(in-package #:org.shirakumo.fraf.mixed)

(defclass channel-convert (segment)
  ()
  (:default-initargs
   :in (error "IN required.")
   :out (error "OUT required.")
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment channel-convert) &key in out samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-channel-convert in out samplerate (handle segment))))

(defun make-channel-convert (&rest args &key in out samplerate)
  (declare (ignore in out samplerate))
  (apply #'make-instance 'channel-convert args))

(define-field-accessor channel-count-in channel-convert mixed:channel_t)
(define-field-accessor channel-count-out channel-convert mixed:channel_t)

(defmethod (setf field) :after (value (field (eql :channel-count-in)) (segment channel-convert))
  (setf (slot-value segment 'inputs) (adjust-array (inputs segment) value :initial-element NIL))
  (setf (getf (direct-info segment) :max-inputs) value)
  (setf (getf (direct-info segment) :min-inputs) value))

(defmethod (setf field) :after (value (field (eql :channel-count-out)) (segment channel-convert))
  (setf (slot-value segment 'outputs) (adjust-array (outputs segment) value :initial-element NIL))
  (setf (getf (direct-info segment) :outputs) value))
