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
(define-field-accessor bypass convolution :bool :bypass)
(define-field-accessor wet convolution :float :mix)

(defmethod (setf field) ((value buffer) (field (eql :fir)) (segment convolution))
  (with-error-on-failure nil
    (mixed:segment-set :fir (handle value) (handle segment)))
  value)

(defmethod (setf field) ((value vector) (field (eql :fir)) (segment convolution))
  (check-type value (simple-array single-float (*)))
  (cffi:with-pointer-to-vector-data (ptr value)
    (cffi:with-foreign-objects ((buf '(:struct mixed:buffer)))
      (setf (mixed:buffer-data buf) ptr)
      (setf (mixed:buffer-size buf) (length value))
      (setf (mixed:buffer-read buf) 0)
      (setf (mixed:buffer-write buf) (length value))
      (setf (mixed:buffer-reserved buf) 0)
      (setf (mixed:buffer-virtual-p buf) 0)
      (with-error-on-failure nil
        (mixed:segment-set :fir buf (handle segment)))))
  value)

(defmethod (setf fir) (value (segment convolution))
  (setf (field :fir segment) value))
