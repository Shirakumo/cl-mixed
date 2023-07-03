(in-package #:org.shirakumo.fraf.mixed)

(defclass spatial-reverb (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((space spatial-reverb) &key samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-spatial-reverb samplerate (handle space))))

(defun make-spatial-reverb (&key (samplerate *default-samplerate*) distance-delay max-distance)
  (let ((instance (make-instance 'spatial-reverb :samplerate samplerate)))
    (when max-distance (setf (max-distance instance) max-distance))
    (when distance-delay (setf (distance-delay instance) distance-delay))
    instance))

(define-field-accessor max-distance spatial-reverb :float :spatial-reverb-max-distance-cutoff)
(define-field-accessor distance-delay spatial-reverb :float :spatial-reverb-distance-delay)

(defmethod set-parameters ((segment spatial-reverb) distances hit-ratios absorption-rates)
  (with-foreign-object (value :float 12)
    (let ((i -1))
      (flet ((copy (source)
               (dotimes (j 4)
                 (setf (cffi:mem-aref value :float (incf i)) (aref source j)))))
        (copy distances)
        (copy hit-ratios)
        (copy absorption-rates)))
    (with-error-on-failure ()
      (mixed:segment-set :spatial-reverb-parameters value (handle segment)))))

(defmethod add-spatial-probe ((segment spatial-reverb) angle length absorption-rate)
  (with-foreign-object (value :float 3)
    (setf (cffi:mem-aref value :float 0) angle)
    (setf (cffi:mem-aref value :float 1) length)
    (setf (cffi:mem-aref value :float 2) absorption-rate)
    (with-error-on-failure ()
      (mixed:segment-set :spatial-reverb-probe value (handle segment)))))
