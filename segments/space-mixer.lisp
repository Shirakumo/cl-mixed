(in-package #:org.shirakumo.fraf.mixed)

(defclass space-mixer (mixer)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((space space-mixer) &key samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-space-mixer samplerate (handle space))))

(defun make-space-mixer (&rest args &key (samplerate *default-samplerate*) up soundspeed doppler-factor min-distance max-distance rolloff attenuation)
  (declare (ignore up soundspeed doppler-factor min-distance max-distance rolloff attenuation))
  (let ((instance (make-instance 'space-mixer :samplerate samplerate)))
    (loop for (field value) on args by #'cddr
          do (unless (eql field :samplerate)
               (setf (field field instance) value)))
    instance))

(defmethod channels ((mixer space-mixer)) 1)

(define-vector-field-accessor location space-mixer :space-location)
(define-vector-field-accessor velocity space-mixer :space-velocity)
(define-vector-field-accessor direction space-mixer :space-direction)
(define-vector-field-accessor up space-mixer :space-up)

(define-input-vector-field-accessor input-location space-mixer :location :space-location)
(define-input-vector-field-accessor input-velocity space-mixer :velocity :space-velocity)
(define-input-field-accessor input-min-distance space-mixer :min-distance :space-min-distance :float)
(define-input-field-accessor input-max-distance space-mixer :max-distance :space-max-distance :float)
(define-input-field-accessor input-rolloff space-mixer :rolloff :space-rolloff :float)

(define-field-accessor soundspeed space-mixer :float :space-soundspeed)
(define-field-accessor doppler-factor space-mixer :float :space-doppler-factor)
(define-field-accessor min-distance space-mixer :float :space-min-distance)
(define-field-accessor max-distance space-mixer :float :space-max-distance)
(define-field-accessor rolloff space-mixer :float :space-rolloff)
(define-field-accessor volume space-mixer :float :volume)
(define-field-accessor channel-count-out space-mixer mixed:channel_t)
;; TODO: (define-field-accessor channel-configuration space-mixer)

(defmethod field ((field (eql :attenuation)) (segment space-mixer))
  (cffi:with-foreign-object (value-ptr :pointer)
    (with-error-on-failure ()
      (mixed:segment-get :space-attenuation value-ptr (handle segment)))
    (loop with int = (cffi:mem-ref value-ptr :int)
          for keyword in (cffi:foreign-enum-keyword-list 'mixed:attenuation)
          do (when (= int (cffi:foreign-enum-value 'mixed:attenuation keyword))
               (return keyword))
          finally (return (cffi:mem-ref value-ptr :pointer)))))

(defmethod (setf field) (value (field (eql :attenuation)) (segment space-mixer))
  (cffi:with-foreign-object (value-ptr :pointer)
    (etypecase value
      (keyword
       (setf (cffi:mem-ref value-ptr 'mixed:size_t)
             (cffi:foreign-enum-value 'mixed:attenuation value)))
      (cffi:foreign-pointer
       (setf (cffi:mem-ref value-ptr :pointer) value)))
    (with-error-on-failure ()
      (mixed:segment-set :space-attenuation value-ptr (handle segment))))
  value)

(defmethod attenuation ((space space-mixer))
  (field :attenuation space))

(defmethod (setf attenuation) (value (space space-mixer))
  (setf (field :attenuation space) value))

(defmethod add ((new segment) (segment space-mixer))
  (setf (input-field :buffer T segment) (output 0 new))
  new)

(defmethod withdraw ((old segment) (segment space-mixer))
  (setf (input-field :buffer (position (output 0 old) (inputs segment)) segment) NIL)
  old)
