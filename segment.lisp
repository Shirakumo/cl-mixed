#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defun decode-flags (integer)
  (etypecase integer
    (list integer)
    (integer
     (loop for flag in (cffi:foreign-enum-keyword-list 'cl-mixed-cffi:info-flags)
           when (/= 0 (logand integer (cffi:foreign-enum-value 'cl-mixed-cffi:info-flags flag)))
           collect flag))))

(Defun encode-flags (flags)
  (etypecase flags
    (integer flags)
    (list
     (loop with integer = 0
           for flag in flags
           do (setf integer (logior integer (cffi:foreign-enum-value 'cl-mixed-cffi:info-flags flag)))
           finally (return integer)))))

(defun decode-field-info (info)
  (loop for field = (cffi:foreign-slot-pointer info '(:struct cl-mixed-cffi:segment-info) 'cl-mixed-cffi::fields)
        then (cffi:inc-pointer field (cffi:foreign-type-size '(:struct cl-mixed-cffi:field-info)))
        repeat 32
        until (cl-mixed-cffi:field-info-flags field)
        collect (list :field (cl-mixed-cffi:field-info-field field)
                      :description (cl-mixed-cffi:field-info-description field)
                      :flags (decode-flags
                              (cl-mixed-cffi:field-info-flags field)))))

(defun encode-field-info (fields info)
  (loop for field = (cffi:foreign-slot-pointer info '(:struct cl-mixed-cffi:segment-info) 'cl-mixed-cffi::fields)
        then (cffi:inc-pointer field (cffi:foreign-type-size '(:struct cl-mixed-cffi:field-info)))
        for fieldspec in fields
        do (destructuring-bind (&key fieldno description flags) fieldspec
             (setf (cl-mixed-cffi:field-info-field field) fieldno)
             (setf (cl-mixed-cffi:field-info-description field) description)
             (setf (cl-mixed-cffi:field-info-flags field) (encode-flags flags)))))

;; See the comment on the mixer class for an explanation on the arrays.
(defclass segment (c-object)
  ((inputs :initform (make-array 0 :adjustable T :fill-pointer T) :reader inputs)
   (outputs :initform (make-array 0 :adjustable T :fill-pointer T) :reader outputs)
   (info :initform NIL :accessor direct-info)))

(defmethod info ((segment segment))
  (unless (direct-info segment)
    (let ((info (cl-mixed-cffi:segment-info (handle segment))))
      (when (cffi:null-pointer-p info)
        (error "Failed to retrieve information about ~a" segment))
      (setf (direct-info segment)
            (list :name (cl-mixed-cffi:segment-info-name info)
                  :description (cl-mixed-cffi:segment-info-description info)
                  :flags (decode-flags (cl-mixed-cffi:segment-info-flags info))
                  :min-inputs (cl-mixed-cffi:segment-info-min-inputs info)
                  :max-inputs (cl-mixed-cffi:segment-info-max-inputs info)
                  :outputs (cl-mixed-cffi:segment-info-outputs info)
                  :fields (decode-field-info info)))
      (cffi:foreign-free info)))
  (direct-info segment))

(defmethod start ((segment segment))
  (cl-mixed-cffi:segment-start (handle segment)))

(defmethod mix (samples (segment segment))
  (cl-mixed-cffi:segment-mix samples (handle segment)))

(defmethod end ((segment segment))
  (cl-mixed-cffi:segment-end (handle segment)))

(defmethod allocate-handle ((segment segment))
  (calloc '(:struct cl-mixed-cffi:segment)))

(defmethod free-handle ((segment segment) handle)
  (lambda ()
    (cl-mixed-cffi:free-segment handle)
    (cffi:foreign-free handle)
    (setf (pointer->object handle) NIL)))

(defmethod (setf input-field) (value field location segment)
  (etypecase value
    (cffi:foreign-pointer
     (with-error-on-failure ()
       (cl-mixed-cffi:segment-set-in field location value (handle segment))))))

(defmethod (setf output-field) (value field location segment)
  (etypecase value
    (cffi:foreign-pointer
     (with-error-on-failure ()
       (cl-mixed-cffi:segment-set-out field location value (handle segment)))))
  value)

(defmethod input-field ((field (eql :buffer)) location (segment segment))
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get-in field location ptr segment))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) location (segment segment))
  (let ((location (etypecase location
                    ((integer 0) location)
                    (keyword (cffi:foreign-enum-value 'cl-mixed-cffi:location location)))))
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-set-in field location (handle value) (handle segment)))
    (vector-insert-pos location value (inputs segment)))
  value)

(defmethod (setf input-field) ((value null) (field (eql :buffer)) location (segment segment))
  (let ((location (etypecase location
                    ((integer 0) location)
                    (keyword (cffi:foreign-enum-value 'cl-mixed-cffi:location location)))))
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-set-in field location (cffi:null-pointer) (handle segment)))
    (vector-remove-pos location (inputs segment)))
  value)

(defmethod output-field ((field (eql :buffer)) location (segment segment))
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get-out field location ptr segment))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf output-field) ((value buffer) (field (eql :buffer)) location (segment segment))
  (let ((location (etypecase location
                    ((integer 0) location)
                    (keyword (cffi:foreign-enum-value 'cl-mixed-cffi:location location)))))
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-set-out field location (handle value) (handle segment)))
    (vector-insert-pos location value (outputs segment)))
  value)

(defmethod (setf output-field) ((value null) (field (eql :buffer)) location (segment segment))
  (let ((location (etypecase location
                    ((integer 0) location)
                    (keyword (cffi:foreign-enum-value 'cl-mixed-cffi:location location)))))
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-set-out field location (cffi:null-pointer) (handle segment)))
    (vector-remove-pos location (outputs segment)))
  value)

(defmethod (setf field) (value field (segment segment))
  (etypecase value
    (cffi:foreign-pointer
     (with-error-on-failure ()
       (cl-mixed-cffi:segment-set field value (handle segment)))))
  value)

(defmethod input (location (segment segment))
  (input-field :buffer location segment))

(defmethod (setf input) (buffer location (segment segment))
  (setf (input-field :buffer location segment) buffer))

(defmethod output (location (segment segment))
  (output-field :buffer location segment))

(defmethod (setf output) (buffer location (segment segment))
  (setf (output-field :buffer location segment) buffer))

(defmethod connect ((source segment) source-location (drain segment) drain-location buffer)
  (setf (output source-location source) buffer)
  (setf (input drain-location drain) buffer))

(defclass many-inputs-segment (segment)
  ((sources :initform (make-array 0 :adjustable T :fill-pointer T) :accessor sources)))

(defmethod add ((buffer buffer) (segment many-inputs-segment))
  (setf (input (length (inputs segment)) segment) buffer))

(defmethod withdraw ((buffer buffer) (segment many-inputs-segment))
  (setf (input (position buffer (inputs segment)) segment) NIL))

(defmethod input-field ((field (eql :source)) location (segment many-inputs-segment))
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get-in field location ptr (handle segment)))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf input-field) ((value segment) (field (eql :source)) location (segment many-inputs-segment))
  (with-error-on-failure ()
    (cl-mixed-cffi:segment-set-in field location (handle value) (handle segment)))
  (vector-insert-pos location value (sources segment))
  value)

(defmethod (setf input-field) :after ((value null) (field (eql :source)) location (segment many-inputs-segment))
  (vector-remove-pos location (sources segment)))

(defmethod source ((location integer) (segment many-inputs-segment))
  (input-field :source location segment))

(defmethod source ((buffer buffer) (segment many-inputs-segment))
  (input-field :source (position buffer (inputs segment)) segment))

(defmethod (setf source) ((value segment) (location integer) (segment many-inputs-segment))
  (setf (input-field :source location segment) value))

(defmethod (setf source) ((value segment) (buffer buffer) (segment many-inputs-segment))
  (setf (input-field :source (position buffer (inputs segment)) segment) value))

(defclass source (segment)
  ((channel :initarg :channel :reader channel))
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((source source) &key samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-source (handle (channel source)) samplerate (handle source))))

(defun make-source (data size encoding channels layout source-samplerate &optional (target-samplerate source-samplerate))
  (make-instance 'source :channel (make-channel data size encoding channels layout source-samplerate)
                         :samplerate target-samplerate))

(define-delegated-slot-accessor data source channel)
(define-delegated-slot-accessor size source channel)
(define-delegated-slot-accessor encoding source channel)
(define-delegated-slot-accessor channels source channel)
(define-delegated-slot-accessor layout source channel)
(define-delegated-slot-accessor samplerate source channel)
(define-field-accessor volume source :float :volume)
(define-field-accessor bypass source :bool :bypass)

(defclass drain (segment)
  ((channel :initarg :channel :reader channel))
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((drain drain) &key samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-drain (handle (channel drain)) samplerate (handle drain))))

(defun make-drain (data size encoding channels layout source-samplerate &optional (target-samplerate source-samplerate))
  (make-instance 'drain :channel (make-channel data size encoding channels layout source-samplerate)
                        :samplerate target-samplerate))

(define-delegated-slot-accessor data drain channel)
(define-delegated-slot-accessor size drain channel)
(define-delegated-slot-accessor encoding drain channel)
(define-delegated-slot-accessor channels drain channel)
(define-delegated-slot-accessor layout drain channel)
(define-delegated-slot-accessor samplerate drain channel)
(define-field-accessor volume drain :float :volume)
(define-field-accessor bypass drain :bool :bypass)

(defclass linear-mixer (many-inputs-segment)
  ((channels :initarg :channels :accessor channels))
  (:default-initargs :channels 1))

(defmethod initialize-instance :after ((mixer linear-mixer) &key)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-mixer (channels mixer) (handle mixer))))

(defun make-linear-mixer (channels)
  (make-instance 'linear-mixer :channels channels))

(define-field-accessor volume linear-mixer :float :volume)

(defmethod add ((new segment) (segment linear-mixer))
  (let ((buffers (outputs new))
        (location (length (inputs segment))))
    (loop for i from 0 below (channels segment)
          do (setf (input-field :buffer (+ i location) segment)
                   (aref buffers i)))
    (setf (input-field :source location segment) new)
    new))

(defmethod withdraw ((old segment) (segment linear-mixer))
  (let ((buffers (outputs old))
        (inputs (inputs segment)))
    (loop for i from 0 below (channels segment)
          for location = (position (aref buffers i) inputs)
          do (setf (input-field :buffer location segment)
                   NIL))
    old))

(defclass general (segment)
  ()
  (:default-initargs
   :volume 1.0
   :pan 0.0))

(defmethod initialize-instance :after ((segment general) &key volume pan)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-general volume pan (handle segment))))

(defun make-general (&rest args &key volume pan)
  (declare (ignore volume pan))
  (apply #'make-instance 'general args))

(define-field-accessor volume general :float :volume)
(define-field-accessor pan general :float :general-pan)
(define-field-accessor bypass general :bool :bypass)

(defclass fade (segment)
  ()
  (:default-initargs
   :from 0.0
   :to 1.0
   :time 1.0
   :type :cubic-in-out
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment fade) &key from to time type samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-fade from to time type samplerate (handle segment))))

(defun make-fade (&rest args &key from to time type samplerate)
  (declare (ignore from to time type samplerate))
  (apply #'make-instance 'fade args))

(define-field-accessor from fade :float :fade-from)
(define-field-accessor to fade :float :fade-to)
(define-field-accessor duration fade :float :fade-time)
(define-field-accessor fade-type fade cl-mixed-cffi:fade-type)
(define-field-accessor bypass fade :bool :bypass)

(defclass generator (segment)
  ()
  (:default-initargs
   :type :sine
   :frequency 440
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment generator) &key type frequency samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-generator type frequency samplerate (handle segment))))

(defun make-generator (&rest args &key type frequency samplerate)
  (declare (ignore type frequency samplerate))
  (apply #'make-instance 'generator args))

(define-field-accessor volume generator :float :volume)
(define-field-accessor wave-type generator cl-mixed-cffi:generator-type :generator-type)
(define-field-accessor frequency generator :float :generator-frequency)

(defclass ladspa (segment)
  ()
  (:default-initargs
   :file (error "LADSPA FILE required.")
   :index 0
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((segment ladspa) &key file index samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-ladspa file index samplerate (handle segment))))

(defun make-ladspa (&rest args &key file (index 0) (samplerate *default-samplerate*) &allow-other-keys)
  (let ((instance (make-instance 'ladspa :file file :index index :samplerate samplerate))
        (other-args (removef args :file :index :samplerate)))
    (loop for (field value) on other-args by #'cddr
          do (setf (field field instance) value))
    instance))

(defmethod field (field (segment ladspa))
  (cffi:with-foreign-object (value-ptr :float)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get field value-ptr segment))
    (cffi:mem-ref value-ptr :float)))

(defmethod (setf field) (value field (segment ladspa))
  (cffi:with-foreign-object (value-ptr :float)
    (setf (cffi:mem-ref value-ptr :float) (coerce value 'single-float))
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get field value-ptr segment)))
  value)

(defclass space (many-inputs-segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((space space) &key samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-space samplerate (handle space))))

(defun make-space (&rest args &key (samplerate *default-samplerate*) up soundspeed doppler-factor min-distance max-distance rolloff attenuation)
  (declare (ignore up soundspeed doppler-factor min-distance max-distance rolloff attenuation))
  (let ((instance (make-instance 'space :samplerate samplerate)))
    (loop for (field value) on args by #'cddr
          do (unless (eql field :samplerate)
               (setf (field field instance) value)))
    instance))

(define-vector-field-accessor location space :space-location)
(define-vector-field-accessor velocity space :space-velocity)
(define-vector-field-accessor direction space :space-direction)
(define-vector-field-accessor up space :space-up)

(define-input-vector-field-accessor input-location space :location :space-location)
(define-input-vector-field-accessor input-velocity space :velocity :space-velocity)

(define-field-accessor soundspeed space :float :space-soundspeed)
(define-field-accessor doppler-factor space :float :space-doppler-factor)
(define-field-accessor min-distance space :float :space-min-distance)
(define-field-accessor max-distance space :float :space-max-distance)
(define-field-accessor rolloff space :float :space-rolloff)
(define-field-accessor volume space :float :volume)

(defmethod field ((field (eql :attenuation)) (segment space))
  (cffi:with-foreign-object (value-ptr :pointer)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get field value-ptr segment))
    (loop with int = (cffi:mem-ref value-ptr :int)
          for keyword in (cffi:foreign-enum-keyword-list 'cl-mixed-cffi:attenuation)
          do (when (= int (cffi:foreign-enum-value 'cl-mixed-cffi:attenuation keyword))
               (return keyword))
          finally (return (cffi:mem-ref value-ptr :pointer)))))

(defmethod (setf field) (value (field (eql :attenuation)) (segment space))
  (cffi:with-foreign-object (value-ptr :pointer)
    (etypecase value
      (keyword
       (setf (cffi:mem-ref value-ptr :int)
             (cffi:foreign-enum-value 'cl-mixed-cffi:attenuation value)))
      (cffi:foreign-pointer
       (setf (cffi:mem-ref value-ptr :pointer) value)))
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get field value-ptr segment)))
  value)

(defmethod attenuation ((space space))
  (field :attenuation space))

(defmethod (setf attenuation) (value (space space))
  (setf (field :attenuation space) value))

(defmethod add ((new segment) (segment space))
  (let ((buffer (aref (outputs new) 0))
        (location (length (inputs segment))))
    (add buffer segment)
    (setf (input-field :source location segment) new)
    new))

(defmethod withdraw ((old segment) (segment space))
  (let ((buffer (aref (outputs old) 0)))
    (withdraw buffer segment)
    old))

(defclass virtual (segment)
  ())

(defmethod initialize-instance :after ((segment virtual) &key)
  (let ((handle (handle segment)))
    (setf (cl-mixed-cffi:direct-segment-free handle) (cffi:callback virtual-free))
    (setf (cl-mixed-cffi:direct-segment-info handle) (cffi:callback virtual-info))
    (setf (cl-mixed-cffi:direct-segment-start handle) (cffi:callback virtual-start))
    (setf (cl-mixed-cffi:direct-segment-mix handle) (cffi:callback virtual-mix))
    (setf (cl-mixed-cffi:direct-segment-end handle) (cffi:callback virtual-end))
    (setf (cl-mixed-cffi:direct-segment-set-in handle) (cffi:callback virtual-set-input))
    (setf (cl-mixed-cffi:direct-segment-set-out handle) (cffi:callback virtual-set-output))
    (setf (cl-mixed-cffi:direct-segment-get-in handle) (cffi:callback virtual-get-input))
    (setf (cl-mixed-cffi:direct-segment-get-out handle) (cffi:callback virtual-get-output))
    (setf (cl-mixed-cffi:direct-segment-set handle) (cffi:callback virtual-set))
    (setf (cl-mixed-cffi:direct-segment-get handle) (cffi:callback virtual-get))))

(defmethod free-handle ((virtual virtual) handle)
  (lambda ()
    (cffi:foreign-free handle)
    (setf (pointer->object handle) NIL)))

(defmethod info ((virtual virtual)))
(defmethod start ((virtual virtual)))
(defmethod mix (samples (virtual virtual)))
(defmethod end ((virtual virtual)))
(defmethod input-field (field location (virtual virtual)))
(defmethod (setf input-field) (value field location (virtual virtual)))
(defmethod output-field (field location (virtual virtual)))
(defmethod (setf output-field) (value field location (virtual virtual)))
(defmethod field (field (virtual virtual)))
(defmethod (setf field) (value field (virtual virtual)))

(defmethod input-field ((field (eql :buffer)) location (segment virtual))
  (aref (inputs segment) location))

(defmethod output-field ((field (eql :buffer)) location (segment virtual))
  (aref (outputs segment) location))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) location (segment virtual))
  (vector-insert-pos location value (inputs segment))
  value)

(defmethod (setf output-field) ((value buffer) (field (eql :buffer)) location (segment virtual))
  (vector-insert-pos location value (outputs segment))
  value)

(defmethod (setf input-field) ((value null) (field (eql :buffer)) location (segment virtual))
  (vector-remove-pos location (inputs segment))
  value)

(defmethod (setf output-field) ((value null) (field (eql :buffer)) location (segment virtual))
  (vector-remove-pos location (outputs segment))
  value)

(define-callback virtual-free :void ((segment :pointer))
    NIL
  (free (pointer->object segment)))

(define-callback virtual-info :pointer ((segment :pointer))
    (cffi:null-pointer)
  (destructuring-bind (&key name description flags min-inputs max-inputs outputs fields)
      (info (pointer->object segment))
    (let ((info (cffi:foreign-alloc '(:struct cl-mixed-cffi:segment-info))))
      (setf (cl-mixed-cffi:segment-info-name info) name)
      (setf (cl-mixed-cffi:segment-info-description info) description)
      (setf (cl-mixed-cffi:segment-info-flags info) (encode-flags flags))
      (setf (cl-mixed-cffi:segment-info-min-inputs info) min-inputs)
      (setf (cl-mixed-cffi:segment-info-max-inputs info) max-inputs)
      (setf (cl-mixed-cffi:segment-info-outputs info) outputs)
      (encode-field-info fields info)
      info)))

(define-std-callback virtual-start ((segment :pointer))
  (start (pointer->object segment)))

(define-callback virtual-mix :void ((samples size_t) (segment :pointer))
    NIL
  (mix samples (pointer->object segment)))

(define-std-callback virtual-end ((segment :pointer))
  (end (pointer->object segment)))

(define-std-callback virtual-set-input ((field size_t) (location size_t) (value :pointer) (segment :pointer))
  (setf (input-field field location (pointer->object segment)) value))

(define-std-callback virtual-set-output ((field size_t) (location size_t) (value :pointer) (segment :pointer))
  (setf (output-field field location (pointer->object segment)) value))

(define-std-callback virtual-get-input ((field size_t) (location size_t) (value-ptr :pointer) (segment :pointer))
  (multiple-value-bind (value type) (input-field field location (pointer->object segment))
    (setf (cffi:mem-ref value-ptr type) value)))

(define-std-callback virtual-get-output ((field size_t) (location size_t) (value-ptr :pointer) (segment :pointer))
  (multiple-value-bind (value type) (output-field field location (pointer->object segment))
    (setf (cffi:mem-ref value-ptr type) value)))

(define-std-callback virtual-set ((field size_t) (value :pointer) (segment :pointer))
  (setf (field field (pointer->object segment)) value))

(define-std-callback virtual-get ((field size_t) (value-ptr :pointer) (segment :pointer))
  (multiple-value-bind (value type) (field field (pointer->object segment))
    (setf (cffi:mem-ref value-ptr type) value)))
