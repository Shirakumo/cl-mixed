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

(defclass segment (c-object)
  ())

(defmethod info ((segment segment))
  (let ((info (cl-mixed-cffi:segment-info (handle segment))))
    (list :name (cl-mixed-cffi:segment-info-name info)
          :description (cl-mixed-cffi:segment-info-description info)
          :flags (decode-flags (cl-mixed-cffi:segment-info-flags info))
          :min-inputs (cl-mixed-cffi:segment-info-min-inputs info)
          :max-inputs (cl-mixed-cffi:segment-info-max-inputs info)
          :outputs (cl-mixed-cffi:segment-info-outputs info)
          ;; FIXME fields
          )))

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
    (cffi:foreign-free handle)))

(defmethod input-field (field location (segment segment))
  )

(defmethod (setf input-field) (value field location (segment segment))
  )

(defmethod output-field (field location (segment segment))
  )

(defmethod (setf output-field) (value field location (segment segment))
  )

(defmethod field (field (segment segment))
  )

(defmethod (setf field) (value field (segment segment))
  )

(defmethod input (location (segment segment))
  (input-field :buffer location segment))

(defmethod (setf input) ((buffer buffer) location (segment segment))
  (setf (input-field :buffer location segment) buffer))

(defmethod output (location (segment segment))
  (output-field :buffer location segment))

(defmethod (setf output) ((buffer buffer) location (segment segment))
  (setf (output-field :buffer location segment) buffer))

(defclass many-inputs-segment (segment)
  ())

(defmethod add ((buffer buffer) (segment many-inputs-segment))
  )

(defmethod withdraw ((buffer buffer) (segment many-inputs-segment))
  )

(defclass source (segment)
  ())

(defmethod initialize-instance :after ((source source) &key channel)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-source (handle channel) (handle source))))

(defclass drain (segment)
  ())

(defmethod initialize-instance :after ((drain drain) &key channel)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-drain (handle channel) (handle drain))))

(defclass linear-mixer (many-inputs-segment)
  ())

(defmethod initialize-instance :after ((mixer linear-mixer) &key buffers)
  (cffi:with-foreign-object (buflist :pointer (1+ (length buffers)))
    (loop for buffer in buffers
          for i from 0
          do (setf (cffi:mem-aref buflist :pointer i) (handle buffer)))
    (setf (cffi:mem-aref buflist :pointer (length buffers)) (cffi:null-pointer))
    (with-error-on-failure ()
      (cl-mixed-cffi:make-segment-mixer buflist (handle mixer)))))

(defclass general (segment)
  ()
  (:default-initargs
   :volume 1.0
   :pan 0.0))

(defmethod initialize-instance :after ((general general) &key volume pan)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-general volume pan (handle general))))

(defclass fade (segment)
  ()
  (:default-initargs
   :from 0.0
   :to 1.0
   :time 1.0
   :type :cubic-in-out
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((general general) &key from to time type samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-fade from to time type sampelerate (handle general))))

(defclass generator (segment)
  ()
  (:default-initargs
   :type :sine
   :frequency 440
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((generator generator) &key type frequency samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-generator type frequency sampelerate (handle general))))

(defclass ladspa (segment)
  ()
  (:default-initargs
   :file (error "LADSPA FILE required.")
   :index 0
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((ladspa ladspa) &key file index samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-ladspa file index sampelerate (handle general))))

(defclass space (many-inputs-segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((space space) &key samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-space sampelerate (handle general))))

(defclass virtual (segment)
  ())

(defmethod initialize-instance :after ((virtual virtual) &key)
  (let ((handle (handle virtual)))
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
    (cffi:foreign-free handle)))

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
      (setf (cl-mixed-cffi:segment-info-min-inptus info) min-inptus)
      (setf (cl-mixed-cffi:segment-info-max-inputs info) max-inputs)
      ;; FIXME fields
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
  (setf (field field location (pointer->object segment)) value))

(define-std-callback virtual-get ((field size_t) (value-ptr :pointer) (segment :pointer))
  (multiple-value-bind (value type) (field field location (pointer->object segment))
    (setf (cffi:mem-ref value-ptr type) value)))
