(in-package #:org.shirakumo.fraf.mixed)

(defclass virtual (segment)
  ())

(defmethod initialize-instance :after ((segment virtual) &key)
  (let ((handle (handle segment)))
    (setf (mixed:direct-segment-free handle) (cffi:callback virtual-free))
    (setf (mixed:direct-segment-info handle) (cffi:callback virtual-info))
    (setf (mixed:direct-segment-start handle) (cffi:callback virtual-start))
    (setf (mixed:direct-segment-mix handle) (cffi:callback virtual-mix))
    (setf (mixed:direct-segment-end handle) (cffi:callback virtual-end))
    (setf (mixed:direct-segment-set-in handle) (cffi:callback virtual-set-input))
    (setf (mixed:direct-segment-set-out handle) (cffi:callback virtual-set-output))
    (setf (mixed:direct-segment-get-in handle) (cffi:callback virtual-get-input))
    (setf (mixed:direct-segment-get-out handle) (cffi:callback virtual-get-output))
    (setf (mixed:direct-segment-set handle) (cffi:callback virtual-set))
    (setf (mixed:direct-segment-get handle) (cffi:callback virtual-get))))

(defmethod free ((virtual virtual)))

(defmethod info ((virtual virtual)))
(defmethod start ((virtual virtual)))
(defmethod mix ((virtual virtual)))
(defmethod end ((virtual virtual)))
(defmethod input-field (field location (virtual virtual)))
(defmethod (setf input-field) (value field location (virtual virtual)))
(defmethod output-field (field location (virtual virtual)))
(defmethod (setf output-field) (value field location (virtual virtual)))
(defmethod field (field (virtual virtual)))
(defmethod (setf field) (value field (virtual virtual)))

(defmethod input-field ((field (eql :buffer)) (location integer) (segment virtual))
  (aref (inputs segment) location))

(defmethod output-field ((field (eql :buffer)) (location integer) (segment virtual))
  (aref (outputs segment) location))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) (location integer) (segment virtual))
  (setf (aref (inputs segment) location) value))

(defmethod (setf output-field) ((value buffer) (field (eql :buffer)) (location integer) (segment virtual))
  (setf (aref (outputs segment) location) value))

(defmethod (setf input-field) ((value null) (field (eql :buffer)) (location integer) (segment virtual))
  (setf (aref (inputs segment) location) value))

(defmethod (setf output-field) ((value null) (field (eql :buffer)) (location integer) (segment virtual))
  (setf (aref (outputs segment) location) value))

(define-callback virtual-free :void ((segment :pointer))
    NIL
  (free (pointer->object segment)))

(define-callback virtual-info :pointer ((segment :pointer))
  (cffi:null-pointer)
  (destructuring-bind (&key name description flags min-inputs max-inputs outputs fields)
      (info (pointer->object segment))
    (let ((info (cffi:foreign-alloc '(:struct mixed:segment-info))))
      (setf (mixed:segment-info-name info) name)
      (setf (mixed:segment-info-description info) description)
      (setf (mixed:segment-info-flags info) flags)
      (setf (mixed:segment-info-min-inputs info) min-inputs)
      (setf (mixed:segment-info-max-inputs info) max-inputs)
      (setf (mixed:segment-info-outputs info) outputs)
      (encode-field-info fields info)
      info)))

(define-std-callback virtual-start ((segment :pointer))
  (start (pointer->object segment)))

(define-std-callback virtual-mix ((segment :pointer))
  (mix (pointer->object segment)))

(define-std-callback virtual-end ((segment :pointer))
  (end (pointer->object segment)))

(define-std-callback virtual-set-input ((field :uint32) (location :uint32) (value :pointer) (segment :pointer))
  (setf (input-field field location (pointer->object segment)) value))

(define-std-callback virtual-set-output ((field :uint32) (location :uint32) (value :pointer) (segment :pointer))
  (setf (output-field field location (pointer->object segment)) value))

(define-std-callback virtual-get-input ((field :uint32) (location :uint32) (value-ptr :pointer) (segment :pointer))
  (multiple-value-bind (value type) (input-field field location (pointer->object segment))
    (setf (cffi:mem-ref value-ptr type) value)))

(define-std-callback virtual-get-output ((field :uint32) (location :uint32) (value-ptr :pointer) (segment :pointer))
  (multiple-value-bind (value type) (output-field field location (pointer->object segment))
    (setf (cffi:mem-ref value-ptr type) value)))

(define-std-callback virtual-set ((field :uint32) (value :pointer) (segment :pointer))
  (setf (field field (pointer->object segment)) value))

(define-std-callback virtual-get ((field :uint32) (value-ptr :pointer) (segment :pointer))
  (multiple-value-bind (value type) (field field (pointer->object segment))
    (setf (cffi:mem-ref value-ptr type) value)))
