#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

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

(defmethod input-field ((field (eql :buffer)) (location integer) (segment virtual))
  (aref (inputs segment) location))

(defmethod output-field ((field (eql :buffer)) (location integer) (segment virtual))
  (aref (outputs segment) location))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) (location integer) (segment virtual))
  (vector-insert-pos location value (inputs segment))
  value)

(defmethod (setf output-field) ((value buffer) (field (eql :buffer)) (location integer) (segment virtual))
  (vector-insert-pos location value (outputs segment))
  value)

(defmethod (setf input-field) ((value null) (field (eql :buffer)) (location integer) (segment virtual))
  (vector-remove-pos location (inputs segment))
  value)

(defmethod (setf output-field) ((value null) (field (eql :buffer)) (location integer) (segment virtual))
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
