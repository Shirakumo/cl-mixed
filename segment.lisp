#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defun decode-field-info (info)
  (loop for field = (cffi:foreign-slot-pointer info '(:struct mixed:segment-info) 'mixed::fields)
        then (cffi:inc-pointer field (cffi:foreign-type-size '(:struct mixed:field-info)))
        repeat 32
        until (mixed:field-info-flags field)
        collect (list :field (mixed:field-info-field field)
                      :description (mixed:field-info-description field)
                      :flags (mixed:field-info-flags field)
                      :type (mixed:field-info-type field)
                      :type-count (mixed:field-info-type-count field))))

(defun encode-field-info (fields info)
  (loop for field = (cffi:foreign-slot-pointer info '(:struct mixed:segment-info) 'mixed::fields)
        then (cffi:inc-pointer field (cffi:foreign-type-size '(:struct mixed:field-info)))
        for fieldspec in fields
        do (destructuring-bind (&key fieldno description flags type type-count) fieldspec
             (setf (mixed:field-info-field field) fieldno)
             (setf (mixed:field-info-description field) description)
             (setf (mixed:field-info-flags field) flags)
             (setf (mixed:field-info-type field) (or type :unknown))
             (setf (mixed:field-info-type-count field) (or type-count 1)))))

;; See the comment on the segment-sequence class for an explanation on the arrays.
(defclass segment (c-object)
  ((inputs :reader inputs)
   (outputs :reader outputs)
   (info :initform NIL :accessor direct-info)))

(defmethod initialize-instance :around ((segment segment) &key)
  (call-next-method)
  (revalidate segment))

(defmethod info ((segment segment))
  (unless (direct-info segment)
    (cffi:with-foreign-object (info '(:struct mixed:segment-info))
      (with-error-on-failure ()
        (mixed:segment-info info (handle segment)))
      (setf (direct-info segment)
            (list :name (mixed:segment-info-name info)
                  :description (mixed:segment-info-description info)
                  :flags (mixed:segment-info-flags info)
                  :min-inputs (mixed:segment-info-min-inputs info)
                  :max-inputs (mixed:segment-info-max-inputs info)
                  :outputs (mixed:segment-info-outputs info)
                  :fields (decode-field-info info)))))
  (direct-info segment))

(defmethod revalidate ((segment segment))
  (declare (optimize speed))
  (setf (direct-info segment) NIL)
  (destructuring-bind (&key outputs max-inputs &allow-other-keys) (info segment)
    (declare (type (unsigned-byte 32) outputs max-inputs))
    (flet ((marr (count)
             (if (< 128 count)
                 (make-array 0 :adjustable T :fill-pointer T :initial-element NIL)
                 (make-array count :initial-element NIL))))
      (setf (slot-value segment 'outputs) (marr outputs))
      (setf (slot-value segment 'inputs) (marr max-inputs)))))

(defmethod start ((segment segment))
  (with-error-on-failure ()
    (mixed:segment-start (handle segment))))

(defmethod mix ((segment segment))
  (with-error-on-failure ()
    (mixed:segment-mix (handle segment))))

(defmethod end ((segment segment))
  (with-error-on-failure ()
    (mixed:segment-end (handle segment))))

(defmethod allocate-handle ((segment segment))
  (calloc '(:struct mixed:segment)))

(defmethod free ((segment segment))
  (when (handle segment)
    (mixed:free-segment (handle segment))))

(defmethod (setf input-field) (value field location segment)
  (etypecase value
    (cffi:foreign-pointer
     (with-error-on-failure ()
       (mixed:segment-set-in field location value (handle segment))))))

(defmethod (setf output-field) (value field location segment)
  (etypecase value
    (cffi:foreign-pointer
     (with-error-on-failure ()
       (mixed:segment-set-out field location value (handle segment)))))
  value)

(defmethod input-field ((field (eql :buffer)) (location symbol) (segment segment))
  (input-field field (cffi:foreign-enum-value 'mixed:location location) segment))

(defmethod input-field ((field (eql :buffer)) (location integer) (segment segment))
  (aref (inputs segment) location))

(defmethod (setf input-field) (value (field (eql :buffer)) (location symbol) (segment segment))
  (setf (input-field field (cffi:foreign-enum-value 'mixed:location location) segment) value))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-in field location (handle value) (handle segment)))
  (setf (aref (inputs segment) location) value)
  value)

(defmethod (setf input-field) ((value null) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-in field location (cffi:null-pointer) (handle segment)))
  (setf (aref (inputs segment) location) NIL)
  value)

(defmethod output-field ((field (eql :buffer)) (location symbol) (segment segment))
  (output-field field (cffi:foreign-enum-value 'mixed:location location) segment))

(defmethod output-field ((field (eql :buffer)) (location integer) (segment segment))
  (aref (outputs segment) location))

(defmethod (setf output-field) (value (field (eql :buffer)) (location symbol) (segment segment))
  (setf (output-field field (cffi:foreign-enum-value 'mixed:location location) segment) value))

(defmethod (setf output-field) ((value buffer) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-out field location (handle value) (handle segment)))
  (setf (aref (outputs segment) location) value)
  value)

(defmethod (setf output-field) ((value null) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-out field location (cffi:null-pointer) (handle segment)))
  (setf (aref (outputs segment) location) value)
  value)

(defmethod (setf field) (value field (segment segment))
  (etypecase value
    (cffi:foreign-pointer
     (with-error-on-failure ()
       (mixed:segment-set field value (handle segment)))))
  value)

(defmethod input (location (segment segment))
  (input-field :buffer location segment))

(defmethod (setf input) (buffer location (segment segment))
  (setf (input-field :buffer location segment) buffer))

(defmethod output (location (segment segment))
  (output-field :buffer location segment))

(defmethod (setf output) (buffer location (segment segment))
  (setf (output-field :buffer location segment) buffer))

(defmethod connect ((source segment) source-location (drain segment) drain-location &optional buffer)
  (if buffer
      (setf (output source-location source) buffer)
      (setf buffer (output source-location source)))
  (setf (input drain-location drain) buffer))
