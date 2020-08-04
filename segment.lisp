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
     (loop for flag in (cffi:foreign-enum-keyword-list 'mixed:info-flags)
           when (/= 0 (logand integer (cffi:foreign-enum-value 'mixed:info-flags flag)))
           collect flag))))

(Defun encode-flags (flags)
  (etypecase flags
    (integer flags)
    (list
     (loop with integer = 0
           for flag in flags
           do (setf integer (logior integer (cffi:foreign-enum-value 'mixed:info-flags flag)))
           finally (return integer)))))

(defun decode-field-info (info)
  (loop for field = (cffi:foreign-slot-pointer info '(:struct mixed:segment-info) 'mixed::fields)
        then (cffi:inc-pointer field (cffi:foreign-type-size '(:struct mixed:field-info)))
        repeat 32
        until (mixed:field-info-flags field)
        collect (list :field (mixed:field-info-field field)
                      :description (mixed:field-info-description field)
                      :flags (decode-flags
                              (mixed:field-info-flags field))
                      :type (mixed:field-info-type field)
                      :type-count (mixed:field-info-type-count field))))

(defun encode-field-info (fields info)
  (loop for field = (cffi:foreign-slot-pointer info '(:struct mixed:segment-info) 'mixed::fields)
        then (cffi:inc-pointer field (cffi:foreign-type-size '(:struct mixed:field-info)))
        for fieldspec in fields
        do (destructuring-bind (&key fieldno description flags type type-count) fieldspec
             (setf (mixed:field-info-field field) fieldno)
             (setf (mixed:field-info-description field) description)
             (setf (mixed:field-info-flags field) (encode-flags flags))
             (setf (mixed:field-info-type field) (or type :unknown))
             (setf (mixed:field-info-type-count field) (or type-count 1)))))

;; See the comment on the segment-sequence class for an explanation on the arrays.
(defclass segment (c-object)
  ((inputs :initform (make-array 0 :adjustable T :fill-pointer T) :reader inputs)
   (outputs :initform (make-array 0 :adjustable T :fill-pointer T) :reader outputs)
   (info :initform NIL :accessor direct-info)))

(defmethod info ((segment segment))
  (unless (direct-info segment)
    (cffi:with-foreign-object (info '(:struct mixed:segment-info))
      (with-error-on-failure ()
        (mixed:segment-info info (handle segment)))
      (setf (direct-info segment)
            (list :name (mixed:segment-info-name info)
                  :description (mixed:segment-info-description info)
                  :flags (decode-flags (mixed:segment-info-flags info))
                  :min-inputs (mixed:segment-info-min-inputs info)
                  :max-inputs (mixed:segment-info-max-inputs info)
                  :outputs (mixed:segment-info-outputs info)
                  :fields (decode-field-info info)))))
  (direct-info segment))

(defmethod start ((segment segment))
  (mixed:segment-start (handle segment)))

(defmethod mix (samples (segment segment))
  (mixed:segment-mix samples (handle segment)))

(defmethod end ((segment segment))
  (mixed:segment-end (handle segment)))

(defmethod allocate-handle ((segment segment))
  (calloc '(:struct mixed:segment)))

(defmethod free-handle ((segment segment) handle)
  (lambda ()
    (mixed:free-segment handle)
    (cffi:foreign-free handle)
    (setf (pointer->object handle) NIL)))

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
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (mixed:segment-get-in field location ptr segment))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf input-field) (value (field (eql :buffer)) (location symbol) (segment segment))
  (setf (input-field field (cffi:foreign-enum-value 'mixed:location location) segment) value))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-in field location (handle value) (handle segment)))
  (vector-insert-pos location value (inputs segment))
  value)

(defmethod (setf input-field) ((value null) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-in field location (cffi:null-pointer) (handle segment)))
  (vector-remove-pos location (inputs segment))
  value)

(defmethod output-field ((field (eql :buffer)) (location symbol) (segment segment))
  (output-field field (cffi:foreign-enum-value 'mixed:location location) segment))

(defmethod output-field ((field (eql :buffer)) (location integer) (segment segment))
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (mixed:segment-get-out field location ptr segment))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf output-field) (value (field (eql :buffer)) (location symbol) (segment segment))
  (setf (output-field field (cffi:foreign-enum-value 'mixed:location location) segment) value))

(defmethod (setf output-field) ((value buffer) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-out field location (handle value) (handle segment)))
  (vector-insert-pos location value (outputs segment))
  value)

(defmethod (setf output-field) ((value null) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (mixed:segment-set-out field location (cffi:null-pointer) (handle segment)))
  (vector-remove-pos location (outputs segment))
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

(defmethod connect ((source segment) source-location (drain segment) drain-location buffer)
  (setf (output source-location source) buffer)
  (setf (input drain-location drain) buffer))
