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
                              (cl-mixed-cffi:field-info-flags field))
                      :type (cl-mixed-cffi:field-info-type field)
                      :type-count (cl-mixed-cffi:field-info-type-count field))))

(defun encode-field-info (fields info)
  (loop for field = (cffi:foreign-slot-pointer info '(:struct cl-mixed-cffi:segment-info) 'cl-mixed-cffi::fields)
        then (cffi:inc-pointer field (cffi:foreign-type-size '(:struct cl-mixed-cffi:field-info)))
        for fieldspec in fields
        do (destructuring-bind (&key fieldno description flags type type-count) fieldspec
             (setf (cl-mixed-cffi:field-info-field field) fieldno)
             (setf (cl-mixed-cffi:field-info-description field) description)
             (setf (cl-mixed-cffi:field-info-flags field) (encode-flags flags))
             (setf (cl-mixed-cffi:field-info-type field) (or type :unknown))
             (setf (cl-mixed-cffi:field-info-type-count field) (or type-count 1)))))

;; See the comment on the segment-sequence class for an explanation on the arrays.
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

(defmethod input-field ((field (eql :buffer)) (location symbol) (segment segment))
  (input-field field (cffi:foreign-enum-value 'cl-mixed-cffi:location location) segment))

(defmethod input-field ((field (eql :buffer)) (location integer) (segment segment))
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get-in field location ptr segment))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf input-field) (value (field (eql :buffer)) (location symbol) (segment segment))
  (setf (input-field field (cffi:foreign-enum-value 'cl-mixed-cffi:location location) segment) value))

(defmethod (setf input-field) ((value buffer) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (cl-mixed-cffi:segment-set-in field location (handle value) (handle segment)))
  (vector-insert-pos location value (inputs segment))
  value)

(defmethod (setf input-field) ((value null) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (cl-mixed-cffi:segment-set-in field location (cffi:null-pointer) (handle segment)))
  (vector-remove-pos location (inputs segment))
  value)

(defmethod output-field ((field (eql :buffer)) (location symbol) (segment segment))
  (output-field field (cffi:foreign-enum-value 'cl-mixed-cffi:location location) segment))

(defmethod output-field ((field (eql :buffer)) (location integer) (segment segment))
  (cffi:with-foreign-object (ptr :pointer)
    (with-error-on-failure ()
      (cl-mixed-cffi:segment-get-out field location ptr segment))
    (or (pointer->object (cffi:mem-ref ptr :pointer))
        (make-instance 'buffer :handle (cffi:mem-ref ptr :pointer)))))

(defmethod (setf output-field) (value (field (eql :buffer)) (location symbol) (segment segment))
  (setf (output-field field (cffi:foreign-enum-value 'cl-mixed-cffi:location location) segment) value))

(defmethod (setf output-field) ((value buffer) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (cl-mixed-cffi:segment-set-out field location (handle value) (handle segment)))
  (vector-insert-pos location value (outputs segment))
  value)

(defmethod (setf output-field) ((value null) (field (eql :buffer)) (location integer) (segment segment))
  (with-error-on-failure ()
    (cl-mixed-cffi:segment-set-out field location (cffi:null-pointer) (handle segment)))
  (vector-remove-pos location (outputs segment))
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
