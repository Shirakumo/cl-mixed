#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass drain (virtual)
  ((program-name :initform "Mixed" :initarg :program-name :accessor program-name)
   (pack :initform NIL :reader pack)))

(defmethod initialize-instance :after ((drain drain) &key pack)
  (setf (pack drain) pack))

(defmethod (setf pack) (thing (drain drain))
  (etypecase thing
    ((or null pack) (setf (slot-value drain 'pack) thing))
    (packer (setf (pack drain) (pack thing)))))

(defmethod info ((drain drain))
  (list :name (string (class-name (class-of drain)))
        :description "Output drain."
        :flags ()
        :min-inputs 1
        :max-inputs 1
        :outputs 0
        :fields ()))

(defmethod output-field ((field (eql :pack)) (location (eql 0)) (drain drain))
  (pack drain))

(defmethod (setf output-field) ((value pack) (field (eql :pack)) (location (eql 0)) (drain drain))
  (setf (pack drain) value))

(defmethod (setf output-field) ((value null) (field (eql :pack)) (location (eql 0)) (drain drain))
  (setf (pack drain) value))

(defmethod output ((location (eql 0)) (drain drain))
  (pack drain))
