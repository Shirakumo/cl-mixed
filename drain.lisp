#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass drain (mixed:virtual)
  ((target-samplerate :initarg :target-samplerate :initform 48000 :accessor target-samplerate)
   (program-name :initform NIL :initarg :program-name :initform "Mixed" :accessor program-name)
   (pack :initarg :pack :initform NIL :accessor pack)))

(defmethod info ((drain drain))
  (list :name (string (class-name (class-of drain)))
        :description "Output drain."
        :flags ()
        :min-inputs 1
        :max-inputs 1
        :outputs 0
        :fields ()))

(defmethod input-field ((field (eql :pack)) (location (eql 0)) (drain drain))
  (pack drain))

(defmethod (setf input-field) ((value pack) (field (eql :pack)) (location (eql 0)) (drain drain))
  (setf (pack drain) value))

(defmethod (setf input-field) ((value null) (field (eql :pack)) (location (eql 0)) (drain drain))
  (setf (pack drain) value))

(defmethod input ((location (eql 0)) (drain drain))
  (pack drain))
