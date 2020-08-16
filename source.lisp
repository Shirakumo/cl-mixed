#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass source (virtual)
  ((pack :initform NIL :initarg :pack :accessor pack)
   (byte-position :initform 0 :accessor byte-position)))

(defmethod info ((source source))
  (list :name (string (class-name (class-of source)))
        :description "Input source."
        :flags ()
        :min-inputs 0
        :max-inputs 0
        :outputs 1
        :fields ()))

(defmethod output-field ((field (eql :pack)) (location (eql 0)) (drain drain))
  (pack drain))

(defmethod (setf output-field) ((value pack) (field (eql :pack)) (location (eql 0)) (drain drain))
  (setf (pack drain) value))

(defmethod (setf output-field) ((value null) (field (eql :pack)) (location (eql 0)) (drain drain))
  (setf (pack drain) value))

(defmethod output ((location (eql 0)) (drain drain))
  (pack drain))

(defmethod seek ((source source) position &key (mode :absolute) (by :frame))
  (ecase by
    (:second
     (setf position (floor (* position (samplerate (pack source))))))
    (:percentage
     (setf position (floor (* position (frame-count source)) 100)))
    (:frame))
  (ecase mode
    (:relative
     (setf mode :absolute)
     (incf position (/ (byte-position source) (framesize (pack source)))))
    (:absolute))
  (seek-to-frame source position)
  (setf (byte-position source) (* position (framesize (pack source))))
  source)

(defgeneric seek-to-frame (source position))
(defgeneric frame-count (source))
