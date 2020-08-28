#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass source (virtual)
  ((pack :initform NIL :reader pack)
   (byte-position :initform 0 :accessor byte-position)
   (done-p :initform NIL :accessor done-p)))

(defmethod initialize-instance :after ((source source) &key pack)
  (setf (pack source) pack))

(defmethod (setf pack) (thing (source source))
  (etypecase thing
    ((or null pack) (setf (slot-value source 'pack) thing))
    (unpacker (setf (pack source) (pack thing)))))

(defmethod info ((source source))
  (list :name (string (class-name (class-of source)))
        :description "Input source."
        :flags ()
        :min-inputs 0
        :max-inputs 0
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

(defmethod seek ((source source) position &key (mode :absolute) (by :frame))
  (assert (< 0 position))
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
  (when (<= (frame-count source) position)
    (setf (position source) (frame-count source))
    (setf (done-p source) T))
  (seek-to-frame source position)
  (setf (byte-position source) (* position (framesize (pack source))))
  source)

(defgeneric seek-to-frame (source position))
(defgeneric frame-count (source))
