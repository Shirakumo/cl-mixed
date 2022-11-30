#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass source (virtual)
  ((pack :initform NIL :reader pack)
   (frame-position :initform 0 :accessor frame-position)
   (done-p :initform NIL :accessor done-p)))

(defmethod initialize-instance :after ((source source) &key pack)
  (setf (pack source) pack))

(defmethod print-object ((source source) stream)
  (print-unreadable-object (source stream :type T)
    (ignore-errors
     (cond ((done-p source)
            (write-string "DONE" stream))
           ((null (frame-count source))
            (write-string "STREAM" stream))
           (T
            (format stream "~2d%" (floor (* (/ (frame-position source) (frame-count source)) 100))))))))

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

(defmethod output-field ((field (eql :pack)) (location (eql 0)) (source source))
  (pack source))

(defmethod (setf output-field) ((value pack) (field (eql :pack)) (location (eql 0)) (source source))
  (setf (pack source) value))

(defmethod (setf output-field) ((value null) (field (eql :pack)) (location (eql 0)) (source source))
  (setf (pack source) value))

(defmethod output ((location (eql 0)) (source source))
  (pack source))

(defmethod seek ((source source) position &key (mode :absolute) (by :frame))
  (assert (<= 0 position))
  (ecase by
    (:second
     (setf position (floor (* position (samplerate (pack source))))))
    (:percentage
     (setf position (floor (* position (frame-count source)) 100)))
    (:frame))
  (ecase mode
    (:relative
     (setf mode :absolute)
     (incf position (frame-position source)))
    (:absolute))
  (seek-to-frame source position)
  (cond ((<= (frame-count source) position)
         (setf (frame-position source) (frame-count source))
         (setf (done-p source) T))
        (T
         (setf (frame-position source) position)
         (setf (done-p source) NIL)))
  source)

(defmethod framesize ((source source))
  (framesize (pack source)))

(defmethod byte-position ((source source))
  (* (frame-position source) (framesize (pack source))))

(defmethod (setf byte-position) (position (source source))
  (setf (frame-position source) (floor position (framesize (pack source)))))

(defgeneric seek-to-frame (source position))
(defgeneric frame-count (source))

(defmethod channel-order ((source source))
  *default-channel-order*)

(defmethod duration ((source source))
  (float (/ (frame-count source)
            (samplerate (pack source)))
         0f0))
