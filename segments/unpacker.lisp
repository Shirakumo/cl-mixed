#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass unpacker (segment)
  ((pack :initarg :pack :initform (error "PACK required") :reader pack)))

(defmethod initialize-instance :after ((unpacker unpacker) &key (samplerate *default-samplerate*))
  (with-error-on-failure ()
    (mixed:make-segment-unpacker (handle (pack unpacker)) samplerate (handle unpacker))))

(defun make-unpacker (&key pack (encoding :float) (channels 2) (samplerate *default-samplerate*) (frames (floor samplerate 100)) (source-samplerate samplerate))
  (make-instance 'unpacker :pack (or pack (make-pack :frames frames :encoding encoding :channels channels :samplerate source-samplerate))
                           :samplerate samplerate))

(define-delegated-slot-accessor data unpacker pack)
(define-delegated-slot-accessor size unpacker pack)
(define-delegated-slot-accessor encoding unpacker pack)
(define-delegated-slot-accessor channels unpacker pack)
(define-delegated-slot-accessor samplerate unpacker pack)
(define-field-accessor volume unpacker :float :volume)
(define-field-accessor bypass unpacker :bool :bypass)

(defmethod input-field ((field (eql :pack)) (location (eql 0)) (unpacker unpacker))
  (pack unpacker))

(defmethod input ((location (eql 0)) (unpacker unpacker))
  (pack unpacker))
