#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass unpacker (segment)
  ((pack :initarg :pack :reader pack))
  (:default-initargs
   :pack (error "PACK required.")
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((unpacker unpacker) &key samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-unpacker (handle (pack unpacker)) samplerate (handle unpacker))))

(defun make-unpacker (size encoding channels unpacker-samplerate &optional (target-samplerate unpacker-samplerate))
  (make-instance 'unpacker :pack (make-pack size encoding channels unpacker-samplerate)
                           :samplerate target-samplerate))

(define-delegated-slot-accessor data unpacker pack)
(define-delegated-slot-accessor size unpacker pack)
(define-delegated-slot-accessor encoding unpacker pack)
(define-delegated-slot-accessor channels unpacker pack)
(define-delegated-slot-accessor samplerate unpacker pack)
(define-field-accessor volume unpacker :float :volume)
(define-field-accessor bypass unpacker :bool :bypass)
