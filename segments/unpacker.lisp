#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass unpacker (segment)
  ((packed-audio :initarg :packed-audio :reader packed-audio))
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((unpacker unpacker) &key samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-unpacker (handle (packed-audio unpacker)) samplerate (handle unpacker))))

(defun make-unpacker (data size encoding channels layout unpacker-samplerate &optional (target-samplerate unpacker-samplerate))
  (make-instance 'unpacker :packed-audio (make-packed-audio data size encoding channels layout unpacker-samplerate)
                           :samplerate target-samplerate))

(define-delegated-slot-accessor data unpacker packed-audio)
(define-delegated-slot-accessor size unpacker packed-audio)
(define-delegated-slot-accessor encoding unpacker packed-audio)
(define-delegated-slot-accessor channels unpacker packed-audio)
(define-delegated-slot-accessor layout unpacker packed-audio)
(define-delegated-slot-accessor samplerate unpacker packed-audio)
(define-field-accessor volume unpacker :float :volume)
(define-field-accessor bypass unpacker :bool :bypass)
