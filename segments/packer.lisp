#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass packer (segment)
  ((packed-audio :initarg :packed-audio :reader packed-audio))
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((packer packer) &key samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-packer (handle (packed-audio packer)) samplerate (handle packer))))

(defun make-packer (data size encoding channels layout source-samplerate &optional (target-samplerate source-samplerate))
  (make-instance 'packer :packed-audio (make-packed-audio data size encoding channels layout source-samplerate)
                         :samplerate target-samplerate))

(define-delegated-slot-accessor data packer packed-audio)
(define-delegated-slot-accessor size packer packed-audio)
(define-delegated-slot-accessor encoding packer packed-audio)
(define-delegated-slot-accessor channels packer packed-audio)
(define-delegated-slot-accessor layout packer packed-audio)
(define-delegated-slot-accessor samplerate packer packed-audio)
(define-field-accessor volume packer :float :volume)
(define-field-accessor bypass packer :bool :bypass)
