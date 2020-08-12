#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass packer (segment)
  ((pack :initarg :pack :reader pack))
  (:default-initargs
   :pack (error "PACK required.")
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((packer packer) &key samplerate)
  (with-error-on-failure ()
    (mixed:make-segment-packer (handle (pack packer)) samplerate (handle packer))))

(defun make-packer (frames encoding channels source-samplerate &optional (target-samplerate source-samplerate))
  (make-instance 'packer :pack (make-pack frames encoding channels source-samplerate)
                         :samplerate target-samplerate))

(define-delegated-slot-accessor data packer pack)
(define-delegated-slot-accessor size packer pack)
(define-delegated-slot-accessor encoding packer pack)
(define-delegated-slot-accessor channels packer pack)
(define-delegated-slot-accessor samplerate packer pack)
(define-field-accessor volume packer :float :volume)
(define-field-accessor bypass packer :bool :bypass)
