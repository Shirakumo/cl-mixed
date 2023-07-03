(in-package #:org.shirakumo.fraf.mixed)

(defclass packer (segment)
  ((pack :initarg :pack :initform (error "PACK required") :reader pack)))

(defmethod initialize-instance :after ((packer packer) &key (samplerate *default-samplerate*))
  (with-error-on-failure ()
    (mixed:make-segment-packer (handle (pack packer)) samplerate (handle packer))))

(defun make-packer (&key pack (encoding :float) (channels 2) (samplerate *default-samplerate*) (frames (floor samplerate 100)) (target-samplerate samplerate))
  (make-instance 'packer :pack (or pack (make-pack :frames frames :encoding encoding :channels channels :samplerate target-samplerate))
                         :samplerate samplerate))

(define-delegated-slot-accessor data packer pack)
(define-delegated-slot-accessor size packer pack)
(define-delegated-slot-accessor encoding packer pack)
(define-delegated-slot-accessor channels packer pack)
(define-delegated-slot-accessor samplerate packer pack)
(define-field-accessor volume packer :float :volume)
(define-field-accessor bypass packer :bool :bypass)

(defmethod output-field ((field (eql :pack)) (location (eql 0)) (packer packer))
  (pack packer))

(defmethod output ((location (eql 0)) (packer packer))
  (pack packer))
