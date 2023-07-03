(in-package #:org.shirakumo.fraf.mixed)

(defclass basic-mixer (mixer)
  ((channels :initarg :channels :initform 2 :accessor channels)))

(defmethod initialize-instance :after ((mixer basic-mixer) &key)
  (with-error-on-failure ()
    (mixed:make-segment-basic-mixer (channels mixer) (handle mixer))))

(defun make-basic-mixer (&optional (channels 2))
  (make-instance 'basic-mixer :channels channels))

(define-field-accessor volume basic-mixer :float :volume)

(defmethod add ((new segment) (segment basic-mixer))
  (let ((buffers (outputs new)))
    (loop for i from 0 below (channels segment)
          do (setf (input-field :buffer T segment) (aref buffers i)))
    new))

(defmethod withdraw ((old segment) (segment basic-mixer))
  (let ((buffers (outputs old))
        (inputs (inputs segment)))
    (loop for i from 0 below (channels segment)
          for location = (position (aref buffers i) inputs)
          do (setf (input-field :buffer location segment) NIL))
    old))
