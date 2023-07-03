(in-package #:org.shirakumo.fraf.mixed)

(defclass volume-control (segment)
  ()
  (:default-initargs
   :volume 1.0
   :pan 0.0))

(defmethod initialize-instance :after ((segment volume-control) &key volume pan bypass)
  (with-error-on-failure ()
    (mixed:make-segment-volume-control (float volume 0f0) (float pan 0f0) (handle segment)))
  (setf (bypass segment) bypass))

(defun make-volume-control (&rest args &key volume pan)
  (declare (ignore volume pan))
  (apply #'make-instance 'volume-control args))

(define-field-accessor volume volume-control :float :volume)
(define-field-accessor pan volume-control :float :volume-control-pan)
(define-field-accessor bypass volume-control :bool :bypass)
