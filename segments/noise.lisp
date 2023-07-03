(in-package #:org.shirakumo.fraf.mixed)

(defclass noise (segment)
  ()
  (:default-initargs
   :type :pink))

(defmethod initialize-instance :after ((segment noise) &key type)
  (with-error-on-failure ()
    (mixed:make-segment-noise type (handle segment))))

(defun make-noise (&rest args &key type)
  (declare (ignore type))
  (apply #'make-instance 'noise args))

(define-field-accessor volume noise :float :volume)
(define-field-accessor noise-type noise mixed:noise-type :noise-type)
