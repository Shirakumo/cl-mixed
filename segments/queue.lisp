#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass queue (segment)
  ((segments :initform (make-array 0 :adjustable T :fill-pointer T) :reader segments)))

(defmethod initialize-instance :after ((segment queue) &key inputs outputs)
  (with-error-on-failure ()
    (mixed:make-segment-queue (handle segment)))
  (when inputs (setf (in-count segment) inputs))
  (when outputs (setf (in-count segment) outputs)))

(defun make-queue (&rest args)
  (apply #'make-instance 'queue args))

(defmethod field ((field (eql :current-segment)) (segment queue))
  (with-foreign-object (handle :pointer)
    (with-error-on-failure ()
      (mixed:segment-get :current-segment handle (handle segment)))
    (pointer->object (mem-ref handle :pointer))))

(defmethod current-segment ((segment queue))
  (field :current-segment segment))

(define-field-accessor in-count queue size_t :in-count)
(define-field-accessor out-count queue size_t :out-count)
(define-field-accessor bypass queue :bool :bypass)

(defmethod add ((segment segment) (queue queue))
  (with-error-on-failure ()
    (mixed:queue-add (handle segment) (handle queue)))
  (vector-push-extend segment (segments queue))
  segment)

(defmethod withdraw ((segment segment) (queue queue))
  (with-error-on-failure ()
    (mixed:queue-remove (handle segment) (handle queue)))
  (vector-remove segment (segments queue))
  segment)

(defmethod withdraw ((position integer) (queue queue))
  (with-error-on-failure ()
    (mixed:queue-remove-at position (handle queue)))
  (vector-remove-pos position (segments queue)))

(defmethod clear ((queue queue))
  (with-error-on-failure ()
    (mixed:queue-clear (handle queue)))
  (loop for i from 0 below (length (segments queue))
        do (setf (aref (segments queue) i) NIL))
  (setf (fill-pointer (segments queue)) 0)
  queue)
