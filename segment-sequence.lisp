#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

;; We have to keep track of the segments here in order to ensure
;; that they don't get GCed and thus pulled away from under our
;; feet during the runtime of the program. On the other hand,
;; this can cause problems when another C program modifies the
;; mixer, as we will be blind to that kind of change. For now we
;; settle on hoping you won't do any of that crazy nonsense, or
;; pay for the consequences yourself if you do.
(defclass segment-sequence (c-object)
  ((segments :initform (make-array 0 :adjustable T :fill-pointer T) :reader segments)))

(defmethod initialize-instance :after ((sequence segment-sequence) &key handle)
  (when handle
    ;; Attempt to back-fill.
    (let ((ptr (mixed:segment-sequence-segments handle)))
      (loop for i from 0 below (size sequence)
            do (vector-push-extend (pointer->object (cffi:mem-aref ptr :pointer i))
                                   (segments sequence))))))

(defun make-segment-sequence (&rest segments)
  (let ((sequence (make-instance 'segment-sequence)))
    (dolist (segment segments)
      (add segment sequence))
    sequence))

(defmethod allocate-handle ((sequence segment-sequence))
  (calloc '(:struct mixed:segment-sequence)))

(defmethod free-handle ((sequence segment-sequence) handle)
  (lambda ()
    (mixed:free-segment-sequence handle)
    (cffi:foreign-free handle)
    (setf (pointer->object handle) NIL)))

(defmethod add ((segment segment) (sequence segment-sequence))
  (with-error-on-failure ()
    (mixed:segment-sequence-add (handle segment) (handle sequence)))
  (vector-push-extend segment (segments sequence))
  segment)

(defmethod withdraw ((segment segment) (sequence segment-sequence))
  (with-error-on-failure ()
    (mixed:segment-sequence-remove (handle segment) (handle sequence)))
  (vector-remove segment (segments sequence))
  segment)

(defmethod start ((sequence segment-sequence))
  (with-error-on-failure ()
    (mixed:segment-sequence-start (handle sequence))))

(defmethod mix (samples (sequence segment-sequence))
  (mixed:segment-sequence-mix samples (handle sequence))
  (unless (eql :no-error (mixed:error))
    (error 'mixed-error)))

(defmethod end ((sequence segment-sequence))
  (with-error-on-failure ()
    (mixed:segment-sequence-end (handle sequence))))

(define-accessor size segment-sequence mixed:segment-sequence-count)
