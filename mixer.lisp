#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

;; We have to keep track of the segments here in order to ensure
;; that they don't get GCed and thus pulled away from under our
;; feet during the runtime of  the program. On the other hand,
;; this can cause problems when another C program modifies the
;; mixer, as we will be blind to that kind of change.
(defclass mixer (c-object)
  ((segments :initform (make-array 0 :adjustable T :fill-pointer T) :reader segments)))

(defmethod initialize-instance :after ((mixer mixer) &key handle)
  (when handle
    ;; Attempt to back-fill.
    (let ((ptr (cl-mixed-cffi:mixer-segments handle)))
      (loop for i from 0 below (size mixer)
            do (vector-push-extend (pointer->object (cffi:mem-aref ptr :pointer i))
                                   (segments mixer))))))

(defun make-mixer (&rest segments)
  (let ((mixer (make-instance 'mixer)))
    (dolist (segment segments)
      (add segment mixer))
    mixer))

(defmethod allocate-handle ((mixer mixer))
  (calloc '(:struct cl-mixed-cffi:mixer)))

(defmethod free-handle ((mixer mixer) handle)
  (lambda ()
    (cl-mixed-cffi:free-mixer handle)
    (cffi:foreign-free handle)
    (setf (pointer->object handle) NIL)))

(defmethod add ((segment segment) (mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-add (handle segment) (handle mixer)))
  (vector-push-extend segment (segments mixer))
  segment)

(defmethod withdraw ((segment segment) (mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-remove (handle segment) (handle mixer)))
  (vector-remove segment (segments mixer))
  segment)

(defmethod start ((mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-start (handle mixer))))

(defmethod mix (samples (mixer mixer))
  (cl-mixed-cffi:mixer-mix samples (handle mixer))
  (unless (eql :no-error (cl-mixed-cffi:error))
    (error 'mixed-error)))

(defmethod end ((mixer mixer))
  (with-error-on-failure ()
    (cl-mixed-cffi:mixer-end (handle mixer))))

(define-accessor size mixer cl-mixed-cffi:mixer-count)
