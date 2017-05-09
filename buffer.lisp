#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass buffer (c-object)
  ((size :initarg :size :accessor size)))

(defmethod initialize-instance :after ((buffer buffer) &key handle size)
  (unless handle
    (let ((handle (handle buffer)))
      (unless size
        (error "Buffer SIZE required."))
      (with-error-on-failure ()
        (cl-mixed-cffi:make-buffer (size buffer) handle)))))

(defmethod allocate-handle ((buffer buffer))
  (calloc '(:struct cl-mixed-cffi:buffer)))

(defmethod free-handle ((buffer buffer) handle)
  (lambda ()
    (cl-mixed-cffi:free-buffer handle)
    (cffi:foreign-free handle)))

(define-accessor data buffer cl-mixed-cffi:buffer-data)
(define-accessor size buffer cl-mixed-cffi:buffer-size)
