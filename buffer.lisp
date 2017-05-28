#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass buffer (c-object)
  ())

(defmethod initialize-instance :after ((buffer buffer) &key handle size)
  (unless handle
    (let ((handle (handle buffer)))
      (unless size
        (error "Buffer SIZE required."))
      (with-error-on-failure ()
        (cl-mixed-cffi:make-buffer size handle)))))

(defun make-buffer (size)
  (make-instance 'buffer :size size))

(defmethod allocate-handle ((buffer buffer))
  (calloc '(:struct cl-mixed-cffi:buffer)))

(defmethod free-handle ((buffer buffer) handle)
  (lambda ()
    (cl-mixed-cffi:free-buffer handle)
    (cffi:foreign-free handle)
    (setf (pointer->object handle) NIL)))

(defmethod clear ((buffer buffer))
  (cl-mixed-cffi:clear-buffer buffer))

(define-accessor data buffer cl-mixed-cffi:buffer-data)
(define-accessor size buffer cl-mixed-cffi:buffer-size)

(defmacro with-buffers (size buffers &body body)
  (let ((sizeg (gensym "SIZE")))
    `(let ((,sizeg ,size) ,@buffers)
       (unwind-protect
            (progn
              ,@(loop for buffer in buffers
                      collect `(setf ,buffer (cl-mixed:make-buffer ,sizeg)))
              (let ,(loop for buffer in buffers
                          collect `(,buffer ,buffer))
                ,@body))
         ,@(loop for buffer in buffers
                 collect `(when ,buffer (free ,buffer)))))))
