#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass buffer (c-object)
  ())

(defmethod initialize-instance :after ((buffer buffer) &key handle size initial-contents)
  (when initial-contents
    (check-type initial-contents (vector single-float)))
  (when (and initial-contents (null size))
    (setf size (length initial-contents)))
  (unless handle
    (let ((handle (handle buffer)))
      (unless size
        (error "Buffer SIZE required."))
      (with-error-on-failure ()
        (cl-mixed-cffi:make-buffer size handle))))
  (when initial-contents
    (loop with ptr = (data buffer)
          for i from 0 below (min size (length initial-contents))
          do (setf (cffi:mem-aref ptr :float i) (aref initial-contents i)))))

(defun make-buffer (size/initial-contents)
  (etypecase size/initial-contents
    ((integer 1)
     (make-instance 'buffer :size size/initial-contents))
    ((vector single-float)
     (make-instance 'buffer :initial-contents size/initial-contents))))

(defmethod allocate-handle ((buffer buffer))
  (calloc '(:struct cl-mixed-cffi:buffer)))

(defmethod free-handle ((buffer buffer) handle)
  (lambda ()
    (cl-mixed-cffi:free-buffer handle)
    (cffi:foreign-free handle)
    (setf (pointer->object handle) NIL)))

(defmethod clear ((buffer buffer))
  (cl-mixed-cffi:clear-buffer (handle buffer)))

(define-accessor data buffer cl-mixed-cffi:buffer-data)
(define-accessor size buffer cl-mixed-cffi:buffer-size)

(defmethod (setf size) (new (buffer buffer))
  (with-error-on-failure ()
    (cl-mixed-cffi:resize-buffer new (handle buffer))))

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
