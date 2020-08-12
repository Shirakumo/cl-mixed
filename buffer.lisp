#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass buffer (bip-buffer c-object)
  ((data :reader data)))

(defmethod initialize-instance :after ((buffer buffer) &key size virtual)
  (if virtual
      (setf (mixed:buffer-virtual-p (handle buffer)) 1)
      (let ((data (static-vectors:make-static-vector size :element-type 'single-float))
            (handle (handle buffer)))
        (setf (slot-value buffer 'data) data)
        (setf (mixed:buffer-size handle) size)
        (setf (mixed:buffer-data handle) (static-vectors:static-vector-pointer data)))))

(defun make-buffer (size)
  (make-instance 'buffer :size size))

(defmethod allocate-handle ((buffer buffer))
  (calloc '(:struct mixed:buffer)))

(defmethod free-handle ((buffer buffer) handle)
  (let ((data (unless (mixed:buffer-virtual-p handle)
                (data buffer))))
    (lambda ()
      (when data 
        (static-vectors:free-static-vector data))
      (cffi:foreign-free handle)
      (setf (pointer->object handle) NIL))))

(defmethod clear ((buffer buffer))
  (mixed:clear-buffer (handle buffer)))

(defmethod size ((buffer buffer))
  (length (data buffer)))

(defmethod (setf size) (size (buffer buffer))
  (unless (= size (size buffer))
    (let ((old (data buffer))
          (new (static-vectors:make-static-vector size :element-type 'single-float)))
      (static-vectors:replace-foreign-memory
       (static-vectors:static-vector-pointer new) (static-vectors:static-vector-pointer old)
       (* (cffi:foreign-type-size :float) (length old)))
      (setf (slot-value buffer 'data) new)
      (setf (mixed:buffer-data (handle buffer)) (static-vectors:static-vector-pointer new))
      (setf (mixed:buffer-size (handle buffer)) (length new))
      (tg:cancel-finalization buffer)
      (tg:finalize buffer (free-handle buffer (handle buffer)))
      (static-vectors:free-static-vector old)))
  size)

(defmacro with-buffers (size buffers &body body)
  (let ((sizeg (gensym "SIZE")))
    `(let ((,sizeg ,size) ,@buffers)
       (unwind-protect
            (progn
              ,@(loop for buffer in buffers
                      collect `(setf ,buffer (make-buffer ,sizeg)))
              (let ,(loop for buffer in buffers
                          collect `(,buffer ,buffer))
                ,@body))
         ,@(loop for buffer in buffers
                 collect `(when ,buffer (free ,buffer)))))))

(defmethod transfer ((from buffer) (to buffer))
  (with-error-on-failure ()
    (mixed:transfer-buffer (handle from) (handle to))))
