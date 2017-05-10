#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defvar *c-object-table* (tg:make-weak-hash-table :test 'eql :weakness :value))

(defmethod handle (thing)
  (etypecase thing
    (cffi:foreign-pointer thing)))

(defclass c-object ()
  ((handle :initarg :handle :accessor handle)))

(defmethod initialize-instance :after ((object c-object) &key)
  (unless (handle object)
    (let ((handle (allocate-handle object)))
      (setf (handle object) handle)
      (tg:finalize object (free-handle object handle))
      (setf (pointer->object handle) object))))

(defmethod initialize-instance :around ((object c-object) &key handle)
  (if handle
      (call-next-method)
      (with-cleanup-on-failure (free object)
        (call-next-method))))

(defmethod free ((object c-object))
  (let ((handle (handle object)))
    (when handle
      (tg:cancel-finalization object)
      (setf (handle object) NIL)
      (funcall (free-handle object handle)))))

(defun pointer->object (pointer)
  (let ((address (etypecase pointer
                   (cffi:foreign-pointer (cffi:pointer-address pointer))
                   (integer pointer))))
    (gethash address *c-object-table*)))

(defun (setf pointer->object) (object pointer)
  (let ((address (etypecase pointer
                   (cffi:foreign-pointer (cffi:pointer-address pointer))
                   (integer pointer))))
    (if object
        (setf (gethash address *c-object-table*) object)
        (remhash address *c-object-table*))))
