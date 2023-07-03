(in-package #:org.shirakumo.fraf.mixed)

(defvar *c-object-table* (make-hash-table :test 'eql))

(defmethod handle (thing)
  (etypecase thing
    (cffi:foreign-pointer thing)))

(defclass c-object ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

(defmethod initialize-instance ((object c-object) &key)
  (call-next-method)
  (unless (handle object)
    (let ((handle (allocate-handle object)))
      (setf (handle object) handle)
      (setf (pointer->object handle) object))))

(defmethod initialize-instance :around ((object c-object) &key handle)
  (if handle
      (call-next-method)
      (with-cleanup-on-failure (free object)
        (call-next-method))))

(defmethod free ((object c-object))
  (error "Don't know how to free ~s" object))

(defmethod free :after ((object c-object))
  (when (handle object)
    (setf (pointer->object (handle object)) NIL)
    (cffi:foreign-free (handle object))
    (setf (handle object) NIL)))

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

(defmacro with-objects (bindings &body body)
  `(let ,(mapcar #'first bindings)
     (unwind-protect
          (progn
            ,@(loop for (var init) in bindings
                    collect `(setf ,var ,init))
            (let ,(loop for (var) in bindings
                        collect `(,var ,var))
              ,@body))
       ,@(loop for (var) in bindings
               collect `(when ,var (free ,var))))))
