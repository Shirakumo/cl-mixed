#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defvar *default-samplerate* 44100)
(defvar *c-object-table* (tg:make-weak-hash-table :test 'eql :weakness :value))

(define-condition mixed-error (error)
  ((error-code :initarg :error-code :accessor error-code))
  (:default-initargs :error-code (cl-mixed-cffi:error))
  (:report (lambda (c s) (format s "Mixed error: ~a"
                                 (cl-mixed-cffi:error-string (error-code c))))))

(defmacro with-error-on-failure ((&optional (datum ''mixed-error) &rest args) &body body)
  `(when (= 0 (progn ,@body))
     (error ,datum ,@args)))

(defmacro with-cleanup-on-failure (cleanup &body body)
  (let ((err (gensym "ERROR")))
    `(handler-bind ((error (lambda (,err)
                             (declare (ignore ,err))
                             ,cleanup)))
       ,@body)))

(defun calloc (type &optional (count 1))
  (let ((ptr (cffi:foreign-alloc type :count count)))
    (dotimes (i (* count (cffi:foreign-type-size type)) ptr)
      (setf (cffi:mem-aref ptr :uchar i) 0))))

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
      (setf (gethash (cffi:pointer-address handle) *c-object-table*) object))))

(defmethod free ((object c-object))
  (let ((handle (handle object)))
    (when handle
      (tg:cancel-finalization object)
      (setf (handle object) NIL)
      (funcall (free-handle object handle)))))

(defmethod pointer->object ((pointer integer))
  (gethash integer *c-object-table*))

(defmethod pointer->object (pointer)
  (gethash (cffi:pointer-address pointer) *c-object-table*))

(defmacro define-accessor (name class ffi)
  (let ((value (gensym "VALUE")))
    `(progn
       (defmethod ,name ((,class ,class))
         (,ffi (handle ,class)))
       
       (defmethod (setf ,name) (,value (,class ,class))
         (setf (,ffi (handle ,class)) ,value)))))

(defmacro define-callback (name return args error-return &body body)
  (let ((err (gensym "ERROR")))
    `(cffi:defcallback ,name ,return ,args
       (handler-case
           (progn
             ,@body)
         (error (,err)
           (format T "Error in ~a callback: ~a" ',name ,err)
           ,error-return)))))

(defmacro define-std-callback (name args &body body)
  `(define-callback ,name :int ,args
       0
     (prog1 1
       ,@body)))

(defmacro define-field-accessor (name class type &optional (enum (intern (string name) "KEYWORD")))
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (segment (gensym  "SEGMENT"))
        (field (intern (string name) "KEYWORD")))
    `(progn
       (defmethod field ((field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr ',type)
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-get ,enum ,value-ptr (handle ,segment)))
           (cffi:mem-ref ,value-ptr ',type)))

       (defmethod (setf field) (,value (field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr ',type)
           (setf (cffi:mem-ref ,value-ptr ',type) ,value)
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-get ,enum ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,segment ,class))
         (field ,field ,segment))

       (defmethod (setf ,name) (,value (,segment ,class))
         (setf (field ,field ,segment) ,value)))))

(defmacro define-vector-field-accessor (name class enum)
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (segment (gensym  "SEGMENT"))
        (x (gensym "X")) (y (gensym "Y")) (z (gensym "Z"))
        (field (intern (string name) "KEYWORD")))
    `(progn
       (defmethod field ((field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-get ,enum ,value-ptr (handle ,segment)))
           (list (cffi:mem-ref ,value-ptr :float 0)
                 (cffi:mem-ref ,value-ptr :float 1)
                 (cffi:mem-ref ,value-ptr :float 2))))

       (defmethod (setf field) (,value (field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (destructuring-bind (,x ,y ,z) ,value
             (setf (cffi:mem-ref ,value-ptr :float 0) ,x)
             (setf (cffi:mem-ref ,value-ptr :float 1) ,y)
             (setf (cffi:mem-ref ,value-ptr :float 2) ,z))
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-get ,enum ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,segment ,class))
         (field ,field ,segment))

       (defmethod (setf ,name) (,value (,segment ,class))
         (setf (field ,field ,segment) ,value)))))
