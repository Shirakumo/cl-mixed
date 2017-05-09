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

(defmethod pointer->object ((pointer integer))
  (gethash pointer *c-object-table*))

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
             (cl-mixed-cffi:segment-set ,enum ,value-ptr (handle ,segment))))
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
             (cl-mixed-cffi:segment-set ,enum ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,segment ,class))
         (field ,field ,segment))

       (defmethod (setf ,name) (,value (,segment ,class))
         (setf (field ,field ,segment) ,value)))))

(defmacro define-input-vector-field-accessor (name class field enum)
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (location (gensym "LOCATION"))
        (segment (gensym  "SEGMENT"))
        (x (gensym "X")) (y (gensym "Y")) (z (gensym "Z")))
    `(progn
       (defmethod input-field ((field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-get-in ,enum ,location ,value-ptr (handle ,segment)))
           (list (cffi:mem-ref ,value-ptr :float 0)
                 (cffi:mem-ref ,value-ptr :float 1)
                 (cffi:mem-ref ,value-ptr :float 2))))

       (defmethod (setf input-field) (,value (field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (destructuring-bind (,x ,y ,z) ,value
             (setf (cffi:mem-ref ,value-ptr :float 0) ,x)
             (setf (cffi:mem-ref ,value-ptr :float 1) ,y)
             (setf (cffi:mem-ref ,value-ptr :float 2) ,z))
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-set-in ,enum ,location ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,location buffer) (,segment ,class))
         (input-field ,field (find ,location (inputs ,segment)) ,segment))

       (defmethod ,name ((,location integer) (,segment ,class))
         (input-field ,field ,location ,segment))

       (defmethod (setf ,name) (,value (,location buffer) (,segment ,class))
         (setf (input-field ,field (find ,location (inputs ,segment)) ,segment) ,value))

       (defmethod (setf ,name) (,value (,location integer) (,segment ,class))
         (setf (input-field ,field ,location ,segment) ,value)))))

(defun vector-remove-pos (index vector)
  (loop for i from (1+ index) below (length vector)
        do (setf (aref vector (1- i)) (aref vector i)))
  (setf (aref vector (1- (length vector))) NIL)
  (decf (fill-pointer vector))
  vector)

(defun vector-insert-pos (index element vector)
  (when (<= index (length vector))
    (adjust-array vector (1+ index)))
  (setf (aref vector index) element)
  (incf (fill-pointer vector))
  vector)

(defun vector-remove (element vector)
  (loop for i from 0 below (length vector)
        do (when (eq element (aref vector i))
             (vector-remove-pos i vector)
             (return)))
  vector)
