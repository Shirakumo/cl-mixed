#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defvar *default-samplerate* 44100)

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
    `(let ((,err T))
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (setf ,err NIL))
         (when ,err
           ,cleanup)))))

(defun calloc (type &optional (count 1))
  (let ((ptr (cffi:foreign-alloc type :count count)))
    (dotimes (i (* count (cffi:foreign-type-size type)) ptr)
      (setf (cffi:mem-aref ptr :uchar i) 0))))

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

(defun ptr->vec (value-ptr)
  (let ((value (make-array 3 :initial-element 0.0f0 :element-type 'single-float)))
    (setf (aref value 0) (cffi:mem-aref value-ptr :float 0))
    (setf (aref value 1) (cffi:mem-aref value-ptr :float 1))
    (setf (aref value 2) (cffi:mem-aref value-ptr :float 2))
    value))

(defun vec->ptr (value value-ptr)
  (etypecase value
    (cons
     (destructuring-bind (x y z) value
       (setf (cffi:mem-aref value-ptr :float 0) (coerce x 'single-float))
       (setf (cffi:mem-aref value-ptr :float 1) (coerce y 'single-float))
       (setf (cffi:mem-aref value-ptr :float 2) (coerce z 'single-float))))
    (vector
     (setf (cffi:mem-aref value-ptr :float 0) (coerce (aref value 0) 'single-float))
     (setf (cffi:mem-aref value-ptr :float 1) (coerce (aref value 1) 'single-float))
     (setf (cffi:mem-aref value-ptr :float 2) (coerce (aref value 2) 'single-float)))))

(defmacro define-vector-field-accessor (name class enum)
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (segment (gensym  "SEGMENT"))
        (field (intern (string name) "KEYWORD")))
    `(progn
       (defmethod field ((field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-get ,enum ,value-ptr (handle ,segment)))
           (ptr->vec ,value-ptr)))

       (defmethod (setf field) (,value (field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (vec->ptr ,value ,value-ptr)
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
        (segment (gensym  "SEGMENT")))
    `(progn
       (defmethod input-field ((field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-get-in ,enum ,location ,value-ptr (handle ,segment)))
           (ptr->vec ,value-ptr)))

       (defmethod (setf input-field) (,value (field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float 3)
           (vec->ptr ,value ,value-ptr)
           (with-error-on-failure ()
             (cl-mixed-cffi:segment-set-in ,enum ,location ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,location buffer) (,segment ,class))
         (input-field ,field (position ,location (inputs ,segment)) ,segment))

       (defmethod ,name ((,location integer) (,segment ,class))
         (input-field ,field ,location ,segment))

       (defmethod ,name ((,location source) (,segment ,class))
         (input-field ,field (position ,location (sources ,segment)) ,segment))

       (defmethod (setf ,name) (,value (,location buffer) (,segment ,class))
         (setf (input-field ,field (position ,location (inputs ,segment)) ,segment) ,value))

       (defmethod (setf ,name) (,value (,location integer) (,segment ,class))
         (setf (input-field ,field ,location ,segment) ,value))

       (defmethod (setf ,name) (,value (,location source) (,segment ,class))
         (setf (input-field ,field (position ,location (sources ,segment)) ,segment) ,value)))))

(defmacro define-delegated-slot-accessor (name class accessor)
  (let ((value (gensym "VALUE")))
    `(progn
       (defmethod ,name ((,class ,class))
         (,name (,accessor ,class)))

       (defmethod (setf ,name) (,value (,class ,class))
         (setf (,name (,accessor ,class)) ,value)))))

(defun vector-remove-pos (index vector)
  (loop for i from (1+ index) below (length vector)
        do (setf (aref vector (1- i)) (aref vector i)))
  (setf (aref vector (1- (length vector))) NIL)
  (decf (fill-pointer vector))
  vector)

(defun vector-insert-pos (index element vector)
  (when (<= (length vector) index)
    (adjust-array vector (1+ index))
    (setf (fill-pointer vector) (1+ index)))
  (setf (aref vector index) element)
  vector)

(defun vector-remove (element vector)
  (loop for i from 0 below (length vector)
        do (when (eq element (aref vector i))
             (vector-remove-pos i vector)
             (return)))
  vector)

(defun removef (plist &rest keys)
  (loop for (k v) on plist by #'cddr
        for found = (find k keys)
        unless found collect k
        unless found collect v))
