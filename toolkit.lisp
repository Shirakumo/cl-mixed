#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defvar *default-samplerate* 44100)
(defvar *default-channel-order* '(:left-front-bottom :right-front-bottom :left-rear-bottom :right-rear-bottom :center-front :subwoofer :left-side :right-side :left-front-top :right-front-top :left-rear-top :right-rear-top :center-rear))

(define-condition mixed-error (error)
  ((error-code :initarg :error-code :accessor error-code))
  (:default-initargs :error-code (mixed:error))
  (:report (lambda (c s) (format s "Mixed error: ~a"
                                 (mixed:error-string (error-code c))))))

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
       #-cl-mixed-no-restarts
       (handler-case
           (loop
             (restart-case
                 (handler-bind ((error #'invoke-debugger))
                   (return
                     (locally
                         ,@body)))
               (abort (&optional e)
                 :report "Abort the callback and report an error."
                 (declare (ignore e))
                 (return ,error-return))
               (continue (&optional e)
                 :report "Abort the callback and ignore the error."
                 (declare (ignore e))
                 (return 1))
               (retry (&optional e)
                 :report "Retry the callback."
                 (declare (ignore e)))))
         (error (,err)
           (format T "~&Error in ~a callback: ~a~%" ',name ,err)
           ,error-return))
       #+cl-mixed-no-restarts
       ,@body)))

(defmacro define-std-callback (name args &body body)
  `(define-callback ,name :int ,args
       0
     (locally ,@body
       1)))

(defun coerce-ctype (value type)
  (case type
    (:float (coerce value 'single-float))
    (:double (coerce value 'double-float))
    (:int (round value))
    (:uint (max 0 (round value)))
    (T value)))

(defmacro define-field-reader (name class type &optional (enum (intern (string name) "KEYWORD")))
  (let ((value-ptr (gensym "VALUE-PTR"))
        (segment (gensym  "SEGMENT"))
        (field (intern (string name) "KEYWORD")))
    `(progn
       (defmethod field ((field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr ',type)
           (with-error-on-failure ()
             (mixed:segment-get ,enum ,value-ptr (handle ,segment)))
           (cffi:mem-ref ,value-ptr ',type)))

       (defmethod ,name ((,segment ,class))
         (field ,field ,segment)))))

(defmacro define-field-writer (name class type &optional (enum (intern (string name) "KEYWORD")))
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (segment (gensym  "SEGMENT"))
        (field (intern (string name) "KEYWORD")))
    `(progn
       (defmethod (setf field) (,value (field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr ',type)
           (setf (cffi:mem-ref ,value-ptr ',type) (coerce-ctype ,value ',type))
           (with-error-on-failure ()
             (mixed:segment-set ,enum ,value-ptr (handle ,segment))))
         ,value)

       (defmethod (setf ,name) (,value (,segment ,class))
         (setf (field ,field ,segment) ,value)))))

(defmacro define-field-accessor (name class type &optional (enum (intern (string name) "KEYWORD")))
  `(progn
     (define-field-reader ,name ,class ,type ,enum)
     (define-field-writer ,name ,class ,type ,enum)))

(defmacro define-input-field-accessor (name class field enum type)
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (location (gensym "LOCATION"))
        (segment (gensym  "SEGMENT")))
    `(progn
       (defmethod input-field ((field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr ,type)
           (with-error-on-failure ()
             (mixed:segment-get-in ,enum ,location ,value-ptr (handle ,segment)))
           (cffi:mem-ref ,value-ptr ,type)))

       (defmethod (setf input-field) (,value (field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr ,type)
           (setf (cffi:mem-ref ,value-ptr ,type) ,value)
           (with-error-on-failure ()
             (mixed:segment-set-in ,enum ,location ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,location buffer) (,segment ,class))
         (input-field ,field (position ,location (inputs ,segment)) ,segment))

       (defmethod ,name ((,location integer) (,segment ,class))
         (input-field ,field ,location ,segment))

       (defmethod (setf ,name) (,value (,location buffer) (,segment ,class))
         (setf (input-field ,field (position ,location (inputs ,segment)) ,segment) ,value))

       (defmethod (setf ,name) (,value (,location integer) (,segment ,class))
         (setf (input-field ,field ,location ,segment) ,value)))))

(defun ptr->vec (value-ptr &optional (size 3))
  (let ((value (make-array size :initial-element 0.0f0 :element-type 'single-float)))
    (dotimes (i size value)
      (setf (aref value i) (cffi:mem-aref value-ptr :float i)))))

(defun vec->ptr (value value-ptr &optional (size 3))
  (etypecase value
    (cons
     (loop for i from 0 below size
           for v in value
           do (setf (cffi:mem-aref value-ptr :float i) (coerce v 'single-float))))
    (vector
     (dotimes (i size)
       (setf (cffi:mem-aref value-ptr :float i) (coerce (aref value i) 'single-float))))))

(defmacro define-vector-field-accessor (name class enum &optional (size 3))
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (segment (gensym  "SEGMENT"))
        (field (intern (string name) "KEYWORD")))
    `(progn
       (defmethod field ((field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float ,size)
           (with-error-on-failure ()
             (mixed:segment-get ,enum ,value-ptr (handle ,segment)))
           (ptr->vec ,value-ptr ,size)))

       (defmethod (setf field) (,value (field (eql ,field)) (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float ,size)
           (vec->ptr ,value ,value-ptr ,size)
           (with-error-on-failure ()
             (mixed:segment-set ,enum ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,segment ,class))
         (field ,field ,segment))

       (defmethod (setf ,name) (,value (,segment ,class))
         (setf (field ,field ,segment) ,value)))))

(defmacro define-input-vector-field-accessor (name class field enum &optional (size 3))
  (let ((value-ptr (gensym "VALUE-PTR"))
        (value (gensym "VALUE"))
        (location (gensym "LOCATION"))
        (segment (gensym  "SEGMENT")))
    `(progn
       (defmethod input-field ((field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float ,size)
           (with-error-on-failure ()
             (mixed:segment-get-in ,enum ,location ,value-ptr (handle ,segment)))
           (ptr->vec ,value-ptr ,size)))

       (defmethod (setf input-field) (,value (field (eql ,field)) ,location (,segment ,class))
         (cffi:with-foreign-object (,value-ptr :float ,size)
           (vec->ptr ,value ,value-ptr ,size)
           (with-error-on-failure ()
             (mixed:segment-set-in ,enum ,location ,value-ptr (handle ,segment))))
         ,value)

       (defmethod ,name ((,location buffer) (,segment ,class))
         (input-field ,field (position ,location (inputs ,segment)) ,segment))

       (defmethod ,name ((,location integer) (,segment ,class))
         (input-field ,field ,location ,segment))

       (defmethod (setf ,name) (,value (,location buffer) (,segment ,class))
         (setf (input-field ,field (position ,location (inputs ,segment)) ,segment) ,value))

       (defmethod (setf ,name) (,value (,location integer) (,segment ,class))
         (setf (input-field ,field ,location ,segment) ,value)))))

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
  (when (array-has-fill-pointer-p vector)
    (decf (fill-pointer vector)))
  vector)

(defun vector-insert-pos (index element vector)
  (when (<= (length vector) index)
    (unless (adjustable-array-p vector)
      (error "Cannot insert element at position ~d:~%Index is out of range, and vector is not adjustable."
             index))
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

(declaim (inline samplesize))
(defun samplesize (type)
  (ecase type
    ((:int8 :uint8) 1)
    ((:int16 :uint16) 2)
    ((:int24 :uint24) 3)
    ((:int32 :uint32) 4)
    (:float 4)
    (:double 8)))

(defmacro do-sequence ((i el sequence &optional result) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (let ((,i 0))
         (flet ((,thunk (,el)
                  (progn ,@body)
                  (incf ,i)))
           (map NIL #',thunk ,sequence))
         ,result))))

(defun guess-channel-order-from-count (channels)
  (ecase channels
    (1 '(:center)) ; mono
    (2 '(:left-front :right-front)) ; stereo
    (3 '(:left-front :right-front :center)) ; 3.0
    (4 '(:left-front :right-front :left-rear :right-rear)) ; 4.0
    (5 '(:left-front :right-front :center :left-rear :right-rear)) ; 5.0
    (6 '(:left-front :right-front :center :subwoofer :left-rear :right-rear)) ; 5.1
    (7 '(:left-front :right-front :center :subwoofer :center-rear :left-side :right-side)) ; 6.1
    (8 '(:left-front :right-front :center :subwoofer :left-rear :right-rear :left-side :right-side)) ; 7.1
    (9 '(:left-front :right-front :center :left-rear :right-rear :left-side :right-side :left-front-top :right-front-top)) ; 9.0
    (10 '(:left-front :right-front :center :subwoofer :left-rear :right-rear :left-side :right-side :left-front-top :right-front-top)) ; 9.1
    ))
