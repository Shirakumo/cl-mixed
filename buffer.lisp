#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass buffer (c-object)
  ((data :reader data)))

(defmethod initialize-instance :after ((buffer buffer) &key size)
  (unless size (error "Buffer SIZE required."))
  (let ((data (static-vectors:make-static-vector size :element-type 'float))
        (handle (handle buffer)))
    (setf (mixed:buffer-size handle) size)
    (setf (mixed:buffer-data handle) (static-vectors:static-vector-pointer data))
    (setf (mixed:buffer-virtual-p handle) 1)))

(defun make-buffer (size)
  (etypecase size
    ((integer 1)
     (make-instance 'buffer :size size))))

(defmethod allocate-handle ((buffer buffer))
  (calloc '(:struct cl-mixed-cffi:buffer)))

(defmethod free-handle ((buffer buffer) handle)
  (let ((data (data buffer)))
    (lambda ()
      (static-vectors:free-static-vector data)
      (cffi:foreign-free handle)
      (setf (pointer->object handle) NIL))))

(defmethod clear ((buffer buffer))
  (cl-mixed-cffi:clear-buffer (handle buffer)))

(defmethod size ((buffer buffer))
  (length (data buffer)))

(defmethod (setf size) (new (buffer buffer))
  (let ((old (data buffer))
        (new (static-vectors:make-static-vector new :element-type 'float)))
    (static-vectors:replace-foreign-memory
     (static-vectors:static-vector-pointer new) (static-vectors:static-vector-pointer old)
     (* (cffi:foreign-type-size :float) (length old)))
    (setf (slot-value buffer 'data) new)
    (setf (mixed:buffer-data (handle buffer)) (static-vectors:static-vector-pointer new))
    (setf (mixed:buffer-size (handle buffer)) (length new))
    (static-vectors:free-static-vector old)
    (tg:cancel-finalization buffer)
    (tg:finalize buffer (free-handle buffer (handle buffer))))
  new)

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

(declaim (inline free-for-r2 free-after-r1))
(defun free-for-r2 (handle)
  (- (mixed:buffer-r1-start handle)
     (mixed:buffer-r2-start handle)
     (mixed:buffer-r2-size handle)))

(defun free-after-r1 (handle)
  (- (mixed:buffer-size handle)
     (mixed:buffer-r1-start handle)
     (mixed:buffer-r1-size handle)))

(defmethod available-read ((buffer buffer))
  (declare (optimize speed))
  (mixed:buffer-r1-size (handle buffer)))

(defmethod available-write ((buffer buffer))
  (declare (optimize speed))
  (let ((buffer (handle buffer)))
    (if (< 0 (mixed:buffer-r2-size buffer))
        (free-for-r2 buffer)
        (free-after-r1 buffer))))

(defmethod request-write ((buffer buffer) size)
  (declare (optimize speed))
  (let ((buffer (handle buffer)))
    (cond ((< 0 (mixed:buffer-r2-size buffer))
           (let ((free (min size (free-for-r2 buffer)))
                 (start (+ (mixed:buffer-r2-start buffer) (mixed:buffer-r2-size buffer))))
             (setf (mixed:buffer-reserved-size buffer) free)
             (setf (mixed:buffer-reserved-start buffer) start)
             (values start (+ free start))))
          ((<= (mixed:buffer-r1-start buffer) (free-after-r1 buffer))
           (let ((free (min size (free-after-r1 buffer)))
                 (start (+ (mixed:buffer-r1-start buffer) (mixed:buffer-r1-size buffer))))
             (setf (mixed:buffer-reserved-size buffer) free)
             (setf (mixed:buffer-reserved-start buffer) start)
             (values start (+ start free))))
          (T
           (let ((free (min size (mixed:buffer-r1-start buffer))))
             (values (setf (mixed:buffer-reserved-start buffer) 0)
                     free))))))

(defmethod finish-write ((buffer buffer) size)
  (declare (optimize speed))
  (let ((buffer (handle buffer)))
    (when (< size (mixed:buffer-reserved-size buffer))
      (error "Cannot commit more than was allocated."))
    (cond ((= 0 size))
          ((and (= 0 (mixed:buffer-r1-size buffer))
                (= 0 (mixed:buffer-r2-size buffer)))
           (setf (mixed:buffer-r1-start buffer) (mixed:buffer-r2-start buffer))
           (setf (mixed:buffer-r1-size buffer) size))
          ((= (mixed:buffer-reserved-start buffer) (+ (mixed:buffer-r1-start buffer) (mixed:buffer-r1-size buffer)))
           (incf (mixed:buffer-r1-size buffer) size))
          (T
           (incf (mixed:buffer-r2-size buffer) size)))
    (setf (mixed:buffer-reserved-size buffer) 0)
    (setf (mixed:buffer-reserved-start buffer) 0)))

(defmethod request-read ((buffer buffer) size)
  (declare (optimize speed))
  (let ((buffer (handle buffer)))
    (values (mixed:buffer-r1-start buffer)
            (min size (mixed:buffer-r1-size buffer)))))

(defmethod finish-read ((buffer buffer) size)
  (declare (optimize speed))
  (let ((buffer (handle buffer)))
    (when (< (mixed:buffer-r1-size buffer) size)
      (error "Cannot commit more than was available."))
    (cond ((= (mixed:buffer-r1-size buffer) size)
           (shiftf (mixed:buffer-r1-start buffer) (mixed:buffer-r2-start) 0)
           (shiftf (mixed:buffer-r1-size buffer) (mixed:buffer-r2-size) 0))
          (T
           (decf (mixed:buffer-r1-size buffer) size)
           (incf (mixed:buffer-r1-start buffer) size)))))

(defmacro with-buffer-tx ((data start end buffer &key (direction :read) (size #xFFFFFFFF)) &body body)
  (let ((bufferg (gensym "BUFFER"))
        (sizeg (gensym "SIZE"))
        (handle (gensym "HANDLE")))
    `(let* ((,bufferg ,buffer)
            (,data (data ,bufferg)))
       (ecase ,direction
         (:read
          (multiple-value-bind (,start ,end) (request-read ,bufferg ,size)
            (flet ((finish (,sizeg) (finish-read ,bufferg ,sizeg)))
              ,@body)))
         (:write
          (multiple-value-bind (,start ,end) (request-write ,bufferg ,size)
            (flet ((finish (,sizeg) (finish-write ,bufferg ,sizeg)))
              (unwind-protect
                   (progn ,@body)
                (let ((,handle (handle ,buffer)))
                  (setf (mixed:buffer-reserved-size ,handle) 0)
                  (setf (mixed:buffer-reserved-start ,handle) 0))))))))))

(defmacro with-buffer-transfer ((fdata fstart fend from &optional (size #xFFFFFFFF)) (tdata tstart tend to) &body body)
  `(let* ((,fromg ,from)
          (,tog ,to))
     (if (eq ,fromg ,tog)
         (multiple-value-bind (,fstart ,fend) (request-read ,fromg ,size)
           (let* ((,tstart ,fstart) (,tend ,fend)
                  (,fdata (data ,fromg)) (,tdata ,fdata))
             ,@body))
         (with-buffer-tx (,fdata ,fstart ,fend ,fromg :direction :read :size ,size)
           (with-buffer-tx (,tdata ,tstart ,tend ,tog :direction :write :size (min (- ,fend ,fstart) ,size))
             ,@body)))))
