#|
This file is a part of cl-mixed
(c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass bip-buffer ()
  ())

;;;; FIXME: !!! THIS IS NOT YET PORTED RIGHT !!!

(defmacro with-buffer-fields ((read write full-r2) buffer &body body)
  `(let* ((,buffer (handle ,buffer))
          (,read (mixed:buffer-read ,buffer))
          (,write (mixed:buffer-write ,buffer))
          (,full-r2 (logbitp 31 ,write))
          (,write (ldb (byte 31 0) ,write)))
     (declare (type cffi:foreign-pointer ,buffer))
     ,@body))

(declaim (ftype (function (bip-buffer) (unsigned-byte 32)) available-read))
(defun available-read (buffer)
  (declare (optimize speed))
  (with-buffer-fields (read write full-r2) buffer
    (if full-r2
        (if (< read size)
            (- size read)
            write)
        (- write read))))

(declaim (ftype (function (bip-buffer) (unsigned-byte 32)) available-write))
(defun available-write (buffer)
  (declare (optimize speed))
  (with-buffer-fields (read write full-r2) buffer
    (cond (full-r2
           (- read write))
          ((= write (mixed:buffer-size buffer))
           read)
          (- (mixed:buffer-size buffer) write))))

(declaim (ftype (function (bip-buffer (unsigned-byte 32)) (values (unsigned-byte 32) (unsigned-byte 32))) request-write))
(defun request-write (buffer size)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) size))
  (with-buffer-fields (read write full-r2) buffer
    (cond ((not full-r2)
           (let ((available (- (mixed:buffer-size buffer) write)))
             (cond ((< 0 available)
                    (setf (mixed:buffer-reserved buffer) (min size available))
                    (values write (mixed:buffer-reserved buffer)))
                   ((< 0 read)
                    (setf (mixed:buffer-reserved buffer) (min size read))
                    (setf (mixed:buffer-write buffer) #x8000000)
                    (values 0 (mixed:buffer-reserved buffer)))
                   (T
                    (values 0 0)))))
          ((< write read)
           (setf (mixed:buffer-reserved buffer) (min size (- read write)))
           (values write (mixed:buffer-reserved buffer)))
          (T
           (values 0 0)))))

(defun finish-write (buffer size)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) size))
  (let ((buffer (handle buffer)))
    (when (< (mixed:buffer-reserved buffer) size)
      (error "Overcommit."))
    (incf (mixed:buffer-write buffer) size)
    (setf (mixed:buffer-reserved buffer) 0)))

(declaim (ftype (function (bip-buffer (unsigned-byte 32)) (values (unsigned-byte 32) (unsigned-byte 32))) request-read))
(defun request-read (buffer size)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) size))
  (with-buffer-fields (read write full-r2) buffer
    (let ((available (if full-r2
                         (- (mixed:buffer-size buffer) read)
                         (- (mixed:buffer-write buffer) read))))
      (cond ((< 0 available)
             (values read (min size available)))
            (full-r2
             (setf (mixed:buffer-read buffer) 0)
             (values 0 (min size (mixed:buffer-write buffer))))
            (T
             (values 0 0))))))

(defun finish-read (buffer size)
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) size))
  (with-buffer-fields (read write full-r2) buffer
    (when (< (if full-r2
                 (- (mixed:buffer-size buffer) read)
                 (- write read))
             size)
      (error "Overcommit."))
    (setf (mixed:buffer-read buffer) (+ read size))))

(declaim (inline data-ptr))
(defun data-ptr (data &optional (start 0))
  (declare (type (unsigned-byte 32) start))
  (static-vectors:static-vector-pointer data :offset start))

(defmacro with-buffer-tx ((data start size buffer &key (direction :input) ((:size initial-size) #xFFFFFFFF)) &body body)
  (let ((bufferg (gensym "BUFFER"))
        (sizeg (gensym "SIZE")))
    `(let* ((,bufferg ,buffer)
            (,data (data ,bufferg)))
       (declare (type (simple-array * (*)) ,data))
       ,(ecase direction
          ((:input :read)
           `(multiple-value-bind (,start ,size) (request-read ,bufferg ,initial-size)
              (declare (ignorable ,start ,size))
              (flet ((finish (,sizeg) (finish-read ,bufferg ,sizeg))
                     (data-ptr (&optional (,data ,data) (,start ,start)) (data-ptr ,data ,start)))
                (declare (ignorable #'finish #'data-ptr))
                ,@body)))
          ((:output :write)
           `(multiple-value-bind (,start ,size) (request-write ,bufferg ,initial-size)
              (declare (ignorable ,start ,size))
              (flet ((finish (,sizeg) (finish-write ,bufferg ,sizeg))
                     (data-ptr (&optional (,data ,data) (,start ,start)) (data-ptr ,data ,start)))
                (declare (ignorable #'finish #'data-ptr))
                (unwind-protect
                     (progn ,@body)
                  (setf (mixed:buffer-reserved (handle ,buffer)) 0)))))))))

(defmacro with-buffer-transfer ((fdata fstart from) (tdata tstart to) size &body body)
  (let ((fromg (gensym "FROM"))
        (tog (gensym "TO"))
        (tend (gensym "TEND"))
        (fend (gensym "FEND")))
    `(let* ((,fromg ,from)
            (,tog ,to))
       (if (eq ,fromg ,tog)
           (multiple-value-bind (,fstart ,size) (request-read ,fromg #xFFFFFFFF)
             (let* ((,tstart ,fstart)
                    (,fdata (data ,fromg))
                    (,tdata ,fdata))
               (declare (type (simple-array * (*)) ,fdata ,tdata))
               (declare (type (unsigned-byte 32) ,tstart ,size))
               (flet ((finish (,size) (declare (ignore ,size))))
                 ,@body)))
           (with-buffer-tx (,fdata ,fstart ,size ,fromg :direction :read)
             (with-buffer-tx (,tdata ,tstart ,size ,tog :direction :write :size ,size)
               (flet ((finish (,size)
                        (finish-read ,fromg ,size)
                        (finish-write ,tog ,size)))
                 (declare (ignorable #'finish))
                 ,@body)))))))
