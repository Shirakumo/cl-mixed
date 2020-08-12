#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun call-with-out (function rate channels encoding)
  (let ((out  (out123:connect (out123:make-output NIL))))
    (format T "~&Playback device ~a / ~a" (out123:driver out) (out123:device out))
    (out123:start out :rate rate :channels channels :encoding encoding)
    (unwind-protect
         (funcall function out)
      (out123:stop out)
      (out123:disconnect out))))

(defmacro with-out ((out &key (rate 44100) (channels 2) (encoding :float)) &body body)
  `(call-with-out (lambda (,out) ,@body) ,rate ,channels ,encoding))

(defun call-with-mp3 (function pathname samples)
  (let* ((file (mpg123:connect (mpg123:make-file pathname :buffer-size NIL))))
    (multiple-value-bind (rate channels encoding) (mpg123:file-format file)
      (format T "~&Input format ~a Hz ~a channels ~a encoded." rate channels encoding)
      (setf (mpg123:buffer-size file) (* samples channels (mixed:samplesize encoding)))
      (setf (mpg123:buffer file) (cffi:foreign-alloc :uchar :count (mpg123:buffer-size file)))
      (unwind-protect
           (funcall function file rate channels encoding)
        (mpg123:disconnect file)
        (cffi:foreign-free (mpg123:buffer file))))))

(defmacro with-mp3 ((file rate channels encoding &key pathname samples) &body body)
  `(call-with-mp3 (lambda (,file ,rate ,channels ,encoding) ,@body) ,pathname ,samples))

(defmacro with-edge-setup ((file out rate &key pathname samples) &body body)
  (let ((channels (gensym "CHANNELS"))
        (encoding (gensym "ENCODING")))
    `(with-mp3 (,file ,rate ,channels ,encoding :pathname ,pathname :samples ,samples)
       (with-out (,out :rate ,rate :channels ,channels :encoding ,encoding)
         ,@body))))

(defun play (file out sequence samples)
  (let* ((buffer (mpg123:buffer file))
         (buffersize (mpg123:buffer-size file))
         (read (mpg123:process file)))
    (loop for i from read below buffersize
          do (setf (cffi:mem-aref buffer :uchar i) 0))
    (mixed:mix samples sequence)
    (let ((played (out123:play out buffer buffersize)))
      (when (/= played read)
        (format T "~&Playback is not catching up with input by ~a bytes."
                (- read played))))
    (/= 0 read)))

(defmacro with-sequence ((name &rest segments) &body body)
  `(let ((,name (mixed:make-segment-sequence ,@segments)))
     (mixed:start ,name)
     (unwind-protect
          (flet ((mixed:mix (&optional (,name ,name))
                   (mixed:mix ,name)))
            ,@body)
       (mixed:end ,name))))
