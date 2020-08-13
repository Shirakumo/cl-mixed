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

(defun call-with-mp3 (function pathname)
  (let* ((file (mpg123:connect (mpg123:make-file pathname :buffer-size NIL))))
    (multiple-value-bind (rate channels encoding) (mpg123:file-format file)
      (format T "~&Input format ~a Hz ~a channels ~a encoded." rate channels encoding)
      (unwind-protect
           (funcall function file rate channels encoding)
        (mpg123:disconnect file)))))

(defmacro with-mp3 ((file rate channels encoding &key pathname) &body body)
  `(call-with-mp3 (lambda (,file ,rate ,channels ,encoding) ,@body) ,pathname))

(defmacro with-edge-setup ((file out rate &key pathname) &body body)
  (let ((channels (gensym "CHANNELS"))
        (encoding (gensym "ENCODING")))
    `(with-mp3 (,file ,rate ,channels ,encoding :pathname ,pathname)
       (with-out (,out :rate ,rate :channels ,channels :encoding ,encoding)
         ,@body))))

(defmacro with-sequence ((name &rest segments) &body body)
  `(let ((,name (mixed:make-segment-sequence ,@segments)))
     (mixed:start ,name)
     (unwind-protect
          (flet ((mixed:mix (&optional (,name ,name))
                   (mixed:mix ,name)))
            ,@body)
       (mixed:end ,name))))

(defun play (file out sequence)
  (let ((packer (aref (mixed:segments sequence) 0))
        (unpacker (aref (mixed:segments sequence) (1- (length (mixed:segments sequence)))))
        (played 0) (read 0))
    (mixed:with-buffer-tx (data start end (mixed:pack packer) :direction :output)
      (mixed:finish (setf read (mpg123:read-directly file (mixed:data-ptr) (- end start)))))
    (mixed:mix sequence)
    (mixed:with-buffer-tx (data start end (mixed:pack unpacker) :direction :input)
      (mixed:finish (setf played (out123:play-directly out (mixed:data-ptr) (- end start)))))
    (or (< 0 played) (< 0 read))))
