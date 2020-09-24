#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.wav
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi))
  (:export
   #:source))
(in-package #:org.shirakumo.fraf.mixed.wav)

(defun evenify (int)
  (if (evenp int)
      int
      (1+ int)))

(defun decode-int (stream size)
  (let ((int 0))
    (dotimes (i size int)
      (setf (ldb (byte 8 (* 8 i)) int) (read-byte stream)))))

(defun decode-label (stream)
  (map-into (make-string 4) (lambda () (code-char (read-byte stream)))))

(defun check-label (stream label)
  (let ((found (decode-label stream)))
    (unless (string= found label)
      (error "Not a valid RIFF file: encountered ~s instead of ~s."
             found label))))

(defun decode-block-type (type stream)
  (when (string= type "fmt ")
    (list :audio-format (decode-int stream 2)
          :channels (decode-int stream 2)
          :samplerate (decode-int stream 4)
          :byterate (decode-int stream 4)
          :block-align (decode-int stream 2)
          :bits-per-sample (decode-int stream 2))))

(defun decode-block (stream)
  (let ((label (decode-label stream))
        (start (file-position stream))
        (size (decode-int stream 4)))
    (prog1 (list* :label label
                  :start (+ start 4)
                  :end (+ start size)
                  (decode-block-type label stream))
      (file-position stream (evenify (+ start size 4))))))

(defun determine-sample-format (format)
  (case (getf format :audio-format)
    (1 (ecase (/ (getf format :bits-per-sample) 8)
         (1 :uint8)
         (2 :int16)
         (3 :int24)
         (4 :int32)))
    (3 :float)
    (T (error "Unsupported audio format (~d) in file." (getf format :audio-format)))))

(defun decode-wav-header (stream)
  (check-label stream "RIFF")
  (dotimes (i 4) (read-byte stream))
  (check-label stream "WAVE")
  (let* ((blocks (loop for block = (ignore-errors (decode-block stream))
                       while block collect block))
         (format (find "fmt " blocks :key #'second :test #'string=))
         (data (find "data" blocks :key #'second :test #'string=)))
    (unless format
      (error "Format block not found in RIFF file."))
    (unless data
      (error "Data block not found in RIFF file."))
    (file-position stream (getf data :start))
    (values (getf format :channels)
            (getf format :samplerate)
            (determine-sample-format format)
            (getf data :start)
            (getf data :end))))

(defclass source (mixed:source)
  ((file :initarg :file :accessor file)
   (wav-stream :accessor wav-stream)
   (data-start :accessor data-start)
   (data-end :accessor data-end)))

(defmethod initialize-instance :after ((source source) &key)
  (let ((stream (open (file source) :direction :input
                                    :element-type '(unsigned-byte 8))))
    (setf (wav-stream source) stream)
    (multiple-value-bind (channels samplerate encoding start end) (decode-wav-header stream)
      (setf (mixed:samplerate (mixed:pack source)) samplerate)
      (setf (mixed:channels (mixed:pack source)) channels)
      (setf (mixed:encoding (mixed:pack source)) encoding)
      (setf (data-start source) start)
      (setf (data-end source) end))))

(defmethod mixed:free ((source source))
  (when (wav-stream source)
    (close (wav-stream source))
    (setf (wav-stream source) NIL)))

(defmethod mixed:start ((source source)))

(defmethod mixed:mix ((source source))
  (mixed:with-buffer-tx (data start size (mixed:pack source) :direction :output)
    (when (< 0 size)
      (let* ((stream (wav-stream source))
             (avail (min size (- (data-end source) (file-position stream))))
             (read (- (read-sequence data stream :start start :end (+ start avail)) start)))
        (cond ((< 0 read)
               (incf (mixed:byte-position source) read)
               (mixed:finish read))
              ((<= (mixed:available-read (mixed:pack source)) 2)
               (setf (mixed:done-p source) T)))))))

(defmethod mixed:end ((source source))
  (close (wav-stream source)))

(defmethod mixed:seek-to-frame ((source source) position)
  (file-position (wav-stream source)
                 (min (data-end source)
                      (+ (data-start source) (* position (mixed:framesize (mixed:pack source)))))))

(defmethod mixed:frame-count ((source source))
  (/ (- (data-end source) (data-start source)) (mixed:framesize (mixed:pack source))))
