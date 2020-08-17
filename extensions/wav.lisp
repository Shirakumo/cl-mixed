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
   #:wav-source))
(in-package #:org.shirakumo.fraf.mixed.wav)

(defclass wav-source (mixed:source)
  ((file :initarg :file :accessor file)
   (wav-stream :accessor wav-stream)
   (data-start :accessor data-start)
   (data-end :accessor data-end)))

(defmethod initialize-instance :after ((source wav-source) &key)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle source)) (cffi:callback mix)))

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

(defun decode-block (stream)
  (let ((start (file-position stream))
        (label (decode-label stream))
        (size (decode-int stream 4)))
    (list* :label label
           :start start
           :size size
           :end (+ start size)
           (cond ((string= label "fmt ")
                  (list :audio-format (decode-int stream 2)
                        :channels (decode-int stream 2)
                        :samplerate (decode-int stream 4)
                        :byterate (decode-int stream 4)
                        :block-align (decode-int stream 2)
                        :bits-per-sample (decode-int stream 2)))
                 (T
                  (file-position stream (evenify (+ start size)))
                  NIL)))))

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
            (getf data :size))))

(defmethod mixed:start ((source wav-source))
  (let ((stream (open (file source) :direction :input
                                    :element-type '(unsigned-byte 8))))
    (multiple-value-bind (channels samplerate encoding start size) (decode-wav-header stream)
      (setf (mixed:samplerate (mixed:pack source)) samplerate)
      (setf (mixed:channels (mixed:pack source)) channels)
      (setf (mixed:encoding (mixed:pack source)) encoding)
      (setf (data-start source) start)
      (setf (data-end source) (+ start size)))))

(cffi:defcallback mix :int ((segment :pointer))
  (let* ((source (mixed:pointer->object segment))
         (stream (wav-stream source)))
    (mixed:with-buffer-tx (data start end (mixed:pack source) :direction :output)
      (let* ((avail (min (- end start) (- (data-end source) (file-position stream))))
             (read (- (read-sequence data stream :start start :end (+ start avail)) start)))
        (incf (mixed:byte-position source) read)
        (mixed:finish read)))
    1))

(defmethod mixed:end ((source wav-source))
  (close (file source)))

(defmethod mixed:seek-to-frame ((source wav-source) position)
  (file-position (wav-stream source)
                 (min (data-end source)
                      (+ (data-start source) (* position (mixed:framesize (mixed:pack source)))))))

(defmethod mixed:frame-count ((source wav-source))
  (/ (- (data-end source) (data-start source)) (mixed:framesize (mixed:pack source))))
