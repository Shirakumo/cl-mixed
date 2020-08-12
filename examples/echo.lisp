#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defclass echo (mixed:virtual)
  ((buffer :initform NIL :accessor buffer)
   (offset :initform 0 :accessor offset)
   (delay :initarg :delay :initform 0.2 :accessor delay)
   (falloff :initarg :falloff :initform 0.8 :accessor falloff)
   (samplerate :initarg :samplerate :initform 44100 :accessor samplerate)))

(defmethod mixed:start ((echo echo))
  (setf (buffer echo) (make-array (ceiling (* (delay echo) (samplerate echo)))
                                  :element-type 'single-float
                                  :initial-element 0.0f0)))

(defmethod mixed:mix ((echo echo))
  (let ((out (mixed:data (aref (mixed:outputs echo) 0)))
        (in (mixed:data (aref (mixed:inputs echo) 0)))
        (buf (buffer echo))
        (offset (offset echo))
        (falloff (falloff echo)))
    (declare (type cffi:foreign-pointer in out))
    ;; Mix
    (loop for i from 0 below samples
          for sample = (cffi:mem-aref in :float i)
          for echo = (aref buf offset)
          do (setf (cffi:mem-aref out :float i) (+ sample echo))
             (setf (aref buf offset) (* (+ sample echo) falloff))
             (setf offset (mod (1+ offset) (length buf))))
    (setf (offset echo) offset)
    T))

(defun echo (mp3 &key (samples 500) (delay 0.2) (falloff 0.8))
  (with-edge-setup (file out samplerate :pathname mp3 :samples samples)
    (let* ((source (mixed:make-unpacker (mpg123:buffer file)
                                        (mpg123:buffer-size file)
                                        (mpg123:encoding file)
                                        (mpg123:channels file)
                                        samplerate))
           (drain (mixed:make-packer (mpg123:buffer file)
                                     (mpg123:buffer-size file)
                                     (out123:encoding out)
                                     (out123:channels out)
                                     samplerate))
           (echo (make-instance 'echo :samplerate samplerate :falloff falloff :delay delay)))
      (mixed:with-buffers samples (li ri lo ro)
        (mixed:connect source :left echo 0 li)
        (setf (mixed:output :right source) ri)
        (mixed:connect echo :left drain :left lo)
        (mixed:connect echo :right drain :right ro)
        (with-sequence (sequence source echo drain)
          (loop while (play file out sequence samples)))))))
