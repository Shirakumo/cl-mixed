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
  (declare (optimize speed))
  (let ((buf (buffer echo))
        (offset (offset echo))
        (falloff (falloff echo)))
    (declare (type (simple-array single-float (*)) buf)
             (type single-float falloff)
             (type (unsigned-byte 32) offset))
    (mixed:with-buffer-transfer
        (in ins (aref (mixed:inputs echo) 0))
        (out outs (aref (mixed:outputs echo) 0)) size
      (loop for i from 0 below size
            for sample = (aref in (+ i ins))
            for echo = (aref buf offset)
            do (setf (aref out (+ i outs)) (+ sample echo))
               (setf (aref buf offset) (* (+ sample echo) falloff))
               (setf offset (mod (1+ offset) (length buf))))
      (setf (offset echo) offset)
      (mixed:finish size)
      T)))

(defmethod mixed:info ((echo echo))
  (list :name "echo"
        :description "Simple one-channel delay-line echo."
        :flags 0
        :min-inputs 1
        :max-inputs 1
        :outputs 1
        :fields ()))

(defun echo (mp3 &key (samples 500) (delay 0.2) (falloff 0.5))
  (with-edge-setup (file out samplerate :pathname mp3)
    (let* ((source (mixed:make-unpacker samples
                                        (mpg123:encoding file)
                                        (mpg123:channels file)
                                        samplerate))
           (drain (mixed:make-packer samples
                                     (out123:encoding out)
                                     (out123:channels out)
                                     samplerate))
           (echo-l (make-instance 'echo :samplerate samplerate :falloff falloff :delay delay))
           (echo-r (make-instance 'echo :samplerate samplerate :falloff falloff :delay delay)))
      (mixed:with-buffers samples (li ri lo ro)
        (mixed:connect source :left echo-l 0 li)
        (mixed:connect source :right echo-r 0 ri)
        (mixed:connect echo-l :mono drain :left lo)
        (mixed:connect echo-r :mono drain :right ro)
        (with-sequence (sequence source echo-l echo-r drain)
          (loop while (play file out sequence)))))))
