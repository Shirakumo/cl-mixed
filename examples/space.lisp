#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun space (mp3 &key (samples 500) (width 100) (height 50) (speed 0.001))
  (with-edge-setup (file out samplerate :pathname mp3)
    (let* ((source (mixed:make-unpacker samples
                                        (mpg123:encoding file)
                                        (mpg123:channels file)
                                        samplerate))
           (drain (mixed:make-packer samples
                                     (out123:encoding out)
                                     (out123:channels out)
                                     samplerate))
           (void (mixed:make-void))
           (space (mixed:make-space-mixer :samplerate samplerate)))
      (mixed:with-buffers samples (li ri lo ro)
        (mixed:connect source :left space 0 li)
        (mixed:connect source :right void 0 ri)
        (mixed:connect space :left drain :left lo)
        (mixed:connect space :right drain :right ro)
        (with-sequence (sequence source void space drain)
          (loop for tt = 0 then (+ tt speed)
                for dx = 0 then (- (* width (sin tt)) x)
                for dz = 0 then (- (* height (cos tt)) z)
                for x = (* width (sin tt)) then (+ x dx)
                for z = (* height (cos tt)) then (+ z dz)
                do (setf (mixed:input-field :location 0 space) (list x 0 z))
                   (setf (mixed:input-field :velocity 0 space) (list dx 0 dz))
                while (play file out sequence)))))))
