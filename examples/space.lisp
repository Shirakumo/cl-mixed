#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun space (file &key (samples 500) (width 100) (height 50) (speed 0.001) pitch-shift (output 'org.shirakumo.fraf.mixed.out123:drain))
  (mixed:with-objects ((source (mixed:make-unpacker samples :float 2 44100))
                       (drain (mixed:make-packer samples :float 2 44100))
                       (void (mixed:make-void))
                       (space (mixed:make-space-mixer :samplerate 44100))
                       (mp3 (make-instance 'org.shirakumo.fraf.mixed.mpg123:source :file file :pack source))
                       (out (make-instance output :pack drain)))
    (mixed:with-buffers samples (li ri lo ro)
      (mixed:connect source :left space 0 li)
      (mixed:connect source :right void 0 ri)
      (mixed:connect space :left drain :left lo)
      (mixed:connect space :right drain :right ro)
      (mixed:with-chain chain (mp3 source void space drain out)
        (loop for tt = 0 then (+ tt speed)
              for dx = 0 then (- (* width (sin tt)) x)
              for dz = 0 then (- (* height (cos tt)) z)
              for x = (* width (sin tt)) then (+ x dx)
              for z = (* height (cos tt)) then (+ z dz)
              until (mixed:done-p mp3)
              do (setf (mixed:input-field :location 0 space) (list x 0 z))
                 (when pitch-shift
                   (setf (mixed:input-field :velocity 0 space) (list dx 0 dz)))
                 (mixed:mix chain))))))
