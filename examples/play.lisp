#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun play (file &key (samples 500) (output 'org.shirakumo.fraf.mixed.out123:drain))
  (mixed:with-objects ((source (mixed:make-unpacker samples :float 2 44100))
                       (drain (mixed:make-packer samples :float 2 44100))
                       (mp3 (make-instance 'org.shirakumo.fraf.mixed.mpg123:source :file file :pack source))
                       (out (make-instance output :pack drain)))
    (mixed:with-buffers samples (l r)
      (mixed:connect source :left drain :left l)
      (mixed:connect source :right drain :right r)
      (mixed:start out)
      (mixed:with-chain chain (mp3 source drain out)
        (format T "~&Playing back on ~d channels @ ~dHz, ~a~%"
                (mixed:channels drain) (mixed:samplerate drain) (mixed:encoding drain))
        (loop until (mixed:done-p mp3)
              do (mixed:mix chain))))))
