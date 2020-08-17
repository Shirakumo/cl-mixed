#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun play (mp3 &key (samples 500))
  (let* ((source (mixed:make-unpacker samples :float 2 44100))
         (drain (mixed:make-packer samples :float 2 44100))
         (mp3 (make-instance 'org.shirakumo.fraf.mixed.mpg123:mpg123-source :file mp3 :pack source))
         (out (make-instance 'org.shirakumo.fraf.mixed.out123:out123-drain :pack drain)))
    (mixed:with-buffers samples (l r)
      (mixed:connect source :left drain :left l)
      (mixed:connect source :right drain :right r)
      (with-sequence (sequence mp3 source drain out)
        (loop (mixed:mix sequence))))))
