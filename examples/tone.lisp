#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun tone-frequency (tone)
  (ecase tone
    (_    0.0)
    (C4   261.63)
    ( C#4 277.18)
    (D4   293.66)
    ( D#4 311.13)
    (E4   329.63)
    (F4   349.23)
    ( F#4 369.99)
    (G4   392.00)
    ( G#4 415.30)
    (A4   440.00)
    ( A#4 466.16)
    (B4   493.88)
    (C5   523.25)
    ( C#5 554.37)
    (D5   587.33)
    ( D#5 622.25)
    (E5   659.25)
    (F5   698.46)))

(defun tone (tones &key (type :sine))
  (let ((tones (loop for (tone length) in tones
                     collect (list (find-symbol (string tone) #.*package*) length))))
    (with-out (out)
      (multiple-value-bind (rate channels encoding) (out123:playback-format out)
        (let ((generator (mixed:make-generator :type type))
              (distributor (mixed:make-distributor))
              (drain (mixed:make-packer 100 encoding channels rate)))
          (mixed:with-buffers 100 (mono)
            (mixed:connect generator :mono distributor 0 mono)
            (dotimes (i channels)
              (mixed:connect distributor i drain i NIL))
            (with-sequence (sequence generator distributor drain)
              (loop with time = 0.0
                    for (tone duration) = (first tones)
                    while tones
                    do (setf (mixed:frequency generator) (tone-frequency tone))
                       (mixed:mix)
                       (mixed:with-buffer-tx (data start end (mixed:pack drain))
                         (let ((bytes (out123:play out (mixed:data-ptr) (- end start))))
                           (incf time (/ bytes (mixed:samplesize encoding) rate))
                           (mixed:finish bytes)))
                       (when (<= duration time)
                         (decf time duration)
                         (pop tones))))))))))
