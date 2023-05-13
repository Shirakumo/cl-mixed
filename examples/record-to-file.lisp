#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Gleefre
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun record-to-file (file seconds &key (samplerate 44100) (encoding :float))
  (mixed:with-objects ((input (mixed:make-unpacker :samplerate samplerate))
                       (output (mixed:make-packer :samplerate samplerate :encoding encoding))
                       (file-drain (make-instance 'org.shirakumo.fraf.mixed.wav:file-drain
                                                  :pack output :file file))
                       (mic (make-instance 'org.shirakumo.fraf.mixed.pulse:source
                                           :pack input)))
    (mixed:with-buffers 500 (l r)
      (mixed:connect input :left output :left l)
      (mixed:connect input :right output :right r)
      (mixed:with-chain chain (mic input output file-drain)
        (loop while (< (mixed:frame-position file-drain)
                       (* seconds samplerate))
              do (mixed:mix chain))))))
