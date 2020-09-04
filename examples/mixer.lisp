#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defun mixer (files &key (samplerate 44100) (output 'org.shirakumo.fraf.mixed.out123:drain))
  (mixed:with-objects ((drain (mixed:make-packer :samplerate samplerate))
                       (out (make-instance output :pack drain))
                       (mixer (mixed:make-basic-mixer 2))
                       (chain (mixed:make-chain)))
    (let ((objects ()))
      (unwind-protect
           (progn
             (loop for file in files
                   for source = (mixed:make-unpacker :samplerate samplerate)
                   for decoder = (make-instance 'org.shirakumo.fraf.mixed.mpg123:source :file file :pack source)
                   for l = (mixed:make-buffer 500)
                   for r = (mixed:make-buffer 500)
                   do (setf objects (list* source decoder l r objects))
                      (mixed:connect source :left mixer T l)
                      (mixed:connect source :right mixer T r)
                      (mixed:add decoder chain)
                      (mixed:add source chain))
             (mixed:with-buffers 500 (l r)
               (mixed:connect mixer :left drain :left l)
               (mixed:connect mixer :right drain :right r)
               (mixed:add mixer chain)
               (mixed:add drain chain)
               (mixed:add out chain)
               (mixed:start chain)
               (unwind-protect
                    (loop (mixed:mix chain))
                 (mixed:end chain))))
        (dolist (object objects)
          (mixed:free object))))))
