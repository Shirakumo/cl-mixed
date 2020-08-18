#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.examples)

(defmacro with-sequence ((name &rest segments) &body body)
  `(let ((,name (mixed:make-segment-sequence ,@segments)))
     (mixed:start ,name)
     (unwind-protect
          (with-simple-restart (abort "Abort playback.")
            (flet ((mixed:mix (&optional (,name ,name))
                     (mixed:mix ,name)))
              ,@body))
       (mixed:end ,name))))
