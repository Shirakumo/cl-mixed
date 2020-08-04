#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defun extract-constructor-info (segment)
  (cffi:with-foreign-objects ((argc 'size_t)
                              (args :pointer))
    (with-error-on-failure ()
      (mixed:make-segment-info segment argc args))
    (let ((argc (cffi:mem-ref argc 'size_t))
          (args (cffi:mem-ref args :pointer)))
      (loop for i from 0 below argc
            collect (decode-field-info (cffi:mem-aptr args '(:struct mixed:segment-info) i))))))

(defun make-generic-segment (name &rest args &key &allow-other-keys)
  (flet ((find-arg (name)
           (loop for (key val) on args by #'cddr
                 do (when (string-equal name key)
                      (return val))
                 finally (error "An argument named ~s is required but was not provided." name))))
    (let ((info (extract-constructor-info name))
          (argv (loop for field in info
                      for value = (find-arg (getf field :description))
                      for ptr = (calloc (ctype (getf field :type)))
                      ;; FIXME: we're not dealing with arrays at all here.
                      do (setf (cffi:mem-aref ptr (ctype (getf field :type))) value)
                      collect ptr)))
      (unwind-protect
           (cffi:with-foreign-object ((args :pointer (length info)))
             (loop for i from 0
                   for arg in argv
                   do (setf (cffi:mem-aptr args :pointer i) arg))
             (let ((segment (make-instance 'segment)))
               (with-cleanup-on-failure (free segment)
                 (with-error-on-failure ()
                   (mixed:make-segment name args (handle segment)))
                 segment)))
        (mapc #'cffi:foreign-free argv)))))
