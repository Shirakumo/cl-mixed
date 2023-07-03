(in-package #:org.shirakumo.fraf.mixed)

(defun list-segments ()
  (cffi:with-foreign-object (count :uint32)
    (with-error-on-failure ()
      (mixed:list-segments count (cffi:null-pointer)))
    (cffi:with-foreign-object (args :pointer (cffi:mem-ref count :uint32))
      (with-error-on-failure ()
        (mixed:list-segments count args))
      (sort (loop for i from 0 below (cffi:mem-ref count :uint32)
                  collect (cffi:mem-aref args :string i))
            #'string<))))

(defun extract-constructor-info (segment)
  (cffi:with-foreign-objects ((argc :uint32)
                              (args :pointer))
    (with-error-on-failure ()
      (mixed:make-segment-info segment argc args))
    (let ((argc (cffi:mem-ref argc :uint32))
          (args (cffi:mem-ref args :pointer)))
      (loop for i from 0 below argc
            collect (decode-field-info (cffi:mem-aptr args '(:struct mixed:segment-info) i))))))

(defun make-generic-segment (name &rest args &key &allow-other-keys)
  (flet ((find-arg (name)
           (loop for (key val) on args by #'cddr
                 do (when (string-equal name key)
                      (return val))
                 finally (error "An argument named ~s is required but was not provided." name))))
    (let* ((info (extract-constructor-info name))
           (argv (loop for field in info
                       for value = (find-arg (getf field :description))
                       for ptr = (calloc (getf field :type))
                       ;; FIXME: we're not dealing with arrays at all here.
                       do (setf (cffi:mem-aref ptr (getf field :type)) value)
                       collect ptr)))
      (unwind-protect
           (cffi:with-foreign-object (args :pointer (length info))
             (loop for i from 0
                   for arg in argv
                   do (setf (cffi:mem-aref args :pointer i) arg))
             (let ((segment (make-instance 'segment)))
               (with-cleanup-on-failure (free segment)
                 (with-error-on-failure ()
                   (mixed:make-segment name args (handle segment)))
                 segment)))
        (mapc #'cffi:foreign-free argv)))))
