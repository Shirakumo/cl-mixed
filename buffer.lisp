(in-package #:org.shirakumo.fraf.mixed)

(defclass buffer (bip-buffer c-object)
  ((data :reader data)))

(defmethod initialize-instance :after ((buffer buffer) &key size virtual)
  (if virtual
      (setf (mixed:buffer-virtual-p (handle buffer)) 1)
      (let ((data (static-vectors:make-static-vector size :element-type 'single-float :initial-element 0f0))
            (handle (handle buffer)))
        (setf (slot-value buffer 'data) data)
        (setf (mixed:buffer-size handle) size)
        (setf (mixed:buffer-data handle) (static-vectors:static-vector-pointer data)))))

(defmethod describe-object :after ((buffer buffer) stream)
  (format stream "~&
Size:     ~,,'',3:d sample~:p
To read:  ~,,'',3:d sample~:p
To write: ~,,'',3:d sample~:p"
          (size buffer)
          (available-read buffer)
          (available-write buffer)))

(defun make-buffer (size)
  (make-instance 'buffer :size size))

(defmethod allocate-handle ((buffer buffer))
  (calloc '(:struct mixed:buffer)))

(defmethod free ((buffer buffer))
  (unless (mixed:buffer-virtual-p (handle buffer))
    (when (slot-boundp buffer 'data)
      (static-vectors:free-static-vector (data buffer))
      (slot-makunbound buffer 'data))))

(defmethod clear ((buffer buffer))
  (mixed:clear-buffer (handle buffer)))

(defmethod size ((buffer buffer))
  (length (data buffer)))

(defmethod (setf size) (size (buffer buffer))
  (unless (= size (size buffer))
    (let ((old (data buffer))
          (new (static-vectors:make-static-vector size :element-type 'single-float)))
      (static-vectors:replace-foreign-memory
       (static-vectors:static-vector-pointer new) (static-vectors:static-vector-pointer old)
       (* (cffi:foreign-type-size :float) (length old)))
      (setf (slot-value buffer 'data) new)
      (setf (mixed:buffer-data (handle buffer)) (static-vectors:static-vector-pointer new))
      (setf (mixed:buffer-size (handle buffer)) (length new))
      (static-vectors:free-static-vector old)))
  size)

(defmacro with-buffers (size buffers &body body)
  (let ((sizeg (gensym "SIZE")))
    `(let ((,sizeg ,size) ,@buffers)
       (unwind-protect
            (progn
              ,@(loop for buffer in buffers
                      collect `(setf ,buffer (make-buffer ,sizeg)))
              (let ,(loop for buffer in buffers
                          collect `(,buffer ,buffer))
                ,@body))
         ,@(loop for buffer in buffers
                 collect `(when ,buffer (free ,buffer)))))))

(defmethod transfer ((from buffer) (to buffer))
  (with-error-on-failure ()
    (mixed:transfer-buffer (handle from) (handle to))))

(defmethod framesize ((buffer buffer))
  (samplesize :float))

(defun forward-fft (framesize in out)
  (with-error-on-failure ()
    (mixed:fwd-fft framesize (data-ptr in) (data-ptr out))))

(defun inverse-fft (framesize in out)
  (with-error-on-failure ()
    (mixed:inv-fft framesize (data-ptr in) (data-ptr out))))
