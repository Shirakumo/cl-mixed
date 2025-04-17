(in-package #:org.shirakumo.fraf.mixed)

(defclass chain (segment)
  ((segments :initform (make-array 0 :adjustable T :fill-pointer T) :reader segments)))

(defmethod initialize-instance :after ((segment chain) &key segments (bypass NIL bypass-p))
  (with-error-on-failure ()
    (mixed:make-segment-chain (handle segment)))
  (map NIL (lambda (s) (add s segment)) segments)
  (when bypass-p (setf (bypass segment) bypass)))

(defun make-chain (&rest segments)
  (make-instance 'chain :segments segments))

(define-field-accessor bypass chain :bool :bypass)

(defmethod add ((segment segment) (chain chain))
  (with-error-on-failure ()
    (mixed:chain-add (handle segment) (handle chain)))
  (vector-push-extend segment (segments chain))
  segment)

(defmethod withdraw ((i integer) (chain chain))
  (when (handle chain)
    (with-error-on-failure ()
      (mixed:chain-remove-at i (handle chain))))
  (let ((segment (aref (segments chain) i)))
    (vector-remove-pos i (segments chain))
    segment))

(defmethod withdraw ((segment segment) (chain chain))
  (when (handle chain)
    (with-error-on-failure ()
      (mixed:chain-remove (handle segment) (handle chain))))
  (vector-remove segment (segments chain))
  segment)

(defmethod clear ((chain chain))
  (loop for i downfrom (1- (length (segments chain))) to 0
        do (when (handle chain)
             (with-error-on-failure ()
               (mixed:chain-remove-at i (handle chain))))
           (setf (aref (segments chain) i) NIL))
  (setf (fill-pointer (segments chain)) 0))

(defmacro with-chain (name segments &body body)
  `(with-objects ((,name (make-chain ,@segments)))
     (start ,name)
     (unwind-protect
          (let ((,name ,name))
            (flet ((mix (&optional (,name ,name))
                     (mix ,name)))
              ,@body))
       (end ,name))))
