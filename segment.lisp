#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass segment (c-object)
  ())

(defmethod allocate-handle ((segment segment))
  (calloc '(:struct cl-mixed-cffi:segment)))

(defmethod free-handle ((segment segment) handle)
  (lambda ()
    (cl-mixed-cffi:free-segment handle)
    (cffi:foreign-free handle)))

(defmethod input-field (field location (segment segment)))

(defmethod (setf input-field) (value field location (segment segment)))

(defmethod output-field (field location (segment segment)))

(defmethod (setf output-field) (value field location (segment segment)))

(defmethod field (field (segment segment)))

(defmethod (setf field) (field (segment segment)))

(defmethod input (location (segment segment))
  (input-field :buffer location segment))

(defmethod (setf input) ((buffer buffer) location (segment segment))
  (setf (input-field :buffer location segment) buffer))

(defmethod output (location (segment segment))
  (output-field :buffer location segment))

(defmethod (setf output) ((buffer buffer) location (segment segment))
  (setf (output-field :buffer location segment) buffer))

(defclass source (segment)
  ())

(defmethod initialize-instance :after ((source source) &key channel)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-source (handle channel) (handle source))))

(defclass drain (segment)
  ())

(defmethod initialize-instance :after ((drain drain) &key channel)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-drain (handle channel) (handle drain))))

(defclass linear-mixer (segment)
  ())

(defmethod initialize-instance :after ((mixer linear-mixer) &key buffers)
  (cffi:with-foreign-object (buflist :pointer :count (1+ (length buffers)))
    (loop for buffer in buffers
          for i from 0
          do (setf (cffi:mem-aref buflist :pointer i) (handle buffer)))
    (setf (cffi:mem-aref buflist :pointer (length buffers)) (cffi:null-pointer))
    (with-error-on-failure ()
      (cl-mixed-cffi:make-segment-mixer buflist (handle mixer)))))

(defmethod add ((buffer buffer) (mixer linear-mixer))
  )

(defmethod withdraw ((buffer buffer) (mixer linear-mixer))
  )

(defclass general (segment)
  ()
  (:default-initargs
   :volume 1.0
   :pan 0.0))

(defmethod initialize-instance :after ((general general) &key volume pan)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-general volume pan (handle general))))

(defclass fade (segment)
  ()
  (:default-initargs
   :from 0.0
   :to 1.0
   :time 1.0
   :type :cubic-in-out
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((general general) &key from to time type samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-fade from to time type sampelerate (handle general))))

(defclass generator (segment)
  ()
  (:default-initargs
   :type :sine
   :frequency 440
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((generator generator) &key type frequency samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-generator type frequency sampelerate (handle general))))

(defclass ladspa (segment)
  ()
  (:default-initargs
   :file (error "LADSPA FILE required.")
   :index 0
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((ladspa ladspa) &key file index samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-ladspa file index sampelerate (handle general))))

(defclass space (segment)
  ()
  (:default-initargs
   :samplerate *default-samplerate*))

(defmethod initialize-instance :after ((space space) &key samplerate)
  (with-error-on-failure ()
    (cl-mixed-cffi:make-segment-space sampelerate (handle general))))
