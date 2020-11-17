#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed)

(defclass bundle (virtual)
  ((segments :reader segments)))

(defmethod initialize-instance :after ((bundle bundle) &key segment-class segment-initargs (channels 2))
  (let ((segments (make-array channels :initial-element NIL)))
    (when (<= channels 0) (error "Cannot make a bundle with less than 1 channel."))
    (when segment-class
      (loop for i from 0 below channels
            for segment = (apply #'make-instance segment-class segment-initargs)
            do (unless (and (= 1 (getf (info segment) :outputs))
                            (<= 1 (getf (info segment) :max-inputs)))
                 (unwind-protect (error "The segment does not have a 1:1 in/out mapping and can't be used for a bundle.")
                   (free segment)))
               (setf (aref segments i) segment)))
    (setf (slot-value bundle 'segments) segments)))

(defun make-bundle (channels &optional class &rest initargs)
  (make-instance 'bundle :segment-class class :segment-initargs initargs :channels channels))

(defmethod inputs ((bundle bundle))
  (map 'vector (lambda (s) (aref (inputs s) 0)) (segments bundle)))

(defmethod outputs ((bundle bundle))
  (map 'vector (lambda (s) (aref (outputs s) 0)) (segments bundle)))

(defmethod info ((bundle bundle))
  (if (aref (segments bundle) 0)
      (let ((info (info (aref (segments bundle) 0))))
        (setf (getf info :min-inputs) (length (segments bundle)))
        (setf (getf info :max-inputs) (length (segments bundle)))
        (setf (getf info :outputs) (length (segments bundle)))
        info)
      (list :name "bundle"
            :description "Bundle of segments"
            :flags ()
            :min-inputs 0
            :max-inputs 0
            :outputs 0
            :fields ())))

(defmethod start ((bundle bundle))
  (loop for segment across (segments bundle)
        do (start segment)))

(defmethod mix ((bundle bundle))
  (loop for segment across (segments bundle)
        do (mix segment)))

(defmethod end ((bundle bundle))
  (loop for segment across (segments bundle)
        do (end segment)))

(defmethod free ((bundle bundle))
  (loop for segment across (segments bundle)
        do (free segment))
  (slot-makunbound bundle 'segments))

(defmethod input-field (field (location integer) (bundle bundle))
  (input-field field 0 (aref (segments bundle) location)))

(defmethod (setf input-field) (value field (location integer) (bundle bundle))
  (setf (input-field field 0 (aref (segments bundle) location)) value))

(defmethod output-field (field (location integer) (bundle bundle))
  (output-field field 0 (aref (segments bundle) location)))

(defmethod (setf output-field) (value field (location integer) (bundle bundle))
  (setf (output-field field 0 (aref (segments bundle) location)) value))

(defmethod field (field (bundle bundle))
  (field field (aref (segments bundle) 0)))

(defmethod (setf field) (value field (bundle bundle))
  (loop for segment across (segments bundle)
        do (setf (field field segment) value)))

(defmethod input ((location integer) (bundle bundle))
  (input 0 (aref (segments bundle) location)))

(defmethod (setf input) (value (location integer) (bundle bundle))
  (setf (input 0 (aref (segments bundle) location)) value))

(defmethod output ((location integer) (bundle bundle))
  (output 0 (aref (segments bundle) location)))

(defmethod (setf output) (value (location integer) (bundle bundle))
  (setf (output 0 (aref (segments bundle) location)) value))

(macrolet ((define-delegate (accessor)
             `(progn
                (defmethod ,accessor ((bundle bundle))
                  (,accessor (aref (segments bundle) 0)))
                (defmethod (setf ,accessor) (value (bundle bundle))
                  (loop for segment across (segments bundle)
                        do (setf (,accessor segment) value))))))
  (define-delegate bypass)
  (define-delegate volume)
  (define-delegate samplerate)
  (define-delegate duration)
  (define-delegate fade-type)
  (define-delegate from)
  (define-delegate to)
  (define-delegate cutoff)
  (define-delegate frequency-pass)
  (define-delegate open-threshold)
  (define-delegate close-threshold)
  (define-delegate attack)
  (define-delegate hold)
  (define-delegate release)
  (define-delegate wave-type)
  (define-delegate frequency)
  (define-delegate noise-type)
  (define-delegate pitch)
  (define-delegate steps)
  (define-delegate repeat-mode)
  (define-delegate location)
  (define-delegate velocity)
  (define-delegate direction)
  (define-delegate up)
  (define-delegate soundspeed)
  (define-delegate doppler-factor)
  (define-delegate min-distance)
  (define-delegate max-distance)
  (define-delegate rolloff)
  (define-delegate speed-factor)
  (define-delegate pan)
  (define-delegate wet))
