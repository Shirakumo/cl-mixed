#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-mixed-cffi
  (:nicknames #:org.shirakumo.fraf.mixed.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:export
   #:*static*
   #:libmixed
   #:size_t
   #:mixed-error
   #:mixed-error-string))

(defpackage #:cl-mixed
  (:nicknames #:org.shirakumo.fraf.mixed)
  (:use #:cl #:cffi)
  ;; wrapper.lisp
  (:export
   ))
