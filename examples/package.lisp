#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.examples
  (:use #:cl)
  (:shadow #:space)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:out123 #:org.shirakumo.fraf.out123)
   (#:mpg123 #:org.shirakumo.fraf.mpg123))
  (:export
   #:play
   #:echo
   #:space
   #:tone))

(in-package #:org.shirakumo.fraf.mixed.examples)
