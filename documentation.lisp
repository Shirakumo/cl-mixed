#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.cffi)

;; low-level.lisp
(docs:define-docs
  (variable *here*
    "Variable containing the path to the low-level.lisp file.")
  
  (variable *static*
    "Variable containing the path to the static directory.
That directory contains the precompiled library binaries.")
  
  )

(in-package #:org.shirakumo.fraf.mixed)

;; wrapper.lisp
(docs:define-docs
  )
