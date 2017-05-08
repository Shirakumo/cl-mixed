#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mixed.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(define-foreign-library libmixed
  (:darwin (:or "libmixed.dylib" "libmixed.so"
                #+X86 "mac32-libmixed.dylib"
                #+X86-64 "mac64-libmixed.dylib"))
  (:unix (:or "libmixed.so"
              #+X86 "lin32-libmixed.so"
              #+X86-64 "lin64-libmixed.so"))
  (:windows (:or "out123.dll"
                 #+X86 "win32-libmixed.dll"
                 #+X86-64 "win64-libmixed.dll"))
  (t (:default "mixed")))

(use-foreign-library libmixed)

(defctype size_t :uint)



(defcfun (mixed-error "mixed_error") error)

(defcfun (mixed-error-string "mixed_error_string") :string
  (error error))
