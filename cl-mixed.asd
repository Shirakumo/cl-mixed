#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem cl-mixed
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to libmixed, a sound mixing and processing library."
  :homepage "https://github.com/Shirakumo/cl-mixed"
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "toolkit")
               (:file "buffer")
               (:file "channel")
               (:file "segment")
               (:file "mixer")
               (:file "documentation"))
  :depends-on (:alexandria
               :cffi
               :cffi-libffi
               :trivial-features
               :trivial-garbage
               :documentation-utils))
