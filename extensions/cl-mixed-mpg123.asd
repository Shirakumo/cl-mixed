#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-mixed-mpg123
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Mpg123 based playback drain."
  :homepage "https://Shirakumo.github.io/cl-mixed/"
  :bug-tracker "https://github.com/Shirakumo/cl-mixed/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-mixed.git")
  :serial T
  :components ((:file "mpg123"))
  :depends-on (:cl-mixed
               :cl-mpg123))