#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-mixed-examples
  :version "2.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A set of simple examples for usage of cl-mixed"
  :homepage "https://Shirakumo.github.io/cl-mixed/"
  :bug-tracker "https://github.com/Shirakumo/cl-mixed/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-mixed.git")
  :serial T
  :components ((:file "package")
               (:file "tone")
               (:file "space")
               (:file "echo")
               (:file "play")
               (:file "play-to-file")
               (:file "mixer")
               (:file "record-to-file"))
  :depends-on (:cl-mixed
               :cl-mixed-out123
               :cl-mixed-mpg123
               :cl-mixed-wav
               :cl-mixed-pulse))
