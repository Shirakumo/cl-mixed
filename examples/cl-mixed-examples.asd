(asdf:defsystem cl-mixed-examples
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A set of simple examples for usage of cl-mixed"
  :homepage "https://shirakumo.org/docs/cl-mixed/"
  :bug-tracker "https://shirakumo.org/project/cl-mixed/issues"
  :source-control (:git "https://shirakumo.org/project/cl-mixed.git")
  :serial T
  :components ((:file "package")
               (:file "tone")
               (:file "space")
               (:file "echo")
               (:file "play")
               (:file "play-to-file")
               (:file "convolution")
               (:file "mixer")
               (:file "record-to-file"))
  :depends-on (:cl-mixed
               :cl-mixed-out123
               :cl-mixed-mpg123
               :cl-mixed-wav
               :cl-mixed-pulse))
