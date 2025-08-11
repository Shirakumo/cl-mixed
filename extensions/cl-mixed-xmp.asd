(asdf:defsystem cl-mixed-xmp
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Libxmp playback drain for tracker files."
  :homepage "https://shirakumo.org/docs/cl-mixed/"
  :bug-tracker "https://shirakumo.org/project/cl-mixed/issues"
  :source-control (:git "https://shirakumo.org/project/cl-mixed.git")
  :serial T
  :components ((:file "xmp-cffi")
               (:file "xmp"))
  :depends-on (:cl-mixed
               :pathname-utils))
