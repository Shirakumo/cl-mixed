(asdf:defsystem cl-mixed-sdl2
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "SDL2 based playback drain."
  :homepage "https://shirakumo.org/docs/cl-mixed/"
  :bug-tracker "https://shirakumo.org/project/cl-mixed/issues"
  :source-control (:git "https://shirakumo.org/project/cl-mixed.git")
  :serial T
  :components ((:file "sdl2"))
  :depends-on (:cl-mixed
               :sdl2))
