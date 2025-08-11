(asdf:defsystem cl-mixed-pipewire
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "PipeWire based playback drain."
  :homepage "https://shirakumo.org/docs/cl-mixed/"
  :bug-tracker "https://shirakumo.org/project/cl-mixed/issues"
  :source-control (:git "https://shirakumo.org/project/cl-mixed.git")
  :serial T
  :components ((:file "pipewire-cffi")
               (:file "pipewire"))
  :depends-on (:cl-mixed
               (:feature (:not :pipewire-no-threads) :bordeaux-threads)
               :cffi))
