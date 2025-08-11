(asdf:defsystem cl-mixed-coreaudio
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "CoreAudio based playback drain."
  :homepage "https://shirakumo.org/docs/cl-mixed/"
  :bug-tracker "https://shirakumo.org/project/cl-mixed/issues"
  :source-control (:git "https://shirakumo.org/project/cl-mixed.git")
  :serial T
  :components ((:file "coreaudio-cffi")
               (:file "coreaudio"))
  :depends-on (:cl-mixed
               :bordeaux-threads
               :float-features
               :cffi))
