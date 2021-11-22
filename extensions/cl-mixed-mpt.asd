#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-mixed-mpt
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Libopenmpt playback drain for tracker files."
  :homepage "https://Shirakumo.github.io/cl-mixed/"
  :bug-tracker "https://github.com/Shirakumo/cl-mixed/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-mixed.git")
  :serial T
  :components ((:file "mpt-cffi")
               (:file "mpt"))
  :depends-on (:cl-mixed
               :static-vectors))
