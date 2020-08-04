#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-mixed
  :version "2.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to libmixed, a sound mixing and processing library."
  :homepage "https://Shirakumo.github.io/cl-mixed/"
  :bug-tracker "https://github.com/Shirakumo/cl-mixed/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-mixed.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "toolkit")
               (:file "c-object")
               (:file "bip-buffer")
               (:file "buffer")
               (:file "pack")
               (:file "segment")
               (:file "segment-sequence")
               (:file "generic")
               (:module "segments"
                :components ((:file "basic-mixer")
                             (:file "delay")
                             (:file "fader")
                             (:file "frequency-pass")
                             (:file "generator")
                             (:file "ladspa")
                             (:file "noise")
                             (:file "packer")
                             (:file "pitch")
                             (:file "queue")
                             (:file "repeat")
                             (:file "space-mixer")
                             (:file "unpacker")
                             (:file "virtual")
                             (:file "volume-control")))
               (:file "documentation"))
  :depends-on (:alexandria
               :static-vectors
               :cffi
               :trivial-features
               :trivial-garbage
               :documentation-utils))
