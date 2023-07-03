(asdf:defsystem cl-mixed
  :version "2.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
               (:file "generic")
               (:file "drain")
               (:file "source")
               (:file "mixer")
               (:module "segments"
                :components ((:file "basic-mixer")
                             (:file "biquad-filter")
                             (:file "bundle")
                             (:file "chain")
                             (:file "channel-convert")
                             (:file "delay")
                             (:file "distribute")
                             (:file "fader")
                             (:file "gate")
                             (:file "generator")
                             (:file "ladspa")
                             (:file "noise")
                             (:file "null")
                             (:file "packer")
                             (:file "pitch")
                             (:file "plane-mixer")
                             (:file "quantize")
                             (:file "queue")
                             (:file "repeat")
                             (:file "space-mixer")
                             (:file "spatial-reverb")
                             (:file "speed-change")
                             (:file "unpacker")
                             (:file "virtual")
                             (:file "volume-control")))
               (:module "extensions"
                :components ((:file "dummy")))
               (:file "documentation"))
  :depends-on (:alexandria
               :static-vectors
               :cffi
               :trivial-features
               :documentation-utils))
