(defpackage #:org.shirakumo.fraf.mixed.examples
  (:use #:cl)
  (:shadow #:space #:stream)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:out123 #:org.shirakumo.fraf.out123)
   (#:mpg123 #:org.shirakumo.fraf.mpg123))
  (:export
   #:mixer
   #:play
   #:play-to-file
   #:convolution
   #:echo
   #:space
   #:tone
   #:record-to-file))

(in-package #:org.shirakumo.fraf.mixed.examples)
