#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.fraf.mixed.winmm.cffi
  (:use #:cl #:cffi)
  (:export
   #:winmm
   #:WAVE-MAPPER
   #:INFINITE
   #:mmresult
   #:wait-result
   #:open-flags
   #:wave-header
   #:wave-header-data
   #:wave-header-buffer-length
   #:wave-header-bytes-recorded
   #:wave-header-user
   #:wave-header-flags
   #:wave-header-loops
   #:wave-header-next
   #:wave-header-reserved
   #:waveformat-ex
   #:waveformat-ex-format-tag
   #:waveformat-ex-channels
   #:waveformat-ex-samples-per-sec
   #:waveformat-ex-avg-bytes-per-sec
   #:waveformat-ex-block-align
   #:waveformat-ex-bits-per-sample
   #:waveformat-ex-size
   #:wave-out-open
   #:wave-out-close
   #:wave-out-reset
   #:wave-out-write
   #:wave-out-prepare
   #:wave-out-unprepare
   #:create-event
   #:close-handle
   #:wait-for-single-object
   #:encode-wave-format
   #:decode-wave-format))
(in-package #:org.shirakumo.fraf.mixed.winmm.cffi)

(define-foreign-library winmm
  (T (:default "Winmm")))

(defconstant WAVE-FORMAT-PCM #x1)
(defconstant WAVE-MAPPER #xFFFFFFFF)
(defconstant INFINITE (1- (expt 2 32)))

(cffi:defctype word :uint16)
(cffi:defctype dword :uint32)

(cffi:defcenum mmresult
  (:ok               0)
  (:error            1)
  (:bad-device-id    2)
  (:not-enabled      3)
  (:allocated        4)
  (:invalid-handle   5)
  (:no-driver        6)
  (:no-memory        7)
  (:not-supported    8)
  (:bad-error-number 9)
  (:invalid-flag     10)
  (:invalid-param    11)
  (:handle-busy      12)
  (:invalid-alias    13)
  (:bad-db           14)
  (:key-not-found    15)
  (:read-error       16)
  (:write-error      17)
  (:delete-error     18)
  (:value-not-found  19)
  (:no-driver-cb     20)
  (:bad-format       32)
  (:still-playing    33)
  (:unprepared       34))

(cffi:defcenum (wait-result dword)
  (:abandoned #x00000080)
  (:object-0  #x00000000)
  (:timeout   #x00000102)
  (:failed    #xFFFFFFFF))

(cffi:defbitfield (open-flags dword)
  (:format-query      #x000000001)
  (:allow-sync        #x000000002)
  (:mapped            #x000000004)
  (:format-direct     #x000000008)
  (:default-device    #x000000010)
  (:callback-null     #x000000000)
  (:callback-event    #x000050000)
  (:callback-window   #x000100000)
  (:callback-task     #x000200000)
  (:callback-function #x000300000))

(cffi:defbitfield (wave-header-flags dword)
  (:done       #x00000001)
  (:prepared   #x00000002)
  (:begin-loop #x00000004)
  (:end-loop   #x00000008)
  (:in-queue   #x00000010))

(cffi:defcstruct (wave-header :conc-name wave-header-)
  (data :pointer)
  (buffer-length dword)
  (bytes-recorded dword)
  (user :pointer)
  (flags :int)
  (loops dword)
  (next :pointer)
  (reserved :pointer))

(cffi:defcstruct (waveformat-ex :conc-name waveformat-ex-)
  (format-tag word)
  (channels word)
  (samples-per-sec dword)
  (avg-bytes-per-sec dword)
  (block-align word)
  (bits-per-sample word)
  (size word))

(cffi:defcfun (wave-out-open "waveOutOpen") mmresult
  (device :pointer)
  (id :uint)
  (waveformat :pointer)
  (callback :pointer)
  (instance :pointer)
  (open open-flags))

(cffi:defcfun (wave-out-close "waveOutClose") mmresult
  (device :pointer))

(cffi:defcfun (wave-out-reset "waveOutReset") mmresult
  (device :pointer))

(cffi:defcfun (wave-out-write "waveOutWrite") mmresult
  (device :pointer)
  (wave-header :pointer)
  (size :uint))

(cffi:defcfun (wave-out-prepare "waveOutPrepareHeader") mmresult
  (device :pointer)
  (wave-header :pointer)
  (size :uint))

(cffi:defcfun (wave-out-unprepare "waveOutUnprepareHeader") mmresult
  (device :pointer)
  (wave-header :pointer)
  (size :uint))

(cffi:defcfun (create-event "CreateEventW") :pointer
  (event-attribute :pointer)
  (manual-reset :bool)
  (initial-state :bool)
  (name :string))

(cffi:defcfun (close-handle "CloseHandle") :bool
  (object :pointer))

(cffi:defcfun (wait-for-single-object "WaitForSingleObject") wait-result
  (handle :pointer)
  (milliseconds dword))

(defun encode-wave-format (ptr samplerate channels format)
  ;; Bittage of more than 16 is not officially supported, and apparently
  ;; does not work on all targets. TOO BAD!
  (let ((bit-depth (case format
                     (:uint8 8)
                     (T 16))))
    (setf (waveformat-ex-format-tag ptr) WAVE-FORMAT-PCM)
    (setf (waveformat-ex-size ptr) 0)
    (setf (waveformat-ex-channels ptr) channels)
    (setf (waveformat-ex-samples-per-sec ptr) samplerate)
    (setf (waveformat-ex-bits-per-sample ptr) bit-depth)
    (setf (waveformat-ex-block-align ptr) (/ (* channels (waveformat-ex-bits-per-sample ptr)) 8))
    (setf (waveformat-ex-avg-bytes-per-sec ptr) (* samplerate (waveformat-ex-block-align ptr))))
  ptr)

(defun decode-wave-format (ptr)
  (values (waveformat-ex-samples-per-sec ptr)
          (waveformat-ex-channels ptr) 
          (ecase (waveformat-ex-bits-per-sample ptr)
            (64 :int64)
            (32 :int32)
            (24 :int24)
            (16 :int16)
            (8 :uint8))))
