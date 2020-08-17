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
   #:channel-mask
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

(cffi:defbitfield channel-mask
  (:front-left             #x1)
  (:front-right            #x2)
  (:front-center           #x4)
  (:low-frequency          #x8)
  (:back-left              #x10)
  (:back-right             #x20)
  (:front-left-of-center   #x40)
  (:front-right-of-center  #x80)
  (:back-center            #x100)
  (:side-left              #x200)
  (:side-right             #x400)
  (:top-center             #x800)
  (:top-front-left         #x1000)
  (:top-front-center       #x2000)
  (:top-front-right        #x4000)
  (:top-back-left          #x8000)
  (:top-back-center        #x10000)
  (:top-back-right         #x20000)
  (:reserved               #x80000000))

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
  (:no-driver-cb     20))

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
  (flags wave-header-flags)
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

(defun channel-mask-for-channel-count (channels)
  (case channels
    (1 (cffi:foreign-bitfield-value 'channel-mask '(:front-center)))
    (2 (cffi:foreign-bitfield-value 'channel-mask '(:front-left :front-right)))
    (3 (cffi:foreign-bitfield-value 'channel-mask '(:front-left :front-center :front-right)))
    (4 (cffi:foreign-bitfield-value 'channel-mask '(:front-left :front-right :back-left :back-right)))
    (5 (cffi:foreign-bitfield-value 'channel-mask '(:front-left :front-right :back-left :back-right :low-frequency)))
    (6 (cffi:foreign-bitfield-value 'channel-mask '(:front-left :front-center :front-right :back-left :back-right :low-frequency)))
    (T (cffi:foreign-bitfield-value 'channel-mask '()))))

(defun encode-wave-format (ptr samplerate channels format)
  (let ((bit-depth (ecase format
                     ((:int64) 64)
                     ((:int32) 32)
                     ((:int24) 32)
                     ((:int16) 16)
                     ((:uint8) 8))))
    (setf (waveformat-ex-format-tag ptr) WAVE-FORMAT-PCM)
    (setf (waveformat-ex-size ptr) 22)
    (setf (waveformat-ex-channels ptr) channels)
    (setf (waveformat-ex-samples-per-sec ptr) samplerate)
    (setf (waveformat-ex-bits-per-sample ptr) bit-depth)
    (setf (waveformat-ex-block-align ptr) (/ (* channels (waveformat-ex-bits-per-sample ptr)) 8))
    (setf (waveformat-ex-avg-bytes-per-sec ptr) (* samplerate (waveformat-ex-block-align ptr))))
  ptr)

(defun decode-wave-format (ptr)
  (values (waveformat-ex-samples-per-sec ptr)
          (waveformat-ex-channels ptr) 
          (waveformat-ex-bits-per-sample ptr)))
