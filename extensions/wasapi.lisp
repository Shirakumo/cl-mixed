#|
 This file is a part of cl-mixed
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.mixed.wasapi
  (:use #:cl)
  (:local-nicknames
   (#:mixed #:org.shirakumo.fraf.mixed)
   (#:mixed-cffi #:org.shirakumo.fraf.mixed.cffi)
   (#:com #:org.shirakumo.com-on)
   (#:com-cffi #:org.shirakumo.com-on.cffi)
   (#:wasapi #:org.shirakumo.fraf.mixed.wasapi.cffi))
  (:export
   #:drain))
(in-package #:org.shirakumo.fraf.mixed.wasapi)

(defstruct (device :constructor)
  (id NIL :type string)
  (name NIL :type string))

(defmethod print-object ((device device) stream)
  (if *print-readably*
      (call-next-method)
      (write-string (device-name device) stream)))

(defun device-from-win (device)
  (cffi:with-foreign-objects ((key '(:struct wasapi:property-key))
                              (var '(:struct wasapi:propvariant)))
    (loop for i from 0 below (cffi:foreign-type-size 'com:guid)
          do (setf (cffi:mem-aref key :uint8 i) (aref (com:bytes wasapi:PKEY-Device-FriendlyName) i)))
    (setf (wasapi:property-key-pid key) 14)
    (let ((id (com:with-deref (id :pointer)
                (wasapi:imm-device-get-id device id)))
          (store (com:with-deref (store :pointer)
                   (wasapi:imm-device-open-property-store device 0 store))))
      (unwind-protect
           (progn
             (com:check-hresult (wasapi:i-property-store-get-value store key var))
             (make-device :id (com:wstring->string id)
                          :name (com:wstring->string (wasapi:propvariant-value var))))
        (wasapi:clear-propvariant var)
        (com:release store)
        (com-cffi:task-mem-free id)))))

(defun enumerate-devices ()
  (com:with-com (enumerator (com:create wasapi:CLSID-MMDEVICEENUMERATOR wasapi:IID-IMMDEVICEENUMERATOR))
    (com:with-com (collection (com:with-deref (collection :pointer)
                                (wasapi:imm-device-enumerator-enum-audio-endpoints enumerator :render wasapi:DEVICE-STATE-ACTIVE collection)))
      (loop for i from 0 below (com:with-deref (count :uint)
                                 (wasapi:imm-device-collection-get-count collection count))
            collect (com:with-com (device (com:with-deref (device :pointer)
                                            (wasapi:imm-device-collection-item collection i device)))
                      (device-from-win device))))))

(defun find-audio-client (&optional id)
  (com:with-com (enumerator (com:create wasapi:CLSID-MMDEVICEENUMERATOR wasapi:IID-IMMDEVICEENUMERATOR))
    (com:with-com (device (if id
                              (com:with-wstring (wstring id)
                                (com:with-deref (device :pointer)
                                  (wasapi:imm-device-enumerator-get-device enumerator wstring device)))
                              (com:with-deref (device :pointer)
                                (wasapi:imm-device-enumerator-get-default-audio-endpoint enumerator :render :multimedia device))))
      (values
       (com:with-deref (client :pointer)
         (wasapi:imm-device-activate device wasapi:IID-IAUDIOCLIENT com-cffi:CLSCTX-ALL (cffi:null-pointer) client))
       (device-from-win device)))))

(defun format-supported-p (audio-client samplerate channels sample-format &optional (mode :shared))
  (cffi:with-foreign-object (wave '(:struct wasapi:waveformat-extensible))
    (wasapi:encode-wave-format wave samplerate channels sample-format)
    (cffi:with-foreign-object (closest :pointer)
      (let ((pass (wasapi:i-audio-client-is-format-supported
                   audio-client mode wave closest)))
        (let ((closest (cffi:mem-ref closest :pointer)))
          (unwind-protect
               (multiple-value-bind (samplerate new-channels sample-format)
                   (cond ((and (eql :ok pass) (cffi:null-pointer-p closest))
                          (values samplerate channels sample-format))
                         ((not (cffi:null-pointer-p closest))
                          (wasapi:decode-wave-format closest)))
                 (values (eql :ok pass) samplerate new-channels sample-format))
            (com-cffi:task-mem-free closest)))))))

(defun try-formats (audio-client samplerate channels sample-format &optional (mode :shared))
  (let ((channel-formats (list* channels
                          '((:left-front :right-front)
                            (:left-front :right-front :center)
                            (:left-front :right-front :left-rear :right-rear)
                            (:left-front :right-front :center :left-rear :right-rear)
                            (:left-front :right-front :center :left-rear :right-rear :subwoofer)
                            (:left-front :right-front :center :left-rear :right-rear :front-left-of-center :front-right-of-center)
                            (:left-front :right-front :center :left-rear :right-rear :subwoofer :front-left-of-center :front-right-of-center)
                            (:left-front)
                            (:center))))
        (last ()))
    (dolist (samplerate (list samplerate 44100 48000 88200 96000))
      (dolist (sample-format (list sample-format :float :uint8 :int16 :int32 :int64 :int24))
        (dolist (channels channel-formats)
          (multiple-value-bind (ok new-samplerate new-channels new-sample-format)
              (format-supported-p audio-client samplerate channels sample-format mode)
            (when ok (return-from try-formats
                       (values :ok new-samplerate new-channels new-sample-format)))
            (setf last (list new-samplerate new-channels new-sample-format mode))))))
    (when last
      (apply #'format-supported-p audio-client last))))

(defun mix-format (audio-client)
  (com:with-deref (format :pointer)
    (wasapi:i-audio-client-get-mix-format audio-client format)))

(defun reference-time->seconds (reference-time)
  (* reference-time 100 (expt 10 -9)))

(defun seconds->reference-time (seconds)
  (ceiling (* seconds 1/100 (expt 10 9))))

(defun device-period (audio-client)
  (cffi:with-foreign-objects ((default 'wasapi:reference-time)
                              (minimum 'wasapi:reference-time))
    (com:check-hresult
      (wasapi:i-audio-client-get-device-period audio-client default minimum))
    (values (reference-time->seconds (cffi:mem-ref default 'wasapi:reference-time))
            (reference-time->seconds (cffi:mem-ref minimum 'wasapi:reference-time)))))

(defun audio-client-label (audio-client)
  (com:with-com (session (com:with-deref (session :pointer)
                           (wasapi:i-audio-client-get-service audio-client wasapi:IID-IAUDIOSESSIONCONTROL session)))
    (cffi:with-foreign-object (label :pointer)
      (wasapi:i-audio-session-control-get-display-name session label)
      (com:wstring->string (cffi:mem-ref label :pointer)))))

(defun (setf audio-client-label) (label audio-client)
  (ignore-errors
   (com:with-com (session (com:with-deref (session :pointer)
                            (wasapi:i-audio-client-get-service audio-client wasapi:IID-IAUDIOSESSIONCONTROL session)))
     (com:with-wstring (label label)
       (wasapi:i-audio-session-control-set-display-name session label (cffi:null-pointer)))))
  label)

(defclass drain (mixed:device-drain)
  ((mode :initform :shared :initarg :mode :accessor mode)
   (client :initform NIL :accessor client)
   (render :initform NIL :accessor render)
   (event :initform NIL :accessor event)
   (device :initform NIL :reader mixed:device :accessor device)
   (channel-order :initform () :initarg :channel-order :accessor mixed:channel-order)))

(defmethod initialize-instance :after ((drain drain) &key device)
  (com:init)
  (init drain device))

(defun init (drain device)
  ;; FIXME: allow picking shared/exclusive mode
  (setf (mixed:channels (mixed:pack drain)) (max 1 (mixed:channels (mixed:pack drain))))
  (let* ((mode (mode drain))
         (pack (mixed:pack drain))
         ;; Attempt to get a buffer as large as our internal ones.
         (buffer-duration (seconds->reference-time (/ (mixed:size pack) (mixed:framesize pack) (mixed:samplerate pack))))
         (periodicity (ecase mode (:shared 0) (:exclusive buffer-duration)))
         (channels (or (mixed:channel-order drain) (mixed:guess-channel-order-from-count (mixed:channels pack)))))
    (multiple-value-bind (client device) (find-audio-client (etypecase device
                                                              ((member NIL :default) NIL)
                                                              (device (device-id device))
                                                              (string device)))
      (setf (client drain) client)
      (setf (device drain) device)
      (multiple-value-bind (ok samplerate channels encoding) (try-formats client (mixed:samplerate pack) channels :float)
        (declare (ignore ok))
        (setf (mixed:channels pack) (length channels))
        (setf (mixed:samplerate pack) samplerate)
        (setf (mixed:encoding pack) encoding)
        (setf (mixed:channel-order drain) channels))
      ;; Initialise the rest
      (cffi:with-foreign-object (format '(:struct wasapi:waveformat-extensible))
        (wasapi:encode-wave-format format (mixed:samplerate pack) (mixed:channel-order drain) (mixed:encoding pack))
        (com:check-hresult
         (wasapi:i-audio-client-initialize client mode wasapi:AUDCLNT-STREAMFLAGS-EVENTCALLBACK
                                           buffer-duration periodicity
                                           format (cffi:null-pointer))))
      (when (mixed:program-name drain)
        (setf (audio-client-label client) (mixed:program-name drain)))
      (let ((event (wasapi:create-event (cffi:null-pointer) 0 0 (cffi:null-pointer))))
        (com:check-last-error (not (cffi:null-pointer-p event)))
        (setf (event drain) event)
        (com:check-hresult
         (wasapi:i-audio-client-set-event-handle client event)))
      (setf (render drain)
            (com:with-deref (render :pointer)
              (wasapi:i-audio-client-get-service client wasapi:IID-IAUDIORENDERCLIENT render))))))

(defmethod mixed:list-devices ((drain drain))
  (enumerate-devices))

(defmethod (setf mixed:device) (device (drain drain))
  (setf (mixed:channel-order drain) ())
  (cond ((client drain)
         (mixed:end drain)
         (clear drain)
         (init drain device)
         (mixed:start drain))
        (T
         (init drain device)))
  device)

(defmethod mixed:free ((drain drain))
  (clear drain))

(defun clear (drain)
  (when (event drain)
    (wasapi:close-handle (event drain))
    (setf (event drain) NIL))
  (when (render drain)
    (wasapi:i-audio-render-client-release (render drain))
    (setf (render drain) NIL))
  (when (client drain)
    (wasapi:i-audio-client-release (client drain))
    (setf (client drain) NIL)))

(defmethod mixed:start ((drain drain))
  ;; Send one frame of nothing to avoid crackles on startup.
  (com:with-deref (target :pointer)
    (wasapi:i-audio-render-client-get-buffer (render drain) 1 target))
  (wasapi:i-audio-render-client-release-buffer (render drain) 1 2)
  (com:check-hresult
   (wasapi:i-audio-client-start (client drain))
   :ok :not-stopped))

(defmethod mixed:mix ((drain drain))
  (mixed:with-buffer-tx (data start size (mixed:pack drain))
    (when (< 0 size)
      (macrolet ((with-render-deref ((var type) func)
                   `(cffi:with-foreign-object (,var ,type)
                      (case ,func
                        ((:ok :false)
                         (cffi:mem-ref ,var ,type))
                        (:device-invalidated
                         ;; Sleep as many frames as we have to simulate playback
                         (sleep (/ size
                                   (mixed:framesize (mixed:pack drain))
                                   (mixed:samplerate (mixed:pack drain))))
                         (mixed:finish size)
                         ;; Then try to reacquire the sound device
                         (handler-case
                             (progn (init drain (device drain))
                                    (mixed:start drain))
                           (error ()))
                         ;; Just exit out. We'll get back in to try again or resume playback
                         ;; next time we get called.
                         (return-from mixed:mix 0))))))
        (let* ((render (render drain))
               (client (client drain))
               (event (event drain))
               (framesize (mixed:framesize (mixed:pack drain)))
               (buffer-size (* framesize
                               (- (with-render-deref (frames :uint32)
                                    (wasapi:i-audio-client-get-buffer-size client frames))
                                  (with-render-deref (frames :uint32)
                                    (wasapi:i-audio-client-get-current-padding client frames))))))
          (loop while (= 0 buffer-size) ;; Wait until we have stuff available again
                do (wasapi:wait-for-single-object event 1000)
                   (setf buffer-size (* framesize
                                        (- (with-render-deref (frames :uint32)
                                             (wasapi:i-audio-client-get-buffer-size client frames))
                                           (with-render-deref (frames :uint32)
                                             (wasapi:i-audio-client-get-current-padding client frames))))))
          ;; We shouldn't have to do this, but if we don't it seems to spinlock above. Cool.
          (wasapi:reset-event event)
          (let* ((size (min buffer-size size))
                 (frames (/ size framesize))
                 (buffer (with-render-deref (target :pointer)
                           (wasapi:i-audio-render-client-get-buffer render frames target))))
            (static-vectors:replace-foreign-memory buffer (mixed:data-ptr) size)
            (wasapi:i-audio-render-client-release-buffer render frames 0)
            (mixed:finish size)))))))

(defmethod mixed:end ((drain drain))
  (wasapi:i-audio-client-reset (client drain))
  (wasapi:i-audio-client-stop (client drain)))
