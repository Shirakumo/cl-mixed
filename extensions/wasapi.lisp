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
   #:wasapi-drain))
(in-package #:org.shirakumo.fraf.mixed.wasapi)

(defun enumerate-devices ()
  (com:with-com (enumerator (com:create wasapi:CLSID-MMDEVICEENUMERATOR wasapi:IID-IMMDEVICEENUMERATOR))
    (com:with-com (collection (com:with-deref (collection :pointer)
                                       (wasapi:imm-device-enumerator-enum-audio-endpoints enumerator :render wasapi:DEVICE-STATE-ACTIVE collection)))
      (loop for i from 0 below (com:with-deref (count :uint)
                                 (wasapi:imm-device-collection-get-count collection count))
            collect (com:with-com (device (com:with-deref (device :pointer)
                                                   (wasapi:imm-device-collection-item collection i device)))
                      (let ((id (com:with-deref (id :pointer)
                                  (wasapi:imm-device-get-id device id))))
                        (unwind-protect (com:wstring->string id)
                          (com-cffi:task-mem-free id))))))))

(defun find-audio-client (&optional id)
  (com:with-com (enumerator (com:create wasapi:CLSID-MMDEVICEENUMERATOR wasapi:IID-IMMDEVICEENUMERATOR))
    (com:with-com (device (com:with-deref (device :pointer)
                            (if id
                                (let ((wstring (com:string->wstring id)))
                                  (unwind-protect (wasapi:imm-device-enumerator-get-device enumerator wstring device)
                                    (cffi:foreign-free wstring)))
                                (wasapi:imm-device-enumerator-get-default-audio-endpoint enumerator :render :multimedia device))))
      (com:with-deref (client :pointer)
        (wasapi:imm-device-activate device wasapi:IID-IAUDIOCLIENT com-cffi:CLSCTX-ALL (cffi:null-pointer) client)))))

(defun format-supported-p (audio-client samplerate channels sample-format &optional (mode :shared))
  (cffi:with-foreign-object (wave '(:struct wasapi:waveformat-extensible))
    (wasapi:encode-wave-format wave samplerate channels sample-format)
    (cffi:with-foreign-object (closest :pointer)
      (let ((pass (wasapi:i-audio-client-is-format-supported
                   audio-client mode wave closest)))
        (let ((closest (cffi:mem-ref closest :pointer)))
          (unwind-protect
               (multiple-value-bind (samplerate channels sample-format)
                   (cond ((and (eql :ok pass) (cffi:null-pointer-p closest))
                          (values samplerate channels sample-format))
                         ((not (cffi:null-pointer-p closest))
                          (wasapi:decode-wave-format closest)))
                 (values (eql :ok pass) samplerate channels sample-format))
            (com-cffi:task-mem-free closest)))))))

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
  (com:with-com (session (com:with-deref (session :pointer)
                           (wasapi:i-audio-client-get-service audio-client wasapi:IID-IAUDIOSESSIONCONTROL session)))
    (let ((label (com:string->wstring label)))
      (wasapi:i-audio-session-control-set-display-name session label (cffi:null-pointer))
      (cffi:foreign-free label))
    label))

(defclass wasapi-drain (mixed:drain)
  ((mode :initform :shared :accessor mode)
   (client :initform NIL :accessor client)
   (render :initform NIL :accessor render)
   (event :initform NIL :accessor event)
   (audio-client-id :initform NIL :initarg :audio-client-id :accessor audio-client-id)))

(defmethod initialize-instance :after ((drain wasapi-drain) &key)
  (com:init)
  (setf (mixed-cffi:direct-segment-mix (mixed:handle drain)) (cffi:callback mix))
  (cffi:use-foreign-library wasapi:avrt))

(defmethod mixed:start ((drain wasapi-drain))
  ;; FIXME: allow picking a device
  ;; FIXME: allow picking shared/exclusive mode
  (let* ((mode (mode drain))
         (pack (mixed:pack drain))
         ;; Attempt to get a buffer as large as our internal ones.
         (buffer-duration (seconds->reference-time (/ (mixed:size pack)
                                                      (mixed:framesize pack)
                                                      (mixed:target-samplerate drain))))
         (client (find-audio-client (audio-client-id drain)))
         (format (mix-format client)))
    (unwind-protect
         (multiple-value-bind (ok samplerate channels sample-format) (format-supported-p client (mixed:target-samplerate drain) 2 :float)
           (declare (ignore ok))
           (setf (client drain) client)
           (setf (mixed:channels pack) channels)
           (setf (mixed:samplerate pack) samplerate)
           (setf (mixed:encoding pack) sample-format)))
    ;; Initialise the rest
    (unless (render drain)
      (com:check-hresult
       (wasapi:i-audio-client-initialize client mode wasapi:AUDCLNT-STREAMFLAGS-EVENTCALLBACK
                                         buffer-duration (ecase mode (:shared 0) (:exclusive buffer-duration))
                                         format (cffi:null-pointer)))
      (setf (event drain) (com:check-last-error
                              (not (cffi:null-pointer-p (wasapi:create-event (cffi:null-pointer) 0 0 (cffi:null-pointer))))))
      (com:check-hresult
        (wasapi:i-audio-client-set-event-handle client (event drain)))
      (when (mixed:program-name drain)
        (setf (audio-client-label client) (mixed:program-name drain)))
      (com-cffi:task-mem-free format)
      (setf (render drain)
            (com:with-deref (render :pointer)
              (wasapi:i-audio-client-get-service client wasapi:IID-IAUDIORENDERCLIENT render)))
      (com:check-hresult
        (wasapi:i-audio-client-start client)))))

(cffi:defcallback mix :int ((segment :pointer))
  (let* ((drain (mixed:pointer->object segment))
         (render (render drain))
         (client (client drain))
         (buffer-size (- (com:with-deref (frames :uint32)
                           (wasapi:i-audio-client-get-buffer-size client frames))
                         (com:with-deref (frames :uint32)
                           (wasapi:i-audio-client-get-current-padding client frames)))))
    (mixed:with-buffer-tx (data start end (mixed:pack drain))
      (let* ((to-write (min buffer-size (- start end)))
             (buffer (com:with-deref (target :pointer)
                       (wasapi:i-audio-render-client-get-buffer render to-write target))))
        (static-vectors:replace-foreign-memory buffer (mixed:data-ptr) to-write)
        (wasapi:i-audio-render-client-release-buffer render to-write 0)))))

(defmethod mixed:end ((drain wasapi-drain))
  (when (event drain)
    (wasapi:close-handle (event drain))
    (setf (event drain) NIL))
  (when (render drain)
    (wasapi:i-audio-render-client-release (render drain))
    (setf (render drain) NIL))
  (when (client drain)
    (wasapi:i-audio-client-stop (client drain))
    (wasapi:i-audio-client-release (client drain))
    (setf (client drain) NIL)))
