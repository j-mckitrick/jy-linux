(in-package :jy-devices)

(defclass <jy-device> ()
  ((name :accessor device-name-of :initarg :name :initform "a device")
   (unique-id :accessor unique-id-of :initarg :unique-id :initform ""))
  (:documentation
   "JY device base class."))

(defgeneric action-set (device props)
  (:documentation
   "Set a property on a device."))

(defgeneric action-get (device props)
  (:documentation
   "Get a property on a device."))

(defgeneric validate-set (device props)
  (:documentation
   "Validate a property setting on a device."))

(defgeneric validate-get (device &optional props)
  (:documentation
   "Get a validated property on a device."))

(defgeneric initialize (device))

(defmethod initialize ((device <jy-device>))
  device)

;; --------------------------------------------------------------------
(defclass <jy-detector> (<jy-device>)
  ((integration-time :accessor exp-time-of :initarg :exp-time :initform 1))
  (:documentation
   "JY detector class."))

(defgeneric get-data (detector)
  (:documentation
   "Get data from detector."))

(defmethod create-signal-details ((detector <jy-detector>))
  (let ((signal-details (validate-get detector)))
    (jy-log "Signal details: ~A" signal-details)
    (append (list :id (unique-id-of detector)) signal-details)))

(defmethod get-data ((detector <jy-detector>))
  (jy-log "Get-date w/integration time = ~A" (exp-time-of detector))
  (sleep (/ (exp-time-of detector) 1000))
  (random 10000))

(defcp fire-event-data (*data-ready* *data-lock*))
(defcp fire-handshake-event (*handshake-event* *handshake-lock*))
(defsink wait-handshake (*handshake-event* *handshake-lock* :one-time t))

(defvar *data* nil)

(defmethod experiment-get-data ((detector <jy-detector>))
  (setf *data* (list (unique-id-of detector) (get-data detector)))
  (jy-log "Firing data event.")
  (fire-event-data)
  (jy-log "Waiting for handshake event.")
  ;(wait-handshake)
  (jy-log "Received handshake event."))

(defmethod action-get ((detector <jy-detector>) props)
  (experiment-get-data detector))

(defmethod set-integration-time ((detector <jy-detector>) &key int-time)
  (jy-log "Set ~A integration time: ~A." (unique-id-of detector) int-time)
  (setf (exp-time-of detector) int-time)
  int-time)

(defmethod action-set ((detector <jy-detector>) props)
  (setf (exp-time-of detector) (getf props :exp-value)))

(defmethod validate-set ((detector <jy-detector>) props)
  (setf (exp-time-of detector) (getf props :exp-value)))

(defmethod validate-get ((detector <jy-detector>) &optional props)
  (declare (ignorable props))
  (let ((signal-info ()))
    (setf (getf signal-info :data-format) :double)
    (setf (getf signal-info :z-size) 1)
    (setf (getf signal-info :y-size) 1)
    (setf (getf signal-info :x-size) 1)
    signal-info))

;; --------------------------------------------------------------------
(defclass <jy-mono> (<jy-device>)
  ((wl :accessor wl-of :initform 0.0))
  (:documentation
   "JY mono class."))

(defgeneric move (mono &key to)
  (:documentation
   "Move mono."))

(defmethod move ((mono <jy-mono>) &key to)
  (jy-log "Move mono ~A to ~A." (unique-id-of mono) to)
  (setf (wl-of mono) to)
  to)

(defmethod action-set ((mono <jy-mono>) props)
  (move mono :to (getf props :exp-value)))

(defmethod action-get ((mono <jy-mono>) props)
  mono)

(defmethod validate-set ((mono <jy-mono>) props)
  (move mono :to (getf props :exp-value)))

(defmethod validate-get ((mono <jy-mono>) &optional props)
  (declare (ignorable props))
  mono)
