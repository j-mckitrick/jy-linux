(in-package :jy-system)

(defclass <jy-axis> ()
  ((number :accessor number-of :initarg number :initform 1)
   (axis-type :accessor axis-type-of :initarg axis-type :initform :wavelength)
   (begin :accessor begin-of :initarg begin :initform 1)
   (end :accessor end-of :initarg end :initform 10)
   (increment :accessor increment-of :initarg increment :initform 1)
   (done :accessor done-p :initform nil)
   (current-position :accessor current-position-of :initform 0)
   (pre-ops :accessor pre-ops-of :initform ())
   (data-ops :accessor data-ops-of :initform ()))
  (:documentation
   "JY experiment axis."))

(defmethod add-bound-op ((axis <jy-axis>) (op <jy-operation>))
  (setf (pre-ops-of axis) (append (pre-ops-of axis) (list op))))

(defmethod add-data-op ((axis <jy-axis>) (op <jy-operation>))
  (setf (data-ops-of axis) (append (data-ops-of axis) (list op))))

(defmacro defaxismethod (name (axis &rest args) &body body)
  `(defjymethod ,name (,axis <jy-axis> ,@args) ,@body))

(defaxismethod reset-axis (axis)
  (setf (current-position-of axis) (begin-of axis)
	(done-p axis) nil))

(defaxismethod kick-axis (axis)
  (jy-log "Kicking axis ~A" (number-of axis))
  (incf (current-position-of axis))
  (dolist (pre-op (pre-ops-of axis))
    (jy-log "pre-op params: ~A" (op-parameters-of pre-op))
    (setf (getf (op-parameters-of pre-op) :exp-value) (current-position-of axis))
    (do-it pre-op))
  (format t "Data ops: ~A" (data-ops-of axis))
  (dolist (data-op (data-ops-of axis))
    (jy-log "Data op: ~A" data-op)
    (do-it data-op))
  (when (> (current-position-of axis) (end-of axis))
    (setf (done-p axis) t)))

(defaxismethod do-axis (axis)
  (jy-log "Execute axis ~A." (number-of axis))
  (reset-axis axis)
  (loop
     do
       (kick-axis axis)
;       (sleep 1)
     until (done-p axis))
  (jy-log "Done axis ~A." (number-of axis)))

