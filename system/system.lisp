(in-package :jy-system)

(defparameter *config-file* "conf/config.xml")

(defclass <jy-engine> ()
  ((name :accessor name-of :initform "A JY engine"))
  (:documentation
   "JY engine base class."))

(defclass <jy-configure-engine> (<jy-engine>)
  ()
  (:documentation
   "JY configure engine class."))

(defclass <jy-experiment-engine> (<jy-engine>)
  ((parent-system :accessor parent-system-of :initform nil)
   (general-properties
    :accessor general-properties-of :initform (make-hash-table :test 'equal))
   (specific-properties
    :accessor specific-properties-of :initform (make-hash-table :test 'equal))
   (start-ops :accessor start-ops-of :initform ())
   (axes :accessor axes-of :initform (make-hash-table))
   (end-ops :accessor end-ops-of :initform ())
   (result :accessor result-of :initform (make-hash-table :test 'equal))
   (signals :accessor signals-of :initform nil)
   (points :accessor points-in :initform 0))
  (:documentation
   "JY experiment engine class."))

(defclass <jy-preview-engine> (<jy-engine>)
  ()
  (:documentation
   "JY preview engine class."))

(defclass <jy-init-engine> (<jy-engine>)
  ()
  (:documentation
   "JY init engine class."))

(defclass <jy-system> ()
  ((name :accessor name-of :initform "A system")
   (detectors :accessor detectors-of :initform ())
   (monos :accessor monos-of :initform ())
   (init-engine
    :accessor init-engine-of :initform (make-instance '<jy-init-engine>))
   (experiment-engine
    :accessor experiment-engine-of :initform (make-instance '<jy-experiment-engine>))
   (configure-engine
    :accessor configure-engine-of :initform (make-instance '<jy-configure-engine>))
   (preview-engine
    :accessor preview-engine-of :initform (make-instance '<jy-preview-engine>)))
  (:documentation
   "JY system class."))

(defjymethod init (system <jy-system>)
  (with-open-file (s "log/logfile" :direction :output :if-exists :supersede)
    (format s "Initialized.~%")))

;(defgeneric do-experiment (obj))
(defmethod do-experiment ((system <jy-system>))
  (execute (experiment-engine-of system)))

;(defgeneric configure (system))
(defmethod do-configure ((system <jy-system>))
  (setf (parent-system-of (experiment-engine-of system)) system)
  (push (make-instance '<jy-detector> :unique-id "scd1") (detectors-of system))
  (push (make-instance '<jy-mono> :unique-id "mono1") (monos-of system))
  (push (make-instance '<jy-detector> :unique-id "scd2") (detectors-of system))
  (push (make-instance '<jy-mono> :unique-id "mono2") (monos-of system))
  system)

(defmethod save-config ((system <jy-system>) config-name)
  (with-open-file
      (s *config-file* :direction :output :if-exists :supersede)
    (with-xml-output (s)
      (with-tag ("JY-SYSTEM" `(("CONFIG-NAME" ,config-name)))
	(with-tag ("DETECTORS")
	  (dolist (detector (detectors-of system))
	    (with-tag ("DETECTOR" `(("NAME" "A Detector")
				    ("ID" ,(unique-id-of detector)))))))
	(with-tag ("MONOS")
	  (dolist (mono (monos-of system))
	    (with-tag ("MONO" `(("NAME" "A Mono")
				("ID" ,(unique-id-of mono)))))))))))

(defun load-element-fn (name attributes seed)
  (cond
    ((string= name "JY-SYSTEM")
     (let ((system (make-instance '<jy-system>)))
       (setf (parent-system-of (experiment-engine-of system)) system)
       (setf (name-of system) (cdr (assoc :config-name attributes)))
       (setf seed system)))
    ((string= name "DETECTOR")
     (let ((detector (make-instance '<jy-detector>)))
       (setf (device-name-of detector) (cdr (assoc :name attributes)))
       (setf (unique-id-of detector) (cdr (assoc :id attributes)))
       (push detector (detectors-of seed))))
    ((string= name "MONO")
     (let ((mono (make-instance '<jy-mono>)))
       (setf (device-name-of mono) (cdr (assoc :name attributes)))
       (setf (unique-id-of mono) (cdr (assoc :id attributes)))
       (push mono (monos-of seed)))))
  seed)

(defun done-element-fn (name attributes parent-seed seed)
  (declare (ignorable name attributes parent-seed seed))
  seed)

(defun load-content-fn (text seed)
  (declare (ignorable text seed))
  seed)

(defun load-config (config-name)
  (declare (ignorable config-name))
  (with-open-file
      (s *config-file* :direction :input)
    (start-parse-xml s
                     (make-instance 'xml-parser-state
                                    :seed nil
                                    :new-element-hook #'load-element-fn
                                    :finish-element-hook #'done-element-fn
                                    :text-hook #'load-content-fn))))

;(defgeneric get-devices (system))
(defmethod get-devices ((system <jy-system>))
  (append (detectors-of system) (monos-of system)))

;(defgeneric get-device-by-id (system id))
(defmethod get-device-by-id ((system <jy-system>) id)
  (loop for device in (get-devices system)
     when (string= id (unique-id-of device))
       do (return device)))


