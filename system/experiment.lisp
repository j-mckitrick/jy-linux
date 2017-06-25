(in-package :jy-system)

;(declaim (optimize debug))
(push :exp-debug *features*)

;; experiment
(defmethod add-start-op ((exp <jy-experiment-engine>) (op <jy-operation>))
  (setf (start-ops-of exp) (append (start-ops-of exp) (list op))))

(defmethod add-axis ((exp <jy-experiment-engine>) (axis <jy-axis>))
  (setf (gethash (number-of axis) (axes-of exp)) axis))

(defjymethod get-axis (exp <jy-experiment-engine> axis-num)
  (gethash axis-num (axes-of exp)))

(defjymethod validate (exp <jy-experiment-engine> ops)
  (mapcar #'validate-it ops))

(defjymethod preflight (exp <jy-experiment-engine>)
  (validate exp (start-ops-of exp)))

(defmethod get-available-signals ((exp <jy-experiment-engine>))
  (preflight exp)
  (nreverse (mapcar #'create-signal-details (detectors-of (parent-system-of exp)))))

(defvar *done-p* nil)

(defsink sink-fn (*data-ready* *data-lock* :one-time t)
  (unless *done-p*
    (jy-log "Got data event! ~A" *data*)
    ;(jyd::fire-handshake-event)
    ;(jy-log "Fired handshake.")
    (jy-log "... leaving sink")
   *data*))

(defparameter *ready-to-start* nil)

(defcp fire-start-event (*start-event* *start-lock*))
(defsink wait-start-event (*start-event* *start-lock* :one-time t)
    (jy-log "Start event received."))

;; this *might* be better as a class
(defparameter *experiment-params* nil)

(defun experiment-thread-fn ()
  (jy-log "Running experiment.")
  (jy-log "Execute setup.")
  ;(jy-log "Firing start event.")
  ;(wait-start-event)
  ;(jy-log "Start event fired.")
  ;(sleep 1)
  (with-lock-held (*start-lock*)
    (do-axis (gethash 1 (axes-of *experiment-params*)))
    (jy-log "Execute cleanup.")
    (jy-log "Setting done condition.")
    (jy-log "Setting done data event.")
    (setf *done-p* t)
    ;;(sleep 0.1)
	)
  (jyd::fire-event-data))

(defun experiment-fn ()
  (jy-log "Running experiment function (non-threaded).")
  (jy-log "Execute setup.")
  (do-axis (gethash 1 (axes-of *experiment-params*)))
  (jy-log "Execute cleanup.")
  (jy-log "Setting done condition.")
  (jy-log "Setting done data event.")
  (setf *done-p* t))

(defun sink-manager (exp)
  (let ((tallied-signals (copy-list (signals-of exp))))
    (jy-log "Copying signals ~A" (signals-of exp))
    (jy-log "Tallied signals ~A" tallied-signals)
    (jy-log "Waiting for start.")
    ;(fire-start-event)
    (loop do
	 (jy-log "Entering sink...")
	 (sink-fn)
	 (jy-log "Got signal ~A" (first *data*))
	 (vector-push-extend (second *data*) (gethash (first *data*) (result-of exp)))
	 (jy-log "Tallied signals before ~A" tallied-signals)
	 (setf tallied-signals (remove-if (lambda (x) (string= x (first *data*)))
					  tallied-signals))
	 (jy-log "Tallied signals after ~A" tallied-signals)
	 (jy-log "Length of data ~A" (length (gethash (first *data*) (result-of exp))))
	 (jy-log "Points expected ~A" (points-in exp))
	 (when (or *done-p*
		   (and
		    (>= (length (gethash (first *data*) (result-of exp)))
			(points-in exp))
		    (= (length tallied-signals) 0)))
	   (jy-log "Exiting sink manager thread.")
	   (return))
	 (unless (plusp (length tallied-signals))
	   (setf tallied-signals (copy-list (signals-of exp)))))))

(defjymethod setup-sinks (exp <jy-experiment-engine>)
  (jy-log "Startops: ~A~%" (start-ops-of exp))
  (let ((all-devices (mapcar (lambda (op) (unique-id-of (device-of op)))
			     (start-ops-of exp))))
    (jy-log "All devices: ~A~%" all-devices)))

(defjymethod setup-a-signal (exp <jy-experiment-engine> exp-signal num-points)
  (let ((sig-id (getf exp-signal :id))
	(x-size (getf exp-signal :x-size))
	(y-size (getf exp-signal :y-size))
	(z-size (getf exp-signal :z-size))
	(data-format (getf exp-signal :data-format)))
    (jy-log "Setup a signal: ~A" sig-id)
    (jy-log "Data dimensions: ~Ax~Ax~A" x-size y-size z-size)
    (jy-log "Data format: ~A" data-format)
    (unless (member sig-id (signals-of exp) :test #'equal)
      (push sig-id (signals-of exp)))
    (setf (gethash sig-id (result-of exp))
	  (make-array num-points :fill-pointer 0 :adjustable t))))

(defjymethod setup-signals (exp <jy-experiment-engine>)
  ;; initialize result object
  (setf (signals-of exp) nil)
  (setf (result-of exp) (make-hash-table :test 'equal))
  (let ((axis (get-axis exp 1)))
    (setf (points-in exp) (/ (1+ (- (end-of axis) (begin-of axis)))
			     (increment-of axis)))
    (dolist (exp-signal (get-available-signals exp))
      (jy-log "Signal: ~A" exp-signal)
      (setup-a-signal exp exp-signal (points-in exp)))))

(defjymethod execute-sync (exp <jy-experiment-engine>)
  (setup-sinks exp)
  (setup-signals exp)
  (jy-log "Signals: ~A" (signals-of exp))
  ;; create experiment complete event
  (setf *experiment-params* exp)
  (setf *done-p* nil)
  (setf *data* nil)
  ;; create experiment thread, start sink manager
  (let ((exp-thread (sb-thread:make-thread 'experiment-fn)))
    (jy-log "Started experiment thread.~%")
    #- (and) (when (sb-thread:thread-alive-p exp-thread)
      (sink-manager exp))
    ;; Shouldn't happen.
    (when (sb-thread:thread-alive-p exp-thread)
      (jy-log "Ahh! Joining exp-thread~%")
      (sb-thread:join-thread exp-thread))
    #+exp-debug (format t "Done: ~A~%" *done-p*)
;    #+exp-debug (format t "Experiment return value: ~A~%" exp-result)
    ;(format t "Exp result object: ~A~%" (result-of exp))
    (loop
       for data being the hash-values in (result-of exp) using (hash-key signal)
	 do (format t "Signal: ~A Data: ~A~%" signal data))))

(defjymethod execute (exp <jy-experiment-engine>)
  (jy-log "----------")
  (setup-sinks exp)
  (setup-signals exp)
  (jy-log "Signals: ~A" (signals-of exp))
  ;; create experiment complete event
  (setf *experiment-params* exp)
  (setf *done-p* nil)
  (setf *data* nil)
  ;; create experiment thread, start sink manager
  (let (exp-thread)
    (with-lock-held (*start-lock*)
      (setf exp-thread (make-thread 'experiment-thread-fn))
      (jy-log "Started experiment thread.~%"))
    (when (thread-alive-p exp-thread)
      (sink-manager exp))
    ;; Shouldn't happen.
    (when (thread-alive-p exp-thread)
      (jy-log "Ahh! Joining exp-thread")
      (join-thread exp-thread))
    #+exp-debug (format t "Done: ~A~%" *done-p*)
;    #+exp-debug (format t "Experiment return value: ~A~%" exp-result)
    ;(format t "Exp result object: ~A~%" (result-of exp))
    (loop
       for data being the hash-values in (result-of exp) using (hash-key signal)
	 do (format t "Signal: ~A ~A Data: ~A~%" signal (length data) data))
	)
  (jy-log "----------~%"))

(defparameter *test-start-var* nil)

(defcp fire-test-start-event (*test-start-event* *test-start-lock*))
(defsink wait-test-start-event (*test-start-event* *test-start-lock* :one-time t)
  (setf *test-start-var* nil)
  (jy-log "Wait test start event received."))

(defcp fire-test-data-event (*test-data-event* *test-data-lock*))
(defsink wait-test-data-event (*test-data-event* *test-data-lock*)
  (jy-log "Wait test data event received."))

(defun event-signaler-fn ()
  (jy-log "Waiting for start.")
  (wait-test-start-event)
  (jy-log "Running.")
;  (dotimes (i 10)
;    (sleep 1)
;    (fire-test-data-event))
  (jy-log "Setting done condition."))

(defun event-listen ()
  (setf *test-start-var* t)
  (fire-start-event)
  (dotimes (i 10)
    (wait-test-data-event)))

(defun sink-test ()
  (let ((test-thread (sb-thread:make-thread 'event-signaler-fn)))
    (when (sb-thread:thread-alive-p test-thread)
      (event-listen))))

