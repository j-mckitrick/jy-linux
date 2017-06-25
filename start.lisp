(defpackage #:jy
  (:use #:cl #:cl-user #:jy-devices #:jy-system)
  (:export
   ;; globals
   #:*system*))

(in-package #:jy)

;; create system with some devices
(defparameter *system* (make-instance '<jy-system>))
;(configure *system*)
(setf *system* (load-config "hi"))
(init *system*)

;; add some startops
(dotimes (i 2)
  (let ((start-op (make-instance '<jy-operation>)))
    (setf (device-of start-op) (get-device-by-id *system* (format nil "scd~A" (1+ i))))
    (setf (op-action-of start-op) :action-set)
    (setf (op-command-of start-op) :cmd-int-time)
    (setf (op-parameters-of start-op) '(:exp-value 10))
    (add-start-op (experiment-engine-of *system*) start-op)))

(dotimes (i 2)
  (let ((start-op (make-instance '<jy-operation>)))
    (setf (device-of start-op) (get-device-by-id *system* (format nil "mono~A" (1+ i))))
    (setf (op-action-of start-op) :action-set)
    (setf (op-command-of start-op) :cmd-move)
    (setf (op-parameters-of start-op) '(:exp-value 100))
    (add-start-op (experiment-engine-of *system*) start-op)))

;; add an axis
(defparameter *axis* (make-instance '<jy-axis>))
(setf (number-of *axis*) 1)
(setf (begin-of *axis*) 1)
(setf (end-of *axis*) 10)
(setf (increment-of *axis*) 1)
(add-axis (experiment-engine-of *system*) *axis*)

(let ((bound-op (make-instance '<jy-operation>)))
  (setf (device-of bound-op) (get-device-by-id *system* "mono2"))
  (setf (op-action-of bound-op) :action-set)
  (setf (op-command-of bound-op) :cmd-move)
  (setf (op-parameters-of bound-op) (list :exp-value 50))
  (add-bound-op *axis* bound-op))

(let ((data-op (make-instance '<jy-operation>)))
  (setf (device-of data-op) (get-device-by-id *system* "scd1"))
  (setf (op-action-of data-op) :action-get)
  (setf (op-command-of data-op) :cmd-data)
  (add-data-op *axis* data-op))

(let ((data-op (make-instance '<jy-operation>)))
  (setf (device-of data-op) (get-device-by-id *system* "scd2"))
  (setf (op-action-of data-op) :action-get)
  (setf (op-command-of data-op) :cmd-data)
  (add-data-op *axis* data-op))

;(do-experiment *system*)

;(jy-system::execute-sync (jy-system::experiment-engine-of *system*))

(jy-util:defcp fire-test-event (*test-ready* *test-lock*))

(jy-util:defsink test-sink-fn (*test-ready* *test-lock* :one-time t)
  (jy-util:jy-log "Got test event."))

(defun test-thread ()
  (jy-util:jy-log "Sleeping before sink.")
  (sleep 1.0)
  (jy-util:jy-log "Waiting for event in test sink.")
  (test-sink-fn)
  (jy-util:jy-log "Ending thread."))

(defun test-sync ()
  (sb-thread:make-thread 'test-thread)
  (sleep 2.0)
  (fire-test-event)
  (jy-util:jy-log "Done main thread."))

(defun run-experiment ()
  (execute (experiment-engine-of *system*)))

