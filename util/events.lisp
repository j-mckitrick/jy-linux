(in-package :jy-util)

#- (and)
(defmacro defcp (name (event lock))
  `(progn
	 (defvar ,event (make-waitqueue))
     (defvar ,lock (make-lock))
     (defun ,name ()
       (with-mutex (,lock)
										;	 (setf ,var t)
		 (condition-notify ,event)))))

(defmacro defcp (name (event lock))
  `(progn
	 (defvar ,event (make-condition-variable))
     (defvar ,lock (make-lock))
     (defun ,name ()
       (with-lock-held (,lock)
	 (condition-notify ,event)))))

#- (and)
(defmacro defsink (name (event lock &key one-time) &body body)
  `(defun ,name ()
     (,(if one-time `progn `loop)
      (with-mutex (,lock)
	(condition-wait ,event ,lock)
	;(unless ,var (return-from ,name))
	)
       ,@body)))

(defmacro defsink (name (event lock &key one-time) &body body)
  `(defun ,name ()
     (,(if one-time `progn `loop)
      (with-lock-held (,lock)
	(condition-wait ,event ,lock)
	;(unless ,var (return-from ,name))
	)
       ,@body)))

(defcp my-cp (my-event my-lock))

(defsink my-sink (my-event my-lock)
  (format t "Got it!~%"))

(defvar *log-lock* (make-lock))

(defun jy-log (&rest args)
  (with-lock-held (*log-lock*)
    (with-open-file (s "log/logfile" :direction :output :if-exists :append)
      (format s "logged @ ~A: " (get-internal-real-time))
      (apply #'format s args)
      (format s "~%"))))

