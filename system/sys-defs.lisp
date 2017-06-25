(in-package :jy-system)

;(declaim (optimize debug))

(defmacro defjymethod (name (obj class &rest args) &body body)
  `(progn
     (defgeneric ,name (,obj ,@args))
     (defmethod ,name ((,obj ,class) ,@args)
       ,@body)))




