(defpackage :jy-util
  (:nicknames :jyu)
  (:use :cl :cl-user :bordeaux-threads)
  (:export

   ;; macros
   #:defcp
   #:defsink

   ;; functions
   #:jy-log
   ))