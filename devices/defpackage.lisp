(defpackage :jy-devices
  (:nicknames :jyd)
  (:use :cl :cl-user :jy-util)
  (:export
   
   ;; classes
   #:<jy-device>
   #:<jy-detector>
   #:<jy-mono>

   ;; accessors
   #:unique-id-of
   #:device-name-of

   ;; methods
   #:action-set
   #:action-get
   #:validate-set
   #:validate-get
   #:initialize
   #:get-data
   #:move
   #:create-signal-details

   ;; variables
   #:*data*
   #:*data-lock*
   #:*data-ready*
   ))

