(defpackage :jy-system
  (:nicknames :jys)
  (:use :cl :cl-user :bordeaux-threads :jy-util :jy-devices :xml-emitter :s-xml)
  (:export

   ;; classes
   #:<jy-operation>
   #:<jy-axis>
   #:<jy-init-engine>
   #:<jy-experiment-engine>
   #:<jy-configure-engine>
   #:<jy-preview-engine>
   #:<jy-system>

   ;; accessors
   #:device-of
   #:op-action-of
   #:op-command-of
   #:op-parameters-of
   #:experiment-engine-of
   #:begin-of
   #:end-of
   #:increment-of
   #:number-of
   #:detectors-of
   #:monos-of
   
   ;; methods
   #:do-it
   #:validate-it
   #:configure
   #:add-start-op
   #:get-device-by-id
   #:get-available-signals
   #:execute
   #:add-axis
   #:add-bound-op
   #:add-data-op
   #:init
   #:do-experiment

   #:save-config
   #:load-config
   ))

