;; -*- mode: lisp; encoding: utf-8 -*-

(defpackage :jy-asdf-system (:use :cl :asdf))
(in-package :jy-asdf-system)

(defsystem jy
  :name "jy"
  :components
  ((:module "util" :serial t
	    :components ((:file "defpackage")
			 (:file "events")))
   (:module "devices" :depends-on ("util") :serial t
	    :components ((:file "defpackage")
			 (:file "devices")))
   (:module "system" :depends-on ("devices") :serial t
	    :components ((:file "defpackage")
			 (:file "sys-defs")
			 (:file "operation")
			 (:file "axis")
			 (:file "system")
			 (:file "experiment")))
   (:file "start" :depends-on ("system")))
  :depends-on (:xml-emitter :s-xml :bordeaux-threads))


