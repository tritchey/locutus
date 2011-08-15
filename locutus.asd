;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; LOCUTUS - the new paragent agent framework
;;;
;;; Copyright (c) 2005, 2006
;;; Paragent, LLC
;;; 
;;; This is the ASDF system definition.

(in-package :asdf)

(defsystem #:locutus
    :name "LOCUTUS"
    :author "Timothy Ritchey <tritchey@paragent.com>"
    :maintainer "Timothy Ritchey <tritchey@paragent.com>"
    :version "0.1"
    :licence "NONE"
    :description ""
    :long-description ""

    :depends-on (:cl-zeroconf)
    
    :components ((:file "package")
		 (:file "guid")
		 (:file "routes" :depends-on ("guid"))
		 (:file "network" :depends-on ("guid"))
		 (:file "krpc" :depends-on ("routes" "network"))
		 (:file "agent" :depends-on ("guid" "network"))
		 (:file "bootstrap" :depends-on ("agent"))
		 (:file "kernel" :depends-on ("bootstrap" "agent" "krpc"))))
