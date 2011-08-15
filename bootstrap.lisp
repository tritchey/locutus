;;;; functions for handling bootstrapping the agent
;;;; first, an agent will try to find other agents
;;;; within its local area network through zeroconf

(defvar *dns-sd-service* nil)

(defun register-agent (agent)
  (setf *dns-sd-service* 
	(make-instance 'dns-sd:service
		       :name (print-id (id agent) nil)
		       :type "_warwick._tcp"
		       :port (port agent)
		       :txt-record  
		       (dns-sd:build-txt-record `(("txtvers" . "2")
						  ("name" . ,(name agent))
						  ("version" . ,*version*)
						  ("rem" . "locutus lives")))))
    (dns-sd:publish *dns-sd-service* nil)
    (dns-sd:process-dns-sd-events 3.0))