;;;;

(defparameter *version* "0.1")
(defvar *hostname* nil)

(defun start-kernel ()
  (setf *myid* (make-random-guid))
  (setf *hostname* (machine-instance))
  (setf *routes* 
	(insert-route (make-instance 'route :id *myid*)))
  (setf *self* (make-instance 'agent :id *myid* :name *hostname* :port *default-port*))
  (register-agent *self*))