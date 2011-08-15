;;;; define the agent class that holds information like
;;;; name, id, group, etc.

(defvar *self* nil)

(defclass agent ()
  ((id :accessor id
       :initarg :id
       :initform 0)
   (name :accessor name
	 :initarg :name
	 :initform "unknown")
   (port :accessor port
	 :initarg :port
	 :initform *default-port*)))
