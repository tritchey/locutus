;;;; functions for handling our globally unique identifiers

(defvar *myid*)
(defparameter *idlength* 160)

(defun make-random-guid ()
  (random (expt 2 *idlength*)))

(defun guid-distance (first second)
  (logxor first second))

(defun print-id (id &optional stream)
    (format stream "~44,'0,':,8:x" id))




