;;;; route storage, lookup, etc.

;;; each route holds the route id (a bit-vector)
;;; the address (ipv4/6) and a proximity metric
(defclass route ()
  ((id :accessor id
       :initarg :id
       :initform 0)
   (address :accessor address
	    :initarg :address
	    :initform 0)
   (proximity :accessor proximity
	      :initarg :proximity
	      :initform 0)))

(defmethod print-object ((r route) stream)
  (with-slots (id) r
    (print-id id stream)))

;;; utility function for creating a route with a random id
(defun make-random-route ()
  "generate a route with random settings"
  (make-instance 'route :id (make-random-guid)))

(defclass routing-table ()
  ((root :accessor root
	 :initarg :root
	 :initform nil)
   (k :accessor k
      :initarg :k
      :initform 20)
   (myid :accessor myid
	 :initarg :myid
	 :initform 0)
   (split-prefix :accessor split-prefix
		 :initarg :split-prefix
		 :initform -1)))

;;; convenience macro
(defun make-routing-table (&rest args)
  (apply #'make-instance 'routing-table args))

;;; get a kbucket that most closely matches the prefix
;;; of id in a tree
(defun get-kbucket (id table)
  "find the kbucket that has the same prefix as id"
  (labels ((findkb (id tree level)
	     (if (null tree)
		 nil
		 (if (not (typep (car tree) 'route))
		     (if (logbitp level id)
			 (findkb id (cdr tree) (1- level))
			 (findkb id (car tree) (1- level)))
		     tree))))
    (findkb id (root table) *idlength*)))

;;; get the exact route that matches an id
;;; returns nil of no route matches
(defun get-route (id table)
  "find the route with a specific id"
  (find id (get-kbucket id table) :key 'id))
  
;;; take a sub-tree, and its level, and split it into
;;; two seperate branches, returning a cons of the branches
(defun kbucket-split (table tree level)
  "split a kbucket using the appropriate bit of the ids"
  (if (find (myid table) tree :key 'id) ; support relaxed routing table
      (incf (split-prefix table)))      ; by inc the match prefix
  (let ((left nil)
	(right nil))
    (dolist (r tree)
      (if (not (logbitp level (id r)))
	  (setf left (cons r left))
	  (setf right (cons r right))))
    (cons left right)))

;;; decide whether to split a kbucket
(defun kbucket-splitp (table bucket id)
  (or (find (myid table) bucket :key 'id) ; always split the bucket with our id
      ;; or, split any buckets that match the split-prefix
      (let ((spec (byte (split-prefix table) 
			(- *idlength* (split-prefix table)))))
	(eql (mask-field spec id)
	     (mask-field spec (myid table))))))

;;; insert a route into a k-bucket based tree
;;; the optional level command is used when called
;;; recursively on subnodes to let us know which
;;; bit of the id to use to make a path decision
(defun insert-route (obj table) 
  "insert a route into the k-bucket tree"
  (labels ((insert (obj tree level)
	     (if (null tree)
		 (list obj)
		 (if (not (typep (car tree) 'route))
		     ;; we are at an upper node in the tree
		     (if (logbitp level (id obj))
			 (cons (car tree)
			       (insert obj (cdr tree) (1- level)))
			 (cons (insert obj (car tree) (1- level))
			       (cdr tree)))
		     ;; we have reached a kbucket
		     (if (find (id obj) tree :key 'id)
			 ;; the key already exists, move to front
			 (cons obj (remove-if #'(lambda (x) (eql (id obj) (id x))) tree))
			 ;; the key is not in here
			 (if (< (length tree) *k*)
			     ;; we have room, so just add
			     (cons obj tree)
			     ;; no room in the inn
			     (if (kbucket-splitp table tree (id obj))
				 ;; split this list and insert on the new node
				 (insert obj (kbucket-split table tree level) level)
				 ;; no joy splitting, lets see if we can eject the LRU
				 (if (not (send-ping (last tree))) 
				     (cons obj (butlast tree)) ; yes - tag new on front
				     ;; the LRU responded, move to front of bucket
				     (append (last tree) (butlast tree))))))))))
    (insert obj (root table) *idlength*)))

;;; define the setf expansion for convenience
(defun (setf get-route) (rt id table)
  (setf (root table) (insert-route rt table))
  (get-route id table)) 
  
;;; sort a kbucket based on xor distance
(defun kbucket-dsort (id bucket)
  (sort 
   (copy-list bucket) 
   #'< :key #'(lambda (r) (guid-distance id (id r)))))
  
  
;;; return a route regardless of whether we
;;; have an exact match or not
(defun find-closest-route (id)
  "find the closest route we know about in id-space"
  (kbucket-dsort id (find-kbucket id)))
  
(defun kbucket-nclosest (id bucket n)
  (let ((routes (kbucket-dsort id bucket)))
    (if (>= (length routes) n)
	(subseq routes 0 n)
	routes)))
  
(defun find-routes (id n)
  (labels ((findr (id tree n level)
	     (if (null tree)
		 nil
		 (if (not (typep (car tree) 'route))
		     (let ((routes nil) (main nil) (remain nil))
		       (if (logbitp level id)
			   (setf main (cdr tree) remain (car tree))
			   (setf main (car tree) remain (cdr tree)))
		       (setf routes (findr id main n (1- level)))
		       (kbucket-dsort id (nconc routes 
						(findr id remain 
						       (- n (length routes)) 
						       (1- level)))))
		     (kbucket-nclosest id tree n)))))
    (findr id *routes* n *idlength*)))

;;; return up to *k* closest routes we know about
(defun find-kroutes (id)
  "find the *k* closest routes we know about to id"
  (find-routes id *k*))

;;; get the number of entries in the routing tree
(defun num-routes (table)
  "count the number of routes in tree"
  (let ((tree (root table)))
    (if (null tree)
	0
	(if (not (typep (car tree) 'route))
	    (+ (num-routes (car tree)) (num-routes (cdr tree)))
	    (length tree)))))





