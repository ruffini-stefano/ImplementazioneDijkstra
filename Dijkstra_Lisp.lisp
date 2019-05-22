(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
	(list 'vertex graph-id vertex-id)))

(defun graph-vertices (graph-id)
  (let (( keys ())
	(vals ()))
    (maphash (lambda (k v)
	       (cond
		((eql (second k) graph-id)
		 (push k keys)
	       	 (push v vals))))
	     *vertices*)
    keys))

(defun new-arc (graph-id vertex1-id vertex2-id &optional (weight 1))
  (setf (gethash (list 'arc graph-id vertex1-id vertex2-id weight) *arcs*)
	(list 'arc graph-id vertex1-id vertex2-id weight)))

(defun graph-arcs (graph-id)
  (let (( keys ())
	(vals ()))
    (maphash (lambda (k v)
	       (cond
		((eql (second k) graph-id)
		 (push k keys)
		 (push v vals))))
	     *arcs*)
    keys))


(defun graph-arcs-sssp (graph-id v1 v2)
  (let (( keys ())
	( vals ()))
    (maphash (lambda (k v)
	       (cond
		((and
		  (eql (second k) graph-id)
		  (eql (third k) v1)
		  (eql (fourth k) v2))
		 (push k keys)
		 (push v vals))))
	     *arcs*)
    keys))

(defun graph-print (graph-id)
  (let (
	(l1 (graph-vertices graph-id))
	(l2 (graph-arcs graph-id)))
   (print(append l1 l2))))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((keys ())
	(vals ()))
    (maphash (lambda (k v)
	       (cond
		((and
		 (eql (second k) graph-id)
		 (eql (third k) vertex-id))
		 (push k keys)
		 (push v vals))))		 
	     *arcs*)
    keys))

(defun delete-vertices (graph-id)
  (maphash (lambda (k v)
	     (let  ((g (second k)))
	       (cond ((eql g graph-id)
		      (remhash k *vertices*))
		     ( (eql v 'ciaooo) t ))))
	   *vertices*))

(defun delete-arcs (graph-id)
  (maphash (lambda (k v)
	     (let ((g (second k)))
	       (cond ((eql g graph-id)
		      (remhash k *arcs*))
		     ( (eql v 'ciaooo) t ))))
	   *arcs*))

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (delete-vertices graph-id)
  (delete-arcs graph-id))

(defparameter *heaps* (make-hash-table :test #'equal))

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (second l)))

(defun heap-size (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (third l)))

(defun heap-capacity (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (length(fourth l))))

(defun heap-actual-heap (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (fourth l)))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

(defun heap-empty (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (cond ((= (third l) 0) t))))

(defun heap-not-empty (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (cond ((/= (third l) 0) t))))

(defun increase-heap-size (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (setf (third l) (+ (third l) 1))))

(defun decrease-heap-size (heap-id)
  (let ((l (gethash heap-id *heaps*)))
    (setf (third l) (- (third l) 1))))

(defun heap-insert-last (heap-id K V)
  (let (( l (gethash heap-id *heaps*))
	( p (heap-size heap-id)))
    (setf (aref (fourth l) p) (list K V))
    (increase-heap-size heap-id)
    t))

(defun heap-swap2 (heap-id p1 p0)
  (let (( l (heap-actual-heap heap-id)))
    (let (( k0 (first (aref l p0)))
	  ( k1 (first (aref l p1))))
      (cond
       ((< k1 k0)
	(let (( c (aref l p0)))
	  (setf (aref l p0) (aref l p1))
	  (setf (aref l p1) c)))))))

(defun heap-heapify3 (heap-id s)
  (cond
   ((= s 1) t)
   (t
    (heap-swap2 heap-id (- s 1) (- s 2))
    (heap-heapify3 heap-id (- s 1)))))

(defun heap-heapify (heap-id)
  (let (( s (heap-size heap-id)))
    (cond
     ((= s 1) t)
     ((= s 2) (heap-swap2 heap-id 1 0))
     ((> s 2) (heap-heapify3 heap-id s)))))

(defun heap-insert (heap-id K V)
  (let (( c (heap-capacity heap-id))
	( s (heap-size heap-id)))
    (cond
     ((< s c)
      (heap-insert-last heap-id K V)
      (heap-heapify heap-id)
       t)
     (t
      (print 'array-pieno)
      nil))))

(defun heap-swap-nill (heap-id p1 p0)
  (let (( l (heap-actual-heap heap-id)))
    (setf (aref l p0) (aref l p1))
    (setf (aref l p1) nil)))

(defun heap-swap-all (heap-id i s)
  (cond
   ((= i s) t)
   (t
    (heap-swap-nill heap-id (+ i 1) i)
    (heap-swap-all heap-id (+ i 1) s))))

(defun heap-heapify-extract (heap-id)
  (let (( s (heap-size heap-id)))
    (cond
     ((= s 0) t)
     ((= s 1) (heap-swap-nill heap-id 1 0))
     ((> s 1) (heap-swap-all heap-id 0 s)))))

(defun heap-extract (heap-id)
  (let (( l (heap-actual-heap heap-id)))
    (let ((  v (aref l 0)))
      (setf (aref l 0) nil)
      (decrease-heap-size heap-id)
      (heap-heapify-extract heap-id)
      v)))

(defun find-key (array s key)
  (cond
   ((< s 0) nil)
   (t
    (let (( k (first (aref array s))))
      (cond
       ((= k key) s)
       (t (find-key array (- s 1) key)))))))

(defun heap-shift-d (array s p)
  (cond
   ((= p s) t)
   ((> (first(aref array p)) (first(aref array (+ p 1))))
    (let (( c (aref array p)))
      (setf (aref array p) (aref array (+ p 1 )))
      (setf (aref array (+ p 1)) c)
      (heap-shift-d array s (+ p 1))))))

(defun heap-shift-s (array s p)
  (cond
   ((= p 0) t)
   ((< (first(aref array p)) (first(aref array (- p 1))))
    (let (( c (aref array p)))
      (setf (aref array p) (aref array (- p 1)))
      (setf (aref array (- p 1)) c)
      (heap-shift-s array s (- p 1))))))

(defun heap-shift (array s p)
  (cond
   ((= p 0) (heap-shift-d array s p))
   ((= p s) (heap-shift-s array s p))
   (t
    (heap-shift-d array s p)
    (heap-shift-s array s p))))

(defun heap-modify-key (heap-id new-key old-key)
  (let (( l (heap-actual-heap heap-id))
	( e (find-key
	     (heap-actual-heap heap-id)
	     (- (heap-size heap-id) 1)
	     old-key)))
    (cond
     ((eql e nil) nil)
     (t
      (let ( (v (second(aref l e))))
	(setf (aref l e) (list new-key v))
	(heap-shift l (- (heap-size heap-id) 1) e)
	t)))))

(defun heap-head (heap-id)
  (aref (heap-actual-heap heap-id) 0))

(defun heap-print (heap-id)
  (print(gethash heap-id *heaps*)))

(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *dist* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defun sssp-dist (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *dist*))

(defun sssp-change-dist (graph-id vertex-id new-dist)
  (setf (gethash (list graph-id vertex-id) *dist*)
	new-dist)
  nil)

(defun sssp-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))

(defun sssp-change-previous (graph-id vertex-id new-previous)
  (setf (gethash (list graph-id vertex-id) *previous*)
	new-previous)
  nil)

(defun sssp-visited (graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*)
	t))

(defun aggiorna-vertice (graph-id vertex-id a p)
  (let (( v (fourth(nth p a)))
	( w (fifth(nth p a))))
    (let (( old-dist (sssp-dist graph-id v))
	  (new-dist (+ (sssp-dist graph-id vertex-id) w)))
      (cond
       ((eql old-dist nil)
	(sssp-change-dist graph-id v new-dist)
	(sssp-change-previous graph-id v vertex-id)
	(heap-insert graph-id new-dist v))
       ((< new-dist old-dist)
	(sssp-change-dist graph-id v new-dist)
	(sssp-change-previous graph-id v vertex-id)
	(heap-modify-key graph-id new-dist old-dist))))))

(defun aggiorna-vertici (graph-id vertex-id a p)
  (cond((>= p 0)
	(aggiorna-vertice graph-id vertex-id a p)
	(aggiorna-vertici graph-id vertex-id a (- p 1)))))

(defun aggiorna-tot (graph-id vertex-id)
  (let (( a (graph-vertex-neighbors graph-id vertex-id)))
    (let (( l (length a)))
      (aggiorna-vertici graph-id vertex-id a (- l 1)))))

(defun sssp (graph-id Source)
  (sssp-change-dist graph-id Source 0)
  (new-heap graph-id (length(graph-vertices graph-id)))
  (aggiorna-tot graph-id Source)
  (sssp-visited graph-id Source)
  (sssp-aus graph-id)
  nil)

(defun sssp-aus (graph-id)
  (cond((heap-not-empty graph-id)
	(let ((n (second(heap-extract graph-id))))
	  (aggiorna-tot graph-id n)
	  (sssp-visited graph-id n)
	  (sssp-aus graph-id)))))

(defun sssp-shortest-path-aus (graph-id Source V l)
  (let ((v1 (sssp-previous graph-id V)))
    (cond((eql V Source) l)
	 (t
	  (push (first(graph-arcs-sssp graph-id v1 V)) l)
	  (sssp-shortest-path-aus graph-id Source v1 l)))))

(defun sssp-shortest-path (graph-id Source V)
  (sssp-shortest-path-aus graph-id Source V ()))