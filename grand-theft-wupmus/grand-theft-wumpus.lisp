(load "graph-util.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cops-odds* 15)

(defun random-node ()
  (+ 1 (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* 
                        collect (edge-pair (random-node) (random-node)))))

; get the list of edges that goes from the specified node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                  edge-list))

; get the list of the all the reachable nodes from an specified node 
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
         (traverse node))
      visited))

; find islands, i.e. disconnected nodes
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes edge-list))) 
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
         (find-island nodes))
      islands))

; it will trace the islands with bridges
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands)) 
            (connect-with-bridges (cdr islands)))))

; it will connect-all-islands found with find-islands
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i)) 
         (edege-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cops-odds*)))
                              edge-list
                              )))
      (add-cops (edge-to-alist edge-list) cops)))

(defun add-cops (edge-alist edges-with-cops)
   (mapcar (lambda (x)
               (let ((node1 (car x))
                     (node1-edges (cdr x)))
                 (cons node1
                       (mapcar (lambda (edge)
                                 (let ((node2 (car edge)))
                                   (if (interesection (edge-pair node1 node2) 
                                                      edges-with-cops
                                                      :test #'equal)
                                     (list node2 'cops)
                                     edge)))
                               node1-edges))))
           edge-alist))

(defun edge-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

; get the neighbors of a given node
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

; hmmm, it seems that this function does the same as the preivous ... 
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))
