;; using graphviz to generate graphical representation of lisp tree-like/digraphs.
(defparameter *max-label-length* 30)

; this function transforms lisp identifier into graphviz valid ones
(defun dot-name (exp)
   (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

; crop a long text into a shorter one
(defun dot-label (exp)
   (if exp
      (let ((s (write-to-string exp :pretty nil)))
         (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

; this function talks in dot language. It represents nodes, for instance:
;    living_room [label="living room, there is a wizzard sleeping here"];
(defun nodes->dot (nodes)
   (mapc (lambda (node)
            (fresh-line)
            (princ (dot-name (car node)))
            (princ "[label=\"")
            (princ (dot-label node))
            (princ "\"];"))
         nodes))

; draw the edges (directed graph)
(defun edges->dot (edges)
   (mapc (lambda (node)
            (mapc (lambda (edge)
                     (fresh-line)
                     (princ (dot-name (car node)))
                     (princ "->")
                     (princ (dot-name (car edge)))
                     (princ "[label=\"")
                     (princ (dot-label (cdr edge)))
                     (princ "\"];"))
                  (cdr node)))
         edges))

; draw the edges (undirected graph)
(defun unedges->dot (edges)
   (maplist (lambda (lst)
               (mapc (lambda (edge)
                        (unless (assoc (car edge) (cdr lst))
                           (fresh-line)
                           (princ (dot-name (caar lst)))
                           (princ "--")
                           (princ (dot-name (car edge)))
                           (princ "[label=\"")
                           (princ (dot-label (cdr edge)))
                           (princ "\"];")))
                     (cdar lst)))
            edges))
               
; generate the dot data
(defun graph->dot (nodes edges)
   (princ "digraph {")
   (nodes->dot nodes)
   (edges->dot edges)
   (princ "}"))   

; generate the dot data (undirected graphs)
(defun ugraph->dot (nodes edges)
   (princ "digraph {")
   (nodes->dot nodes)
   (unedges->dot edges)
   (princ "}"))   

; generate a png file from the nodes and edges
(defun dot->png (fname thunk)
   (let ((dotfile (concatenate 'string fname ".dot")))
      (with-open-file (*standard-output*
                      dotfile
                      :direction :output
                      :if-exists :supersede)
          (funcall thunk))
      (eq (ext:shell (concatenate 'string "dot -T png -O " dotfile)) nil)))

; replace function
(defun remove-png-ext (the-string)
   (let ((l (length the-string)))
      (if (search ".png" the-string :start2 (- l 4))
         (subseq the-string 0 (- l 4))
         the-string)))

; generate a png from a directed graph
(defun graph->png (fname nodes edges)
   (dot->png (remove-png-ext fname) 
               (lambda ()
                  (graph->dot nodes edges))))

; generate the png for an undirected graph
(defun ugraph->png (fname nodes edges)
   (dot->png (remove-png-ext fname)
               (lambda ()
                  (ugraph->dot nodes edges))))

;;;; Some data for the examples ;;;;
;; description of the world
(defparameter *wizzard-nodes* '(
      (living-room (You are in the living-room. A wizzard is snoring loudly on the couch.))
      (garden (You are in a beatiful garden. There is a well in front of you))
      (attic (You are in the attic. There is a gigant welding torch in the corner.))))

;; edges (definition of the paths available between locations
(defparameter *wizzard-edges* '(
      (living-room (garden west door) (attic upstairs ladder))
      (garden (living-room east door))
      (attic (living-room downstairs ladder))))
