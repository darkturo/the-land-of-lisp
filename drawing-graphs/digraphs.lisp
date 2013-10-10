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

; draw the edges
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
               
; generate the dot data
(defun graph->dot (nodes edges)
   (princ "digraph {")
   (nodes->dot nodes)
   (edges->dot edges)
   (princ "}"))   
