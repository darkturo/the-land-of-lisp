;; using graphviz to generate graphical representation of lisp tree-like/digraphs.
(defparameter *max-label-length* 30)

; this function transforms lisp identifier into graphviz valid ones
(defun dot-name (exp)
   (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

