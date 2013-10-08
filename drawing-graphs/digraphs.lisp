;; using graphviz to generate graphical representation of lisp tree-like/digraphs.
(defparameter *max-label-length* 30)

; this function transforms lisp identifier into graphviz valid ones
(defun dot-name (exp)
   (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
   (if exp
      (let ((s (write-to-string exp :pretty nil)))
         (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

