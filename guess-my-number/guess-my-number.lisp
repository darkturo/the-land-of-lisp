(defparameter *upper-limit* 100)
(defparameter *lower-limit* 1)

(defun guess-my-number ()
   (ash (+ *upper-limit* *lower-limit*) -1))

