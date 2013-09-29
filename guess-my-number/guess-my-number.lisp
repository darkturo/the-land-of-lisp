(defparameter *upper-limit* 100)
(defparameter *lower-limit* 1)

(defun guess-my-number ()
   (ash (+ *upper-limit* *lower-limit*) -1))

(defun bigger ()
   (setf *lower-limit* (1- (guess-my-number)))
   (guess-my-number))

(defun smaller ()
   (setf *upper-limit* (1+ (guess-my-number)))
   (guess-my-number))

(defun start-over ()

   )
