;; description of the world
(defparameter *nodes* '(
      (living-room (You are in the living-room. A wizzard is snoring loudly on the couch.))
      (garden (You are in a beatiful garden. There is a well in front of you))
      (attic (You are in the attic. There is a gigant welding torch in the corner.))))

;; edges (definition of the paths available between locations
(defparameter *edges* '(
      (living-room (garden west door) (attic upstairs ladder))
      (garden (living-room east door))
      (attic (living-room downstairs ladder))))

;; objects available in the scenario, and their location
(defparameter *objects* '(
      whiskey bucket frog chain))

(defparameter *object-locations* '(
      (whiskey living-room)
      (bucket living-room)
      (chain garden)
      (forg garden)))

;; variable to track the current location
(defparameter *location* 'living-room) 

;; functions to support the logic of the game
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defun describe-path (edge)
   `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
   (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (location objects objects-location)
   (labels 
         ((at-loc-p (obj)
            (eq (cadr (assoc obj objects-location)) location)))
         (remove-if-not #'at-loc-p objects)))

(defun describe-objects (location objects objects-location)
   (labels
         ((describe-obj (obj)
            `(you see a ,obj on the floor.)))
         (apply #'append (mapcar #'describe-obj (objects-at location objects objects-location)))))

;; the game
(defun look ()
   (append (describe-location *location* *nodes*)
           (describe-paths *location* *edges*)
           (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
   (let ((next (find direction 
                  (cdr (assoc *location* *edges*))
                  :key #'cadr)))
      (if next 
         (progn (setf *location* (car next))
               (look))
         '(you cannot go that way.))))

(defun pickup (object)
   (cond ((member object (objects-at *location* *objects* *object-locations*))
            (push (list object 'body) *object-locations*)
            `(you are now carriying the ,object))
            (t '(you cannot get that))))

(defun inventory ()
   (cons 'items- (objects-at 'body *objects* *object-locations*)))
