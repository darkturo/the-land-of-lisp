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

;; functions for the game
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
