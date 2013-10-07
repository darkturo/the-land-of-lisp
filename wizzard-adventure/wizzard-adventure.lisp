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

;; allowed commands
(defparameter *allowed-commands* '(help look walk pickup inventory))

;; help text
(defparameter *help-text* '((help (prints this message))
                            (look (tell the player where he is in the scenario))
                            (walk (walk into the specified direction))
                            (pickup (take an object from the ground))))

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

;; making the interface a little bit nicer to the user
(defun game-read ()
   (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
      (flet ((quote-it (x)
                  (list 'quote x)))
         (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
   (if (member (car sexp) *allowed-commands*)
       (eval sexp)
      '(i do not know that command.))) 

(defun tweak-text (lst cap lit)
   (when lst
      (let ((item (car lst))
            (rest (cdr lst)))
         (cond
            ((eq item #\space) (cons item (tweak-text rest cap lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest cap (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (cap (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
(defun game-print (lst)
   (princ (coerce (tweak-text (coerce (string-trim "() " 
                                                   (prin1-to-string lst))
                                      'list)
                              t
                              nil)
                   'string))
   (fresh-line))
         

(defun game-repl ()
   (let ((cmd (game-read)))
      (unless (eq (car cmd) 'quit)
         (game-print (game-eval cmd))
         (game-repl))))   

;; the game
(defun help ()
   (mapcar (lambda (l) (assoc l *help-text*)) *allowed-commands*))

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

;; start game
;(print "To start the game type '(game-repl)'") 
;(game-repl)
