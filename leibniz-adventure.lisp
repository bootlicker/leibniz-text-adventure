(defparameter *nodes* '((beautiful-garden (DESCRIPTION))
                        
                        (manor-lobby (DESCRIPTION))
                        (street (DESCRIPTION))
                        (library (DESCRIPTION))
                                                
                        (dining-room (DESCRIPTION))
                        (drawing-room (DESCRIPTION))
                        (kitchen (DESCRIPTION))
                        
                        (shopkeepers-domain (DESCRIPTION))
                        (warehouse (DESCRIPTION))
                        (secret-control-room (DESCRIPTION))
                        
                        (trap-door-room (DESCRIPTION))
                        (riddle-of-monads (DESCRIPTION))
                        (science-of-monads (DESCRIPTION))
                        
                        ))

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

(defparameter *edges* '((beautiful-garden   (manor-lobby east door)  
                                            (street south alleyway)
                                            (library north-west gated-hedge))
                        
                        (manor-lobby        (beautiful-garden west door)
                                            (dining-room north-east double-door)
                                            (kitchen down dumb-waiter))
                        (dining-room        (manor-lobby south-west double-door)
                                            (drawing-room due-south double-door))
                        (drawing-room       (dining-room due-north double-door)
                                            (kitchen across shoot))
                        (kitchen            (manor-lobby up dumb-waiter)
                                            (drawing-room up-slightly shoot))
                                            
                        (street             (beautiful-garden north alleyway)
                                            (shopkeepers-domain forward window))
                        (shopkeepers-domain (street backward window)
                                            (warehouse behind counter))
                        (warehouse              (shopkeepers-domain front-of counter)
                                                (secret-control-room around aisle))
                        (secret-control-room    (warehouse under aisle))
                        
                        (library            (beautiful-garden south-east gated-hedge)
                                            (trap-door-room down trap-door))
                        (trap-door-room     (library up trap-door)
                                            (riddle-of-monads forward archway))
                        (riddle-of-monads   (trap-door-room backward archway)
                                            (science-of-monads forward staircase))
                        (science-of-monads  (riddle-of-monads backwards staircase))
                                            ))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(leibniz spectacles lecturn))

(defparameter *object-locations* '((leibniz beautiful-garden)
                                   (spectacles beautiful-garden)
                                   (lecturn library)))

(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t `(you cannot get the ,object))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
    (member object (cdr (inventory))))

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))
