(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(I cannot ,command like that.)))
          (pushnew ',command *allowed-commands*)))
          
(defmacro game-action-mere-subject (command subj place &body body)
  `(progn (defun ,command (subject)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (have ',subj))
                ,@body
            '(It is not possible to ,command ,subj that.)))
          (pushnew ',command *allowed-commands*)))
          
(game-action-mere-subject talk leibniz beautiful-garden

; (defparameter *chain-welded* nil)

(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(game-action dunk bucket well garden
             (if *chain-welded* 
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water))
               '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly. 
                        he hands you the magic low-carb donut- you win! the end.))))

(game-action behold leibniz spectacles beautiful-garden
        '(Leibniz allows you to witness him in all his wigg-ed glory. He scowls, he simpers... Truly he is an ugly, smelly man. You wish you hadn't realised that this command was possible to execute in every single room of this game.))
