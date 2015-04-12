;;e FUNCTION DICTIONARY
;;
;;
;;
(setf *debug* nil)
(setf *random-state* (make-random-state t))
;; testing to see if i can commit this comment
(defun dprint (some-variable &optional (additional-message '()))
	"Debug Print - useful for allowing error/status messages
to be printed while debug=t."
	(if *debug*
		(progn 
			(if additional-message (print additional-message) nil) 
			(print some-variable))
		some-variable))

(defun random-elt (sequence)
  "Returns a random element from a sequence"
  (elt sequence (random (length sequence))))

(defun num-states (q-table)
  "Returns the number of states in a q-table"
  (first (array-dimensions q-table)))

(defun num-actions (q-table &optional state)
  "Returns the number of actions in a q-table"
  (second (array-dimensions q-table)))

(defun make-q-table (num-states num-actions)
  "Makes a q-table, with initial values all set to 0"
  (make-array (list num-states num-actions) :initial-element 0))

(defun max-q (q-table state)
  "Returns the highest q-value for a given state over all possible actions.
If the state is outside the range, then utility-for-outside-state-range is returned."
  (let* ((num-actions (num-actions q-table))
	 (best (aref q-table state (1- num-actions))))  ;; q of last action
    (dotimes (action (1- num-actions) best)  ;; all but last action...
      (setf best (max (aref q-table state action) best)))))
	  

(defun max-action (q-table state &optional val)
  "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random;
else val is returned instead when there's a tie. If state is outside the range, then an error is generated
 (probably array-out-of-bounds)."
  ;; a little inefficient, but what the heck...
  (let ((num-actions (num-actions q-table))
	(best (max-q q-table state))
	bag)
    (dotimes (action num-actions)
      (when (= (aref q-table state action) best)
	(push action bag)))
    (if (and val (rest bag))
	val
      (random-elt bag))))

(defparameter *basic-alpha* 0.5 "A simple alpha constant")
(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)

  
(defun q-learner (q-table reward old-state action current-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called
to provide the current alpha value."
  (if (< current-state 0)
	(setf current-state 0)
	nil
  )
  (dprint reward "you canned q-learner with reward as:")
  (setf *debug* nil)
  ;;equation is (qtable[oldstate][action] = (1-alpha) * qtable[oldstate][action] + alpha*(reward + gamma* (max-q q-table current-state) )
  
  (setf alpha (funcall alpha-func iteration))

  (setf minus-alpha (- 1 alpha))

  (setf the-max-action-q (max-q q-table current-state))
 
  (setf first-half-eq (* minus-alpha (aref q-table old-state action)))

  (setf second-half-eq (* alpha (+ reward (* gamma the-max-action-q))))
  
  (setf (aref q-table old-state action) (+ first-half-eq second-half-eq))
  
  q-table
  ;;; IMPLEMENT ME
)


;; Top-level nim learning algorithm.  The function works roughly like this...
;;
;; Make a q table.  Hint: make it 6 states larger than needed.  Can you see why?
;; Iterations times:
;;   Set state to 0  (no sticks removed from heap yet)
;;   Loop:
;;       old state <- state
;;       Determine and make my move, update state
;;       If I lost, set my reward to -1
;;       Determine and make my opponent's move, update state
;;       If the opponent lost, set my reward to +1
;;       If no reward has been set yet, set it to 0
;;       Update q table with the reward, old-state, my move, and current ("next") state
;;       If new state is bigger than the heap size, exit loop
;; Return q table

(defun learn-nim (heap-size gamma alpha-func num-iterations)
  "Returns a q-table after learning how to play nim"
 
  ;;make q table, ignore sean only do size 20.
  (let ((q-table (make-q-table (+ heap-size 1) 3)))
  
    ;;loop 1 to iterations
	(dotimes (i num-iterations)
		;;set state to heap-size
		(let ((current-state heap-size) (old-state 0))
			;;loop until game finished
			(setf current-state heap-size)
			(loop while (> current-state 0) do
				(dprint current-state "current state is:")
				;;before taking an action, record current state for later use
				(setf old-state current-state)
				;;take an action (call max-action state)
				(setf my-action-taken  (max-action q-table current-state) )
				(dprint my-action-taken "action taken:")
			
				;;randomly do something a bit different
				(if  (=  (random 2) 0)
					;; do something other than optimal
					(if (= (random 2) 0)
						(setf my-action-taken (- my-action-taken 1))
						(setf my-action-taken (+ my-action-taken 1)))
					nil)
				(setf my-action-taken (mod my-action-taken 3))
	
				;; calculate current state (modify current-state)
				(setf current-state (- (- current-state my-action-taken) 1))
				
				;;check to see if you've lost, if so then do a losing-update
				;;(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)

				(if (<= current-state 0)
					
					(setf q-table (q-learner q-table 1 old-state my-action-taken current-state gamma alpha-func i));;losing update
					(progn
						;;else we continue on, let the "opponent" make a move
						;;take an action (call max-action state)
						(setf opp-action-taken  (max-action q-table current-state) )
						(dprint opp-action-taken "action taken:")
				
						;; calculate current state (modify current-state)
						(setf current-state (- (- current-state opp-action-taken) 1))
						
				
						;;check if the opponent lost, if so then do a winning-update
						(if (<= current-state 0)
							(progn (if (< (- old-state current-state) 2) (print (- old-state current-state)) nil)  (setf q-table (q-learner q-table -1 old-state my-action-taken current-state gamma alpha-func i)) );;winning update
							;;if not the game is still going, learn from future rewards!
							(setf q-table (q-learner q-table 0 old-state my-action-taken current-state gamma alpha-func i));;future rewards update
						)
						;;else do a regular old update.
				)))))
				q-table))

(defun would-you-like-to-play ()
	"Returns true if the user wants to play a game of NIM"
	(y-or-n-p "Would you like to play a new game of NIM?"))

(defun ask-if-user-goes-first ()
  "Returns true if the user wants to go first"
  (y-or-n-p "Do you want to play first?"))
 
(defun make-user-move ()
  "Returns the number of sticks the user wants to remove"
  (let ((result))
    (loop
			(setf result (read))
     	(when (and (numberp result) (<= result 3) (>= result 1))
      	 (return result))
     	(format t "~%Please select no fewer than 1 and no more than 3! ~%How many sticks would you like to take?~%"))))
(defun make-user-move-3-nim ()
  "Returns the number of sticks the user wants to remove"
  (let ((result))
    (loop
			(setf result (read))
     	(when (and (numberp result) (<= result 9) (>= result 1))
      	 (return result))
     	(format t "~%Please select no fewer than 1 and no more than 9! ~%How many sticks would you like to take?(1,2,3 = take 1 2 or 3 from first pile, 4 5 6= take 1 2 3 from second pile, etc~%"))))


(defun play-nim (q-table max-heap-size)
  "Plays a game of nim.  Asks if the user wants to play first,
then has the user play back and forth with the game until one of
them wins.  Reports the winner."
	(let ((current-state max-heap-size) (old-state 0) (user-move 0))
			;;loop until user quits
			(print max-heap-size)
			 (loop while (would-you-like-to-play) do 
				(setf current-state max-heap-size)
				(format t  "~%~%~%Starting with ~A sticks.~%" current-state)
				;;loop until game finished
			  (loop while (> current-state 0) do
				  (progn 
						(setf my-action-taken  (max-action q-table current-state) )
				  ;;(dprint my-action-taken "action taken:")
				  (format t "~%Computer took ~A sticks." (1+ my-action-taken))
				  ;; calculate current state (modify current-state)
				  (setf current-state (- (- current-state my-action-taken) 1))
					(if (< current-state 1) (format t "~%YOU WIN!!!~%") (progn 
				  	(format t "~%Number of sticks in the pile after computer move: ~A ~%How many sticks would you like to take?~% " current-state)
						(setf current-state (- current-state (make-user-move)))
						(format t "~%~A~%" current-state)
						(if (< current-state 1) (format t "~%YOU LOSE!!!~%") nil)))
				)))))


(defun play-3-nim (heap-sizes q-table)
  "Returns a q-table after learning how to play nim"
   
  ;;make q table, ignore sean only do size 20.
  (let ((num-iterations 1))
  
    ;;loop 1 to iterations
	(dotimes (i num-iterations)
		;;set state to heap-size
		(let ((current-state (get-state-from-stacks heap-sizes heap-sizes)) (old-state 0))
			;;loop until game finished
			(setf current-state (get-state-from-stacks heap-sizes heap-sizes))
			(loop while (> current-state 0) do
				
			
				;;before taking an action, record current state for later use
				(setf old-state current-state)
				;;take an action (call max-action state)
				(setf my-action-taken   (max-action q-table current-state)  )
				(dprint my-action-taken "action taken:")
				
				;;randomly do something a bit different

				(print "doing action #")
				(print my-action-taken)	
				;; calculate current state (modify current-state)
				(setf *debug* t)
				(dprint current-state "current-state passing in")
				(dprint heap-sizes "heap-sizes passing in") 
				(setf *debug* nil)
				(setf current-state (calculate-new-state current-state my-action-taken heap-sizes))
				;;check to see if you've lost, if so then do a losing-update
				(if (<= current-state 0)
					
					(print "you win!");;losing update
					(progn
						;;else we continue on, let the "opponent" make a move
						;;take an action (call max-action state)
						(print "current stacks are below, how many do you want to take?")
						(print (calculate-3d-from-1d current-state heap-sizes))
						(setf opp-action-taken  (- (make-user-move-3-nim ) 1))
						(dprint opp-action-taken "action taken:")
				
						;; calculate current state (modify current-state)
						;;(setf current-state (- (- current-state opp-action-taken) 1))
						(setf current-state (calculate-new-state current-state opp-action-taken heap-sizes))
						(print (calculate-3d-from-1d current-state heap-sizes))
	
						;;check if the opponent lost, if so then do a winning-update
						(if (<= current-state 0)
							(progn   (print "you lose :[ ") );;winning update
							;;if not the game is still going, learn from future rewards!
							nil)
						;;else do a regular old update.
				)))))
				q-table))


 
(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-) NOTE: our assignment starts from state 0 to state n, where n is the number of sticks"
	(let ((action-list '()))  
	  (dotimes (i (num-states q-table))
		(setf max-action (max-action q-table i 1000000))
		;;(print max-action)	
		(if (> max-action 999999)
			(setf max-action '-)
			nil)
		(if  (<= (max-q q-table i) 0)
			(setf max-action '-)
			nil)
		(setf action-list (append action-list (list max-action))))
	  action-list))

 (defun best-actions-3-nim (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-) NOTE: our assignment starts from state 0 to state n, where n is the number of sticks"
	(let ((action-list '()))  
	  (dotimes (i (num-states q-table))
		(setf max-action (+ (max-action q-table i) 1))
		;;(print max-action)	
		(if (> max-action 999999)
			(setf max-action '-)
			nil)
		(if  (<= (max-q q-table i) 0)
			(setf max-action '-)
			nil)
		(setf action-list (append action-list (list (list i (list (calculate-3d-from-1d i '(3 3 3)))  max-action)))))
	;;	(setf action-list (append action-list (list  max-action))))
	
	   action-list)) 

;; example:
;; 
;;(setq *my-q-table* (learn-nim 22 0.1 #'basic-alpha 50000))
;;
;; to get the policy from this table:
;;
;;(best-actions *my-q-table*)
;;
;; to play a game of your brain versus this q-table:
;;
;;(play-nim *my-q-table* 22)   ;; need to provide the original heap size
;;
;; You might try changing to some other function than #'basic-alpha...

#|

|#
;;3d-> 1d conversion, print out the state given the heap sizes
(defun get-state-from-stacks (max-heap-sizes heap-sizes)
	(setf max-heap-sizes (copy-list max-heap-sizes))
	(incf (first max-heap-sizes))
	(incf (second max-heap-sizes))
	(incf (third max-heap-sizes))	
	(+ (first heap-sizes) (* (second heap-sizes) (first max-heap-sizes)) (* (third heap-sizes) (first max-heap-sizes) (second max-heap-sizes)))
)
;;1d->3d->1d (get in state, convert to heap sizes, apply action, return converted state number)
(defun calculate-new-state (current-state action max-heap-sizes)
	(let ((bin-number (floor action 3))  (old-3d-state '(0 0 0)) (new-1d-state 0) (new-3d-state '(0 0 0)))
		(setf old-3d-state (calculate-3d-from-1d current-state max-heap-sizes))
		(setf new-3d-state (copy-list old-3d-state))
		(if (> (nth bin-number  old-3d-state) 0)
			(progn
				(setf (nth bin-number new-3d-state) (- (nth bin-number old-3d-state) (+ (mod action 3) 1)))
				(if (< (nth bin-number new-3d-state) 0);; if you took to many i'll forgive you
					(setf (nth bin-number new-3d-state) 0)
					0;;doesnt really mean anyhing
				)
			(get-state-from-stacks max-heap-sizes new-3d-state))
			0;; does mean something, if you choose to try to empty a bin that has no blocks, you lose, i wont forgive that mistake.
	)))

;;returns the stacks given the state and max heap sizes
(defun calculate-3d-from-1d (current-state max-heap-sizes)
	(setf max-heap-sizes (copy-list max-heap-sizes))
	(incf (first max-heap-sizes))
	(incf (second max-heap-sizes))
	(incf (third max-heap-sizes))
	(let ((num-sticks 0) (new-3d-state '(0 0 0)))
		(setf num-sticks (floor current-state (* (third max-heap-sizes) (second max-heap-sizes))))
		(setf current-state (- current-state (* num-sticks (* (third max-heap-sizes) (second max-heap-sizes)))))
		(setf (third new-3d-state) num-sticks)
	
		(setf num-sticks (floor current-state (second max-heap-sizes)))
		(setf current-state (- current-state (* num-sticks (second max-heap-sizes))))
		(setf (second new-3d-state) num-sticks)
	
		(setf (first new-3d-state) current-state)
	
		(print (copy-tree new-3d-state)))	
	
)

(defun learn-3-nim (heap-sizes gamma alpha-func num-iterations)
  "Returns a q-table after learning how to play nim"
 
  ;;make q table, ignore sean only do size 20.
  (let ((q-table (make-q-table (+ (* (+ (first heap-sizes) 1) (+ (second heap-sizes) 1) (+ (third heap-sizes) 1))  1) 9)))
  
    ;;loop 1 to iterations
	(dotimes (i num-iterations)
		;;set state to heap-size
		(let ((current-state (get-state-from-stacks heap-sizes heap-sizes)) (old-state 0))
			;;loop until game finished
			(setf current-state (get-state-from-stacks heap-sizes heap-sizes))
			(loop while (> current-state 0) do
				(dprint current-state "current state is:")
				;;before taking an action, record current state for later use
				(setf old-state current-state)
				;;take an action (call max-action state)
				(setf my-action-taken  (max-action q-table current-state) )
				(dprint my-action-taken "action taken:")
				
				;;randomly do something a bit different
				(if  (=  (random 2) 0)
					;; do something other than optimal
					(if (= (random 2) 0);;we don't really need to split this into two conditions since we have a modulus, but whatever
						(setf my-action-taken (- my-action-taken (+ (random 4) 1)))
						(setf my-action-taken (+ my-action-taken (- (random 4) 1)))
					)nil)
				(setf my-action-taken (mod my-action-taken 9))
	
				;; calculate current state (modify current-state)
				;;(setf current-state (- (- current-state my-action-taken) 1))
				(dprint current-state "current-state passing in")
				(dprint heap-sizes "heap-sizes passing in") 
				(setf current-state (calculate-new-state current-state my-action-taken heap-sizes))
				;;check to see if you've lost, if so then do a losing-update
				(if (<= current-state 0)
					
					(setf q-table (q-learner q-table -1 old-state my-action-taken current-state gamma alpha-func i));;losing update
					(progn
						;;else we continue on, let the "opponent" make a move
						;;take an action (call max-action state)
						(setf opp-action-taken  (max-action q-table current-state) )
						(dprint opp-action-taken "action taken:")
						(if  (=  (random 2) 0)
							;; do something other than optimal
							(if (= (random 2) 0);;we don't really need to split this into two conditions since we have a modulus, but whatever
							(setf opp-action-taken (- opp-action-taken (+ (random 4) 1)))
							(setf opp-action-taken (+ opp-action-taken (- (random 4) 1)))
						)nil)
						(setf opp-action-taken (mod opp-action-taken 9))	
	
						;; calculate current state (modify current-state)
						;;(setf current-state (- (- current-state opp-action-taken) 1))
						(setf current-state (calculate-new-state current-state opp-action-taken heap-sizes))
						;;check if the opponent lost, if so then do a winning-update
						(if (<= current-state 0)
							(progn  (setf q-table (q-learner q-table 1 old-state my-action-taken current-state gamma alpha-func i)));;winning update
							;;if not the game is still going, learn from future rewards!
							(setf q-table (q-learner q-table 0 old-state my-action-taken current-state gamma alpha-func i));;future rewards update
						)
						;;else do a regular old update.
				)))))
				q-table))



(defun test-max-action ()
	(let ((qtable (make-q-table 2 2)))
		(setf (aref qtable 0 1) 1)
		(setf (aref qtable 1 0) 10)
		(print (max-action qtable 0))))

;;(setf *debug* nil)
(defun base-assignment ()

	(setf *q-table* (learn-nim 22 .1 #'basic-alpha 450))
	(print (best-actions *q-table*))
	(play-nim *q-table* 22))


(defun test-bin-taking ()

;;get-state-from-stacks (max-heap-sizes heap-sizes)
;;(defun calculate-3d-from-1d (current-state max-heap-sizes)
(let ((one-d-state 0) (three-d-state '(2 2 2)))
	(print "2 2 2 in one d")
	(print (setf one-d-state (get-state-from-stacks '(3 3 3) three-d-state)))
	(print "previous thing in 3d")
	(print (setf three-d-state (calculate-3d-from-1d one-d-state '(3 3 3))))
	(print "END")
	;;(defun calculate-new-state (current-state action max-heap-sizes)
	;;(dotimes (i 9)	
	;;	(print (calculate-3d-from-1d (calculate-new-state one-d-state i '(3 3 3)) '(3 3 3))) 
	;;)
	(print (get-state-from-stacks '(3 3 3) '(0 3 3)))
	(print (calculate-3d-from-1d 60 '(3 3 3)))
	(print (calculate-new-state 60 3 '(3 3 3)))
))
;;(base-assignment)
(setf *debug* nil)

(test-bin-taking)
(setf *q-table* (learn-3-nim '(3 3 3) .1 #'basic-alpha 90000))
(print *q-table*)
(print (best-actions-3-nim *q-table*))
(play-3-nim '(3 3 3) *q-table*)
;;(base-assignment)
