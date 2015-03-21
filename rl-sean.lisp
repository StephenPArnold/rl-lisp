;;; FUNCTION DICTIONARY
;;
;;
;;







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


(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called
to provide the current alpha value."

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

  ;;; IMPLEMENT ME
  )



(defun ask-if-user-goes-first ()
  "Returns true if the user wants to go first"
  (y-or-n-p "Do you want to play first?"))
  
(defun make-user-move ()
  "Returns the number of sticks the user wants to remove"
  (let ((result))
    (loop
     (format t "~%Take how many sticks?  ")
     (setf result (read))
     (when (and (numberp result) (<= result 3) (>= result 1))
       (return result))
     (format t "~%Answer must be between 1 and 3"))))

(defun play-nim (q-table heap-size)
  "Plays a game of nim.  Asks if the user wants to play first,
then has the user play back and forth with the game until one of
them wins.  Reports the winner."

  ;;; IMPLEMENT ME
  )

  
(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
  ;; hint: see optional value in max-action function

  ;;; IMPLEMENT ME
  )


  

;; example:
;; 
;; (setq *my-q-table* (learn-nim 22 0.1 #'basic-alpha 50000))
;;
;; to get the policy from this table:
;;
;; (best-actions *my-q-table*)
;;
;; to play a game of your brain versus this q-table:
;;
;; (play-nim *my-q-table* 22)   ;; need to provide the original heap size
;;
;; You might try changing to some other function than #'basic-alpha...

(make-q-table 30 3)
