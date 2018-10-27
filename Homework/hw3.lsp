;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 


(defun goal-test (s)
	(cond 
  		; Base Cases
		; if s is NIL, return T
		((null s) T)

		; checks if the count for 2 (Box) and 3 (Keeper)
		; is 0 for a row, recursively call on the rest
		; of the rows
		((not (and (= 0 (count 2 (first s))) (= 0 (count 3 (first s))))) NIL)
		(T (goal-test (rest s)))	
  	)
)

(defun get-row (S r)
	(cond 
		; if r is less than S, return NIL
		((< r 0) NIL)
		; Use nthcdr to get a list with the target row r
		; being the first element of the list, then call
		; first to grab it. Will return NIL if r is >= the
		; length of the number of rows.
		(T (first (nthcdr r S)))
	)
)

(defun get-square (S r c)
	(let*
		(
			(row (get-row S r))
			(row-length (length row))
		)
		(cond
			((or (null row) (< c 0) (>= c row-length)) 1)
			; Use nthcdr to get a list of elements of the row,
			; with the first element being the target item of the 
			; c element. First then grabs this element.
			(T (first (nthcdr c row)))
		)
	)
	
)

(defun set-square (S r c v)
	(let*
		(
			(column-len (length S))
			(row (get-row S r))
			(row-len (length row))
		)
		(cond
			; if row is NIL or c is out of bounds, return NIL
			((or (null row) (< c 0) (>= c row-len)) NIL)
			; If not, then the following makes a copy of the rows before and after the
			; row that will be modified, and creates a new row and appends it
			; in between both sides.
			(T (append (butlast S (- column-len r)) 
						(list (append (butlast row (- row-len c)) (list v) (nthcdr (+ c 1) row)))
						(nthcdr (+ r 1) S)
			))
		)
	)
)

;;; For the following move functions, they take a state (S), row number (r), 
;;; column number (c), and state that represents after the keeper moves to another square,
;;; but the keeper is not located in the other square.
;;; For all directions, the value of squares 1 and 2 movements in the direction the keeper
;;; is moving is attained. If the square 1 move away is a blank or a goal, then two 
;;; squares are set: the state where the keeper was standing (depending if it was a blank
;;; or a goal) and the square to represent the keeper. If the square 1 away is a box, 
;;; then the keeper pushes the box only if it square 2 moves away is a blank or a goal state.
;;; All other instances return NIL.

(defun move-up (S r c init_S)
		(let*
			(
				(square-1-up (get-square S (- r 1) c))
				(square-2-up (get-square S (- r 2) c))
				(square-keeper-1-up (set-square init_S (- r 1) c 3))
				(square-keeper-goal-1-up (set-square init_S (- r 1) c 6))
				
			)
			(cond
				((= square-1-up 0) square-keeper-1-up)
				((= square-1-up 4) square-keeper-goal-1-up)
				((= square-1-up 2) 
					(cond
						((= square-2-up 0) (set-square square-keeper-1-up (- r 2) c 2))
						((= square-2-up 4) (set-square square-keeper-1-up (- r 2) c 5))
						(T NIL)
					)
				)
				((= square-1-up 5) 
					(cond
						((= square-2-up 0) (set-square square-keeper-goal-1-up (- r 2) c 2))
						((= square-2-up 4) (set-square square-keeper-goal-1-up (- r 2) c 5))
						(T NIL)
					)
				)
				(T NIL)
			)
		)
)

(defun move-right (S r c init_S)
		(let*
			(
				(square-1-right (get-square S r (+ c 1)))
				(square-2-right (get-square S r (+ c 2)))
				(square-keeper-1-right (set-square init_S r (+ c 1) 3))
				(square-keeper-goal-1-right (set-square init_S r (+ c 1) 6))
				
			)
			(cond
				((= square-1-right 0) square-keeper-1-right)
				((= square-1-right 4) square-keeper-goal-1-right)
				((= square-1-right 2) 
					(cond
						((= square-2-right 0) (set-square square-keeper-1-right r (+ c 2) 2))
						((= square-2-right 4) (set-square square-keeper-1-right r (+ c 2) 5))
						(T NIL)
					)
				)
				((= square-1-right 5) 
					(cond
						((= square-2-right 0) (set-square square-keeper-goal-1-right r (+ c 2) 2))
						((= square-2-right 4) (set-square square-keeper-goal-1-right r (+ c 2) 5))
						(T NIL)
					)
				)
				(T NIL)
			)
		)
)

(defun move-down (S r c init_S)
		(let*
			(
				(square-1-down (get-square S (+ r 1) c))
				(square-2-down (get-square S (+ r 2) c))
				(square-keeper-1-down (set-square init_S (+ r 1) c 3))
				(square-keeper-goal-1-down (set-square init_S (+ r 1) c 6))
				
			)

			(cond
				((= square-1-down 0) square-keeper-1-down)
				((= square-1-down 4) square-keeper-goal-1-down)
				((= square-1-down 2) 
					(cond
						((= square-2-down 0) (set-square square-keeper-1-down (+ r 2) c 2))
						((= square-2-down 4) (set-square square-keeper-1-down (+ r 2) c 5))
						(T NIL)
					)
				)
				((= square-1-down 5) 
					(cond
						((= square-2-down 0) (set-square square-keeper-goal-1-down (+ r 2) c 2))
						((= square-2-down 4) (set-square square-keeper-goal-1-down (+ r 2) c 5))
						(T NIL)
					)
				)
				(T NIL)
			)
		)
)

(defun move-left (S r c init_S)
		(let*
			(
				(square-1-left (get-square S r (- c 1)))
				(square-2-left (get-square S r (- c 2)))
				(square-keeper-1-left (set-square init_S r (- c 1) 3))
				(square-keeper-goal-1-left (set-square init_S r (- c 1) 6))
				
			)
			(cond
				((= square-1-left 0) square-keeper-1-left)
				((= square-1-left 4) square-keeper-goal-1-left)
				((= square-1-left 2) 
					(cond
						((= square-2-left 0) (set-square square-keeper-1-left r (- c 2) 2))
						((= square-2-left 4) (set-square square-keeper-1-left r (- c 2) 5))
						(T NIL)
					)
				)
				((= square-1-left 5) 
					(cond
						((= square-2-left 0) (set-square square-keeper-goal-1-left r (- c 2) 2))
						((= square-2-left 4) (set-square square-keeper-goal-1-left r (- c 2) 5))
						(T NIL)
					)
				)
				(T NIL)
			)
		)
)

(defun try-move (S D)
	(let*
		(
			(pos (getKeeperPosition S 0))
			; represents the initial column
	 		(c (car pos))
			; represents initial row
	 		(r (cadr pos))
			; sets the square of the state that the keeper is in after it left.
			(init_S (set-square S r c 
				(cond
					((= (get-square S r c) 3) 0)
					(T 4)
				)))
		)
		; Direction (D) = 0 - up, 1 - right, 2 - down, 3 - left
		(cond
			((= D 0) (move-up S r c init_S))
			((= D 1) (move-right S r c init_S))
			((= D 2) (move-down S r c init_S))
			((= D 3) (move-left S r c init_S))
		)
	)
)

(defun next-states (s)
  ; Removes NILs from the created list after try-move is called in all directions
  (cleanUpList (append (list (try-move S 0)) (list (try-move S 1)) (list (try-move S 2)) (list (try-move S 3))))
)

(defun h0 (s)
; returns 0
  0
)

; h1 is admissable, as it will not overestimate. Everything out of place will 
; eventually have to move to another place. 
(defun h1 (s)
	(cond 
  		; Base Cases
		; if s is NIL, return 0
		((null s) 0)

		; adds the count for the number of boxes found in the row 
		; and recursively calls for the rest of the rows
		(T (+ (count 2 (first s)) (h1 (rest s))))
  	)
)

(defun find-occurences-row (s r c v)
	(let*
		(
			(row (get-row s r))
			(row-length (length row))
			(square (get-square s r c)) 
		)
		(cond
			; Base Case
			; return NIL when c is greater or equal to the length of the row
			((>= c row-length) NIL)
			; If the square equals the value, append the list of the coordinates
			; with the next square
			((= square v) (append (list (list r c)) (find-occurences-row s r (+ c 1) v)))
			; don't append anything and skip to the next square in the row
			(T (find-occurences-row s r (+ c 1) v))
		)
	)
)

(defun find-occurences (s r v)
	(cond
		; if there are no more rows to check, return NIL
		((>= r (length s)) NIL)
		; If the count of the value in the row is greater than 0, call find-occurences-row on that row and 
		; recursively call find-occurences on the rest of the rows
		((> (count v (get-row s r)) 0) (append (find-occurences-row s r 0 v) (find-occurences s (+ r 1) v)))
		; skip the row and call find-occurences on the next row
		(T (find-occurences s (+ r 1) v))
	)	
)

(defun get-manhattan-distance (box goal)
	; manhattan distance is the difference in
	; x and y coordinates from the box to the goal
	(let*
		(
			(box_r (first box))
			(box_c (second box))
			(goal_r (first goal))
			(goal_c (second goal))
			(diff_r (- box_r goal_r))
			(diff_c (- box_c goal_c))
			; used to get the absolute value if negative
			(real_r (cond 
									((< diff_r 0) (* diff_r -1))
									(T diff_r)
			))
			(real_c (cond 
				((< diff_c 0) (* diff_c -1))
				(T diff_c)
			))
		)
		(+ real_r real_c)
	)
)

(defun single-manhattan-distance (box_pos goal_list)
	(cond
		; if the length of the goal list is 1, return the manhattan distance of from that box to the goal
		((= 1 (length goal_list)) (get-manhattan-distance box_pos (first goal_list)))
		(T
			(let
				(
					; get the minimum distance from the rows already checked out
					(cur-min (single-manhattan-distance box_pos (rest goal_list)))
					; get the current manhattan distance
					(box-distance (get-manhattan-distance box_pos (first goal_list)))
				)

				(cond 
					((< box-distance cur-min) box-distance)
					(T cur-min)
				)
			)
		)
	)
)

(defun sum-manhattan-distance (box_list goal_list)
	(cond
		; return 0 if NIL
		((null box_list) 0)
		; sum up the individual manhattan distances from each box to each of the goals
		(T (+ (single-manhattan-distance (first box_list) goal_list) (sum-manhattan-distance (rest box_list) goal_list)))
	)
)

(defun h204445695 (s)
	(let
		(
			(box_list (find-occurences s 0 2))
			(goal_list (find-occurences s 0 4))
		)
		(sum-manhattan-distance box_list goal_list)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))



;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))


;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0  0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )
;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun