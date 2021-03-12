;; ;
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

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun find-boxes (input)
	(cond ((Null input) nil)
		((isBox (car input)) T)
		(t (find-boxes (cdr input)))
	)
) ; true if there is a box, false otherwise

; if we search through everything then we know there are no boxes
(defun goal-test (s)
  (cond ((null s) T)
	((find-boxes (car s)) nil) ; look for boxes in the current row
	(t (goal-test (cdr s))) ;;else, look for boxes in the rest of the grid
  )
);end defun

; check to make sure that we do not go out of bounds, return 1 if we go out of bounds
(defun get-row(S row)
	(cond ((Null S) 1)
	((equal row 0) (car S)) ; if we are at the desired row #, return 
	(t (get-row (cdr S) (- row 1))) ; recurese through the rest of the rows
	)
)

; this searches for our desired col #
(defun get-col(S col)
	(cond ((Null S) 1)
	((equal col 0) (car S)) ; when we get to the desired col return it
	(t (get-col (cdr S) (- col 1)))
	)
)

;search through an array and for each item, add 1 -> returns length of an array
(defun get-len(S)
	(cond 
		((Null S) 0)
		(t (+ 1 (get-len (cdr S))))
	)
)

;pass in the current state and desired row / column
;if we are out of bounds return a wall
;otherwise get the value of the square at row / col
(defun get-square(S r c) ; state, row, column
	(cond 
		((< r 0) 1)
		((< c 0) 1)
		((>= r (get-len S)) 1)
		((>= c (get-len (car S))) 1)
		(t (get-col (get-row S r) c))
	)
	
)

;pass in a state, row, col and desired value
;this function will search through the state to find the desired r /c
;change the value at the desired location, then return the new updated row /state
(defun set-row(S row col v)
	(cond ((NULL S) nil)
	((equal row 0) (cons (set-col (car S) col v) (cdr S)))
	(t (cons (car S) (set-row (cdr S) (- row 1) col v)))
	)
)
;search through the given row till we find the desired column
;when we find the desired column, update its value and return the new row
(defun set-col(S col v)
	(cond ((NULL S) nil)
		((equal col 0) (cons v (cdr S)))
		(t (cons (car S) (set-col (cdr S) (- col 1) v)))
	)
)

; calls set row with the passed in values
(defun set-square(S r c v)
	(set-row S r c v)
)

; return 4 if the place is a keeper star, 0 otherwise
; used when I am clearing the position of the keeper when he moves
(defun set-old(x)
	(cond
		((isKeeperStar x) 4)
		(t 0)
	)
)


;try move does as described in the spec:
;takes in a state a direction and a location
;for each direction, we try the move and if it produces a valid outcome we return the state that it would produce
;basically for each direction, we try each possible move
;grabs the value of the two tiles in the desired direction and checks their value to deterine what moves are ok
(defun try-move(S D x y)
	(cond 
		((equal 'UP D) (let* ((tile1 (get-square S (- y 1) x)) (tile2 (get-square S (- y 2) x)) (tile0 (get-square S y x) ))

			(cond 

				((isBox tile1)
					(cond 
						((isBlank tile2)
							(set-square (set-square (set-square S (- y 1) x keeper) (- y 2) x box) y x (set-old (get-square S y x)))
						)
						((isStar tile2)
							(set-square (set-square (set-square S (- y 1) x keeper) (- y 2) x boxstar) y x (set-old (get-square S y x)))
						)
					)
				)
				((isBlank tile1)
					(set-square (set-square S (- y 1) x keeper) y x (set-old (get-square S y x)))
					;(set-square S y x keeper)
				)
				((isStar tile1)
					(set-square (set-square S (- y 1) x keeperstar) y x (set-old (get-square S y x)))
				)
				((isBoxStar tile1)
					(cond 
						((isBlank tile2) (set-square (set-square (set-square S (- y 1) x keeperstar) (- y 2) x box) y x (set-old (get-square S y x))))
						((isStar tile2) (set-square (set-square (set-square S (- y 1) x keeperstar) (- y 2) x boxstar) y x (set-old (get-square S y x))))
					)
				)
			)
		)	
		)
		((equal 'DOWN D) (let* ((tile1 (get-square S (+ y 1) x)) (tile2 (get-square S (+ y 2) x)))
			(cond 
				;can be blank tile then we move
				;can be a box with no wall behind it, we move it
					;if its a goal, then update the goal accordingly
				;
				((isBox tile1)
					(cond 
						((isBlank tile2)
							(set-square (set-square (set-square S (+ y 1) x keeper) (+ y 2) x box) y x (set-old (get-square S y x)))
						)
						((isStar tile2)
							(set-square (set-square (set-square S (+ y 1) x keeper) (+ y 2) x boxstar) y x (set-old (get-square S y x)))
						)
					)
				)
				((isBlank tile1)
					(set-square (set-square S (+ y 1) x keeper) y x (set-old (get-square S y x)))
				)
				((isStar tile1)
					(set-square (set-square S (+ y 1) x keeperstar) y x (set-old (get-square S y x)))
				)
				((isBoxStar tile1)
					(cond 
						((isBlank tile2) (set-square (set-square (set-square S (+ y 1) x keeperstar) (+ y 2) x box) y x (set-old (get-square S y x))))
						((isStar tile2) (set-square (set-square (set-square S (+ y 1) x keeperstar) (+ y 2) x boxstar) y x (set-old (get-square S y x))))
					)
				)
			)
		)
		)
		((equal 'LEFT D) (let* ((tile1 (get-square S y (- x 1))) (tile2 (get-square S y (- x 2))))
			(cond 
				;can be blank tile then we move
				;can be a box with no wall behind it, we move it
					;if its a goal, then update the goal accordingly
				;
				((isBox tile1)
					(cond 
						((isBlank tile2)
							(set-square (set-square (set-square S y (- x 1) keeper) y (- x 2) box) y x (set-old (get-square S y x)))
						)
						((isStar tile2)
							(set-square (set-square (set-square S y (- x 1) keeper) y (- x 2) boxstar) y x (set-old (get-square S y x)))
						)
					)
				)
				((isBlank tile1)
					(set-square (set-square S y (- x 1) keeper) y x (set-old (get-square S y x)))
					;(set-square S y (- x 1) keeper)
				)
				((isStar tile1)
					(set-square (set-square S y (- x 1) keeperstar) y x (set-old (get-square S y x)))
				)
				((isBoxStar tile1)
					(cond 
						((isBlank tile2) (set-square (set-square (set-square S y (- x 1) keeperstar) y (- x 2) box) y x (set-old (get-square S y x))))
						((isStar tile2) (set-square (set-square (set-square S y (- x 1) keeperstar) y (- x 2) boxstar) y x (set-old (get-square S y x))))
					)
				)
			)
		)
		)
		((equal 'RIGHT D) (let* ((tile1 (get-square S y (+ x 1))) (tile2 (get-square S y (+ x 2))))
			(cond 
				;can be blank tile then we move
				;can be a box with no wall behind it, we move it
					;if its a goal, then update the goal accordingly
				;
				((isBox tile1)
					(cond 
						((isBlank tile2)
							(set-square (set-square (set-square S y (+ x 1) keeper) y (+ x 2) box) y x (set-old (get-square S y x)))
						)
						((isStar tile2)
							(set-square (set-square (set-square S y (+ x 1) keeper) y (+ x 2) boxstar) y x (set-old (get-square S y x)))
						)
					)
				)
				((isBlank tile1)
					(set-square (set-square S y (+ x 1) keeper) y x (set-old (get-square S y x)))
				)
				((isStar tile1)
					(set-square (set-square S y (+ x 1) keeperstar) y x (set-old (get-square S y x)))
				)
				((isBoxStar tile1)
					(cond 
						((isBlank tile2) (set-square (set-square (set-square S y (+ x 1) keeperstar) y (+ x 2) box) y x (set-old (get-square S y x))))
						((isStar tile2) (set-square (set-square (set-square S y (+ x 1) keeperstar) y (+ x 2) boxstar) y x (set-old (get-square S y x))))
					)
				)
			)
		)
		)
	)
	;attempt to move to character in each direction and we see what happens 
)
; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;next states find the position of the keeper
; get his x and y coords, then return a list of states produced by moving the keeper one tile in each direction
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))

	(result (list (try-move s 'UP x y) (try-move s 'LEFT x y) (try-move s 'RIGHT x y) (try-move s 'DOWN x y)))
	 	;; (result (list (try-move s 'UP x y)))
		 
	 )
    (cleanUpList result);end
   )

);


; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)
;checks the given row recursiveley and finds how many boxes are there that arent placed in goals
(defun inRow (s)
	(cond 
		((NULL s) 0)
		((equal (car s) 2) (+ 1 (inRow (cdr s))))
		(t (+ 0 (inRow (cdr s))))
	)
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; just look through this and find how many of these things are boxes not on a state
; search through each each row and call inRow on it to find how many misplaced boexes are in that row
; sum up all the misplaced boxes from each row and return
; this function is admissible. Admissible means that the heuristic returns less than the minimum amount of moves
; required to solve the current state. IF we count each box that is not on a goal, we know this is admissible as it 
; has nothing to do with distances. In reality, a player will have to move to each box and move that box to a goal
; since this heuristic only returns how many misplaced boxes there are, and has nothing to do with distance, it has to be admissible
; because the player will at least have to move to each of those boxes(our calculation covers this but not the distance he travels) 
; so we know our answer is less than the shortest path. 
(defun h1 (s)
	(cond 
		((NULL s) 0)
		(t (+ (h1 (cdr s)) (inRow (car S))))
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; want to pass in a box and find the closest goal!

;takes in a box and a list of stars
; recursiveley search through the stars list and compare it to each box
; calculate the manhattan distance for each and find the closest one
(defun find-min-distance(box stars)
	(cond
		((null stars) 10000) ; arbitrary number, cant return 0 or it would throw off calculations
		(t (min (calc-manhattan (second box) (car box) (first (first stars)) (second (first stars))) (find-min-distance box (cdr stars))))
	)
)

;this function takex in a list of boxes and stars
;this function is used to call find-min-distance with each box and the stars list
;so for each item in the boxes list, call find-min-distance on the box and the star list and 
;return the sum of all the min distances from the box to the closest stars
(defun mh-box-star(boxes stars)
	(cond
		((null boxes) 0)
        ((null stars) 0)
		(t (+ (mh-box-star (cdr boxes) stars) (find-min-distance (car boxes) stars)))
	)
)

;recursiveley searches through the given row column by column
; if the given row has a star in it, add the stars coordinates to a list and keep searching the row
; for more stars
; overall, the function searches through the given row and returns a list of all stars in the row
(defun find-star-row (s r c)
	(cond 
		((null s) '())
		((isStar (car s)) (cons (list r c) (find-star-row (cdr s) r (+ c 1))))
		(t (find-star-row (cdr s) r (+ c 1)))
		
	)
)
;pass in the given state
;we go through each row and call find-star-row on it to get a list of all stars
(defun list-star(s r)
	(cond 
		((Null s) nil)
		(t (append (find-star-row (car s) r 0) (list-star (cdr s) (+ r 1))))
	)
)

; we look through each column in the row, if it is a box, create a list of the coordinates, add all coordinates
; in the row to a list
(defun find-box-row (s r c)
	(cond 
		((null s) '())
		((isBox (car s)) (cons (list r c) (find-box-row (cdr s) r (+ c 1))))
		(t (find-box-row (cdr s) r (+ c 1)))
		
	)
)

;for each row in the state, call find-box-row on it to generate a list of all 
; boxes in the given state
(defun list-boxes(s r)
	(cond 
		((Null s) '())
		(t (append (find-box-row (car s) r 0) (list-boxes (cdr s) (+ r 1))))
	)
)

;pass in the coordinates of two items
;compute the manhattan distance the two coordinate pairs and return it
(defun calc-manhattan(x y r c)
	(let* ((dist (+ (abs (- x c)) (abs (- y r)))))
		(cond 
			((< dist 0) (* dist -1))
			(t dist)
		)
	)
)

;for the given row that is passed in, if it is a box calculate the 
;manhattan distance from the box to the player
;return the manhattan distance for the given box and add it up to all the 
;other manhattan distances in the row 
(defun explore-row(s x y r c)
	(cond 
		((null s) 0)
		((isBox (car s)) (+ (explore-row (cdr s) x y r (+ c 1)) (calc-manhattan x y r c)))
		(t (explore-row (cdr s) x y r (+ c 1)))
	)
)

;recurse through each row in the state
;for each row, find all the manhattan distances from they player to all the boxes and sum them up
;this will give us the total manhattan distance for a player to all boxes
(defun calcDist(s x y r)
	(cond
		((null s) 0)
		(t (+ (explore-row (car s) x y r 0) (calcDist (cdr s) x y (+ r 1))))
	)
)

;grab the coordinates of the keeper
;call calcDistance to get the manhattan distance from the player to all the boxes
(defun  manhattan(s)
	(let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	)
	(calcDist s x y 0)
   )
)
; basic function that checks to see if the tile is a wall
(defun check-isWall(S r c)
	(cond 
		((isWall (get-square S r c)) 1)
		(t 0)
	)
)

;check to see if the box is cornered in
;check to see if we have walls blocking the box meaning it is dead and can no longer be moved
(defun check-in-corner(S r c)
	(cond 
		((and (equal (check-isWall S (- r 1) c) 1) (equal (check-isWall S (- r 0) (+ c 1)) 1)) t)
		((and (equal (check-isWall S (- r 1) c) 1) (equal (check-isWall S (- r 0) (- c 1)) 1)) t)
		((and (equal (check-isWall S (+ r 1) c) 1) (equal (check-isWall S (- r 0) (- c 1)) 1)) t)
		((and (equal (check-isWall S (+ r 1) c) 1) (equal (check-isWall S (- r 0) (+ c 1)) 1)) t)
		(t nil)
	)
)
;this takes in the current state and a boxes coordinates
;checks the boxes position and compares it to all possible valid moves
;basically we check to make sure the box is in a valid position or if it is trapped
; if a box is trapped we know that a box cannot make it to the goal

(defun ok-spot(st s)
	(let* ((numAround (+ (check-isWall st (first s) (+ (second s) 1)) (+ (check-isWall st (first s) (- (second s) 1)) (+ (check-isWall st (- (first s) 1) (- (second s) 0)) (check-isWall st (+ (first s) 1) (- (second s) 0))))))
		
	)
		(cond 
			((check-in-corner st (first s) (second s)) nil)
			(t T)
		)	
	)
)

;get a list of all the boxes in the current state
;for each box, we check to see if it is in a valid state or if it is trapped
; and the player cannot move it
; return all the boxes that are trapped
(defun trappedBoxes(s boxes) ; find number of trapped boxes
	;(findDeadLockedBox s boxes 0)
	(let* ((len (get-len boxes)))
		(cond 
			((null boxes) 0)
			((ok-spot s (car boxes)) (+ (trappedBoxes s (cdr boxes)) 0))
			(t (+  (trappedBoxes s (cdr boxes)) 1))
	
		)
	)
) ; find how many trapped boxes


; find the length of the list that has the star coordiantes
(defun num-stars(s)
	(get-len (list-star s 0))
)

;gets the total number of trapped boxes in the current state
; if there are more boxes than goals, return a high number so it doesnt get called again
; if there are trapped boxes, also return a high number as the state is no longer valid
(defun checkTrapped(s boxes)
	;(print (num-stars s) (- (get-len boxes)  (trappedBoxes s boxes)))
	;(print (trappedBoxes s boxes))
	(cond 
		(( > (trappedBoxes s boxes) 0) 100)
	(t 0)
	)
)

;this heurisitic determines its value by summing the manhattan distance from the 
;player to each box, and each box to the closest goal 
;in addition, we find any deadlocked boxes and return a high number as we know that state is invalid
(defun h705096169 (s)

	(let* ((boxList (list-boxes s 0)) (pos (getKeeperPosition s 0)))
    ;(print (checkTrapped s boxList))
		(+ (+ (manhattan s) (mh-box-star boxList (list-star s 0))) (checkTrapped s boxList))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
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
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

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

 