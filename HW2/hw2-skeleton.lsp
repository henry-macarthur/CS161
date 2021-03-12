;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code
;; (defun BFS (FRINGE)
;;     ...)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (if (equal S '(T T T T)) ;if the final state is reached return true
        T
        nil ; otherwise return nil
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (cond 
        ((equal A 'h) ;homer
            (cond 
                ((and (equal (third S) (fourth S)) (not (equal (second S) (third S)))) ; if the dog are on the same side and the baby not with them, we ok!
                (list (list (not (first S)) (second S) (third S) (fourth S))) ; make a list with every item, flip homer tho
                )

                (t nil)
                
            ) 
           

        )
        ((equal A 'b) ;moves the baby
            (cond 
                ((equal (car S) (second S)) (list (list (NOT (car S)) (NOT (second S)) (third S) (fourth S)))) ; if homer on side with baby, then we move them, flip them to other side
                (t nil)
            )

        )
        ((equal A 'd) ;dog, must be on the same time, and poison 
            (cond
                ((and (equal (third S) (car S)) (not (equal (second S) (fourth S)))) ; make sure homer on same side as dog, and the baby not with the poison
                    (list (list (not (car S)) (second S) (not (third S)) (fourth s)))
                )
                (t nil)
            )

        )
        ((equal A 'p)
            ; homer must be on side w the poison
            (cond
                ((and (equal (fourth S) (car S))  (not (equal (second S) (third S)))) ; and baby and dog not gonna be left alone
                    (list (list (not (car S)) (second S) (third S) (not (fourth S)))) 
                )
                (t nil)
            )

        )
        (t nil)
    
    )

)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)) ; try moving each one
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond 
        ((null STATES) nil) ; if states is nul -> nul
        ((equal (car STATES) S) t) ; if state on stack return true
        (t (ON-PATH S (cdr STATES))) ; else recurse further
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond
        ((null STATES) nil)
        ;; ((listp (DFS (car STATES) PATH)) (DFS (car STATES) PATH))
        ;; (t (MULT-DFS (cdr STATES) PATH))
        ((null (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH)) ; if the dfs return null, call dfs on the rest of nodes
        (t (DFS (car STATES) PATH)) ; return valid dfs path
    )
)


; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    ; need a if else if else here
    ; need first check the base case that we are at the final solution
    ; need to second check wether or not we have visited this state
    ; otherwise me need to compute dfs
    ;(print (append PATH (list S)))
    ;; (let (possible (SUCC-FN S))  
        (cond 
            ((FINAL-STATE S) (append PATH (list S))) ; check if we are the final state, if so we add the path to the list
            ((not (ON-PATH S PATH)) ; if item is not on the path, we keep exploring
                (MULT-DFS (SUCC-FN S) (append PATH (list S)))
            )
            (t nil) ; otherwise we return  nul
        )
    ;; )
)
