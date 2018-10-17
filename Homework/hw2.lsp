; 1. DFS (TREE)
; ====== Arguments ======
; TREE - list representation of tree
; ====== Return ======
; List of terminal nodes 
; =======================
(defun DFS (TREE)
    (cond 
    ; base cases
    ; when tree is null
    ((null TREE) NIL)
    ; when tree is an atom
    ((not (listp TREE)) (list TREE))

    ; recursively appends calling DFS on the rest of the
    ; list to running DFS on the first element of the list
    (T (append (DFS (rest TREE)) (DFS (first TREE))))
    )
)

; 2. 

; DLS (TREE DEPTH)
; ====== Arguments ======
; TREE - list representation of tree
; DEPTH - Depth of DFS
; ====== Return ======
; List of DFS with given depth
; =======================
(defun DLS (TREE DEPTH)
    (cond 
        ; base cases
        ; when DEPTH is less than 0 or TREE is null
        ((or (null TREE) (< DEPTH 0)) NIL)
        ; When TREE is an atom, create a list element of that atom
        ((not (listp TREE)) (list TREE))

        ; Recursively run DLS on both the first and rest elements of the tree,
        ; appending the call of the first element to the call for the rest of the elements
        ; DEPTH isn't decremented for the call to the rest of the tree because the call isn't
        ; grabbing a single list, but getting the rest of the element
        (T (append (DLS (rest TREE) DEPTH) (DLS (first TREE) (- DEPTH 1))))
    )
)

; DFID (TREE LEVEL)
; ====== Arguments ======
; TREE - list representation of tree
; DEPTH - Maximum Depth of Tree
; ====== Return ======
; List in the order nodes were visited
; =======================
(defun DFID (TREE DEPTH) 
    (cond 
        ; base case
        ; if depth is 0 or TREE is null, return NIL
        ((or (null TREE) (= 0 DEPTH)) NIL)

        ; Recursively append the result of calling DFID
        ; and decrementing DEPTH by 1 and running DLS with
        ; that depth
        (T (append (DFID TREE (- DEPTH 1)) (DLS TREE DEPTH)))
    )
)

; 3.
; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
    (cond 
        ; return true if s equals the goal state
        ((equal s '(3 3 NIL)) T)
        
        ; Return Nil if s does not equal goal state 
        (T NIL)
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
    (let*
        (
            (before-cur-side-m (first s))
            (before-cur-side-c (second s))
            ; get the m and c from the opposite side by subtracting
            ; 3 to the ones on the current side
            (before-opp-side-m (- 3 before-cur-side-m))
            (before-opp-side-c (- 3 before-cur-side-c))

            ; following variables are created after m and c 
            ; are either subracted (from the currents side)
            ; or added (from the receiving side)
            (after-cur-side-m (- before-cur-side-m m))
            (after-cur-side-c (- before-cur-side-c c))
            (after-opp-side-m (+ before-opp-side-m m))
            (after-opp-side-c (+ before-opp-side-c c))
            (side (third s))
        )
            (cond 

                ; Returns NIL with the criteria given from the problem
                ((or (> m before-cur-side-m)
                    (> c before-cur-side-c)
                    (and (> after-cur-side-c after-cur-side-m) (not (= after-cur-side-m 0)))
                    (and (> after-opp-side-c after-opp-side-m) (not (= after-opp-side-m 0)))
                ) NIL)
                ; Returns a list of the list constructed from the m and c
                ; from the opposite side, and negating the side
                (T (list (list after-opp-side-m after-opp-side-c (not side))))
            )
    )
)
       
; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
    ; append a list with all of the possible states
    (append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 0 2) (next-state s 2 0))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
    (cond 
        ; base cases 
        ; when states is null
        ((null states) NIL)
        ; when states is an atom
        ((not (listp states)) NIL)

        ; returns true if s is equal to the first list element in states
        ((equal s (first states)) T)

        ; else recursively calls with the remaining elements
        (T (on-path s (rest states)))
    )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
    (cond 
        ; base case
        ; when states is null
        ((null states) NIL)

        ; calls mc-dfs on the first element in states with the current path,
        ; as well as calls mult-dfs again for the remaining states, and ors both
        ; of these calls together.
        (T (or (mc-dfs (first states) path) (mult-dfs (rest states) path)))
    )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
    (cond
        ; base cases
        ; if s is repeated in path, returns NIL
        ((on-path s path) NIL)
        ; if is is the final state, the path is returned with s (final state)
        ; appended at the end
        ((final-state s) (append path (list s)))

        ; Calls mult-dfs with the states returned from succ-fn and
        ; a path with s appended at the end
        (T (mult-dfs (succ-fn s) (append path (list s))))
    )
)
