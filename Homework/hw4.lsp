(defun initialize-fc (n) 
    (cond
        ; Base Case
        ; n = 0 -> return NIL
        ((= n 0) NIL)
        ; creates a list of n zeros. The zeros mean that
        ; an option hasn't been given a value of T or Nil yet.
        (T (append (list 0) (initialize-fc (- n 1))))
    )
)

(defun get-number (literal)
    ; this function is basically returns the absolute value of
    ; the literal
    (cond
        ; if less than 0, multiply literal by -1
        ((< literal 0) (* -1 literal))
        ; else return the literal
        (T literal)
    )
)   

(defun get-val-literal (cur-list n)
    (cond
        ; Base Case
        ; if n = 1 -> return the value of the first of cur-list
        ((= 1 n) (first cur-list))
        ; recursively goes through list until it reaches the correct number
        (T (get-val-literal (rest cur-list) (- n 1)))
    )
)

(defun is-literal-true (val-literal isNegated)
    ; this function checks to see if it is true given that the literal is
    ; either T or NIL (val-literal) and whether it isNegated. Also, if literal value
    ; is 0, then it hasn't been given a value, so return T
    (cond
        ; if val-literal and isNegated are equal (both T or NIL) or val-literal is 0, 
        ; return T
        ((or (not (equal val-literal isNegated)) (equal val-literal 0)) T)
        ; else return NIL
        (T NIL)
    )
)

(defun valid-clause (cur-list clause)
    (cond 
        ; Base Case
        ; if clause is NIL, return NIL, because a T
        ; wasn't found in the clause
        ((null clause) NIL)
        ; else check if the literal is T
        (T 
            (let*
                (
                    ; gets first in clause
                    (raw-literal (first clause))
                    ; get if it is Negated
                    (isNegated (< raw-literal 0))
                    ; get actual number (runs through get-number)
                    (num-literal (get-number raw-literal))
                    ; gest the value of the literal (whether it has been assigned)
                    (val-literal (get-val-literal cur-list num-literal))
                )
                (cond
                    ((is-literal-true val-literal isNegated) T)
                    (T (valid-clause cur-list (rest clause)))
                )
            )
        )
    )
)

(defun forward-checking (cur-list delta)
    (cond 
        ; Base cases
        ; if delta null -> return T
        ((null delta) T)
        ; checks if the first clause is true, and if so, forward checks on the rest of the clauses
        ((valid-clause cur-list (first delta)) (forward-checking cur-list (rest delta)))
        ; if false, returns NIL right away
        (T NIL)
    )
)

(defun initialize-order (n cur)
    ; this function creates a list from 1 to n - 1
    (cond 
        ; if cur equals n, return NIL
        ((= cur n) NIL)
        ; recursively creates the list
        (T (append (list cur) (initialize-order n (+ 1 cur))))
    )
)

(defun pretty-print (cur-list n)
    ; transforms list of T and NIL to actual numbers (positive and negative)
    (cond 
        ; base case -> return null if cur-list is NIL
        ((null cur-list) NIL)
        (T
            (let*
                (
                    ; get value of literal (T or NIL)
                    (cur-literal (first cur-list))
                    ; gets effect -1 if NIL 1 if T
                    (effect (cond
                        ((equal cur-literal NIL) -1)
                        (T 1)
                    ))
                )
                ; multiplies n with effect, then recursively calls for the rest of the list
                (append (list (* n effect)) (pretty-print (rest cur-list) (+ n 1)))
            )
        )
    )
)

(defun set-value (n v cur-list)
    ; sets value of n to v in list
    (let*
        (
            ; gets length of list
            (list-size (length cur-list))
            ; gets front half of list
            (front-list (butlast cur-list (- list-size n)))
            ; gets back half of list
            (back-list (nthcdr (+ n 1) cur-list))
        )
        ; creates new list with value wedged in between
        (append front-list (list v) back-list)
    )
)

(defun sat?-helper (order-list cur-list delta) 
    (print cur-list)
    (let*
        (
            ; gets whether the current list returns true for forward-checking
            (isComplete (forward-checking cur-list delta))
        )
        (cond
            (isComplete 
                    (cond 
                        ; if order-list is NIL, then there is nothing more to check, return the cur-list
                        ((null order-list) cur-list)
                        (T
                            ; recursively call sat-helper when setting the first value of order-list to T
                            (or (sat?-helper (rest order-list) (set-value (- (first order-list) 1)  T cur-list) delta)
                            ; recursively call sat-helper when setting the first value of order-list to NIL
                                (sat?-helper (rest order-list) (set-value (- (first order-list) 1)  NIL cur-list) delta))
                        )
                    )
            )
        )
    )    
)

(defun sat? (n delta)
    (let*
        (
            ; initialize list with 0s
            (cur-list (initialize-fc n))
            ; initialize list with numbers from 1 to n
            (order-list (initialize-order (+ n 1) 1))
            ; get result of sat?-helper
            (result (sat?-helper order-list cur-list delta))
        )
        (cond
            ; if result is T, pretty-print the result
            ((not (null result)) (pretty-print result 1))
            ; return NIL
            (T NIL)
        )
    )
)


