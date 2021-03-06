; 1. TREE-CONTAINS (N TREE)
; ====== Arguments ======
; N - integer 
; TREE - ordered tree
; ====== Return ======
; T if N appears in Tree, 
; NIL otherwise.
; =======================
(defun TREE-CONTAINS (N TREE) 
    (cond 

        ; Base Case:
        ; When TREE is a number
        ; TREE can then be checked if it is equal to N
        ((not (listp TREE)) (= N TREE))

        ; This line is executed if TREE is a list,
        ; and recursively calls TREE-CONTAINS for the first element (L ordered tree),
        ; second element (m), and third element (R ordered tree), all called
        ; in an or statement. If either is true, the function returns true.
        (T (or (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (second TREE)) (TREE-CONTAINS N (third TREE))))
    )
)

; 2. TREE-MIN (TREE)
; ====== Arguments ======
; TREE - ordered tree
; ====== Return ======
; Minimum number appearing in TREE
(defun TREE-MIN (TREE)
    (cond 

        ; Base Case:
        ; When TREE is a number, return the number
        ((not (listp TREE)) TREE)
        
        ; An ordered tree will have its minimum element at the beginning
        ; of the list. Therefore, continuously calling first on TREE
        ; until a number is found will return the minimum element.
        (T (TREE-MIN (first TREE)))
    )
)

; 3. TREE-ORDER (TREE)
; ====== Arguments ======
; TREE - ordered tree
; ====== Return ======
; Pre-ordered list of numbers appearing in TREE
(defun TREE-ORDER (TREE)
    (cond 
    
        ; Base Case:
        ; When TREE is a number, return the number as a list
        ((not (listp TREE)) (list TREE))

        ; This is reached when TREE is a list. A list is constructed with the second
        ; element in the list (m) and a list created by recursively calling TREE-ORDER
        ; on the first element (L ordered tree). This is then appeneded to the list
        ; obtained from recursively calling TREE-ORDER on the third element of the list
        ; (R ordered tree). 
        (T (append (cons (second TREE) (TREE-ORDER (first TREE))) (TREE-ORDER (third TREE))))
    )
)

; 4. SUB-LIST (L START LEN)
; ====== Arguments ======
; L - list of atoms
; START - integer
; LEN - integer
; ====== Return ======
; sub-list of L starting at START and having length LEN
(defun SUB-LIST (L START LEN) 
    (cond 
        ; Base Case: 
        ; when LEN is 0, this means no more
        ; elements need to be grabbed. return Nil.
        ((= LEN 0) NIL)

         ; if start doesn't equal 0, decrement it by 1.
        ((> START 0) (SUB-LIST (rest L) (- START 1) LEN))

        ; if START is 0, it means that the sub-list can start being created
        ; use const to start create list with first element of L, and recursively 
        ; calling SUB-LIST on the rest of the list. Decrement LEN by 1.
        ((= 0 START) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))

    )
)

; 5. SPLIT-LIST (L)
; ====== Arguments ======
; L - list
; ====== Return ======
; list of two lists (L1 and L2) that satisfies the following criteria
; - L is the result of appending L1 and L2;
; - Length of L1 minus length of L2 is 0 or 1.
(defun SPLIT-LIST (L) 
    (let 
        (
        ; defining variable for length of L
        (list-len (length L))
        )
        (cond 

            ; if the length of the list is even, then each list will
            ; be half of the list. SUB-LIST can then be called to
            ; generate L1 by starting at 0 and getting half of the list,
            ; and L2 can be generated by starting at the index of the
            ; half of the list, and getting the second half of the list.

            ((evenp list-len) 
            (let
                (
                (half-len (/ list-len 2))
                )
                (list (SUB-LIST L 0 half-len) (SUB-LIST L half-len half-len))
            ))

            ; if the length of the list is odd, then the first list
            ; will have a length of (len + 1)/ 2 and the second list
            ; will have a length of one minus the first list. Similar
            ; calls to SUB-LIST are made to satisfy this criteria.

            (T 
            (let
                (
                (half-len (/ (+ list-len 1) 2))
                )
                (list (SUB-LIST L 0 half-len) (SUB-LIST L half-len (- half-len 1))
            ))
            )
        )
    )
)

; 6. BTREE-HEIGHT (TREE)
; ====== Arguments ======
; Tree - binary tree
; ====== Return ======
; Returns the height of the tree
(defun  BTREE-HEIGHT (TREE) 

    (cond 

        ; Base Case:
        ; When TREE is a leaf
        ((not (listp TREE)) 0)
        
        ; When L is a list
        (T 
            (let 
                (
                    (left-side (+ (BTREE-HEIGHT (first TREE)) 1))
                    (right-side (+ (BTREE-HEIGHT (second TREE)) 1))
                )
                (cond 
                    ((> left-side right-side) left-side)
                    (T right-side)
                )
            )   
        )
    )
)

; 7. LIST2BTREE (LEAVES)
; ====== Arguments ======
; LEAVES - non-empty list of atoms
; ====== Return ======
; Binary Tree
(defun  LIST2BTREE (LEAVES) 
    (cond
        ; Base Case:
        ; When there is one element in LEAVES, return that element
        ((= 1 (length LEAVES)) (first LEAVES))

        ; if length of list is greater than 1, call the the split-list function
        ; and call LIST2BTREE on the left and right lists that were created to
        ; recursively create the tree. Create a list from each level called.
        (T 
            (let 
                (
                    (split-list (SPLIT-LIST LEAVES))
                )
                (list (LIST2BTREE (first split-list)) (LIST2BTREE (second split-list)))
            )   
        )
    )
)

; 8. BTREE2LIST (TREE)
; ====== Arguments ======
; TREE - binary tree
; ====== Return ======
; list of atoms
(defun  BTREE2LIST (TREE)
    (cond 
        ; Base Case:
        ; When L is not a list
        ((not (listp TREE)) (list TREE))

        ; if L is a list, recursively call BTREE2LIST
        ; on the left and right side of the function and
        ; append them together
        (T (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
    )
)

; Didn't have enough time to finish :( 
; 9. IS-SAME (E1 E2)
; ====== Arguments ======
; E1 - expression 1
; E2 - expression 2
; ====== Return ======
; T if they are they same, NIL if different
(defun  IS-SAME (E1 E2)

)
