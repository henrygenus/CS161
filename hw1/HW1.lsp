; Henry Genus
; 304965058
;
; SOLUTIONS:
; 1.
; BASE CASE: empty tree -> false | single element tree -> false | value = root -> true
; ALGORITHM: compare passed value to root; if less, recursively call on left, if more on right
;
; 2.
; BASE CASE: single value tree -> value
; ALGORITHM: recursive call on first element, since values are sorted increasingly
;
; 3.
; BASE CASE: single value TREE -> list TREE
; ALGORITHM: recursive call on each of the three elements; return (2nd 1st 3rd)
;
; 4.
; BASE CASE: length 0 or empty list -> NIL
; ALGORITHM: start = 0 ? return first::(recursive call on rest w/ len--) : recursive call on rest w/ start--
;
; 5.
; BASE CASE: list is NIL -> NIL | list is atom -> list
; ALGORITHM: return ((recursive call on left half) (recursive call on right half))
;
; 6.
; BASE CASE: empty or single element tree -> 0
; ALGORITHM: return 1 + (the greater of a recursive call on the left subtree and one on the right)
;
; 7.
; BASE CASE: single element list or null second element -> first element | two element LIST -> LIST
; ALGORITHM: call SPLIT-TREE and return ((recursive call on first) (recursive call on second))
;
; 8.
; BASE CASE: single element -> list of element
; ALGORITHM: return (append (recursive call on first) (recursive call on second))
;
; 9.
; BASE CASE: NIL lists -> T | first elements of different types -> false | single lists -> direct comparison
; ALGORITHM: return (AND (recursive call on first) (recursive call on rest))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; IMPLEMENTATIONS ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.
; INPUT: number N, ordered tree TREE
; OUTPUT: N in TREE ? t : NIL
(DEFUN TREE-CONTAINS (N TREE)
  (COND ((NULL TREE) NIL)
        ((OR (NOT (LISTP TREE)) (NULL (REST TREE))) (= N TREE))
        (T (COND ((= N (second TREE)) T)
                 ((< N (SECOND TREE)) (TREE-CONTAINS N (FIRST TREE)))
                 ((> N (SECOND TREE)) (TREE-CONTAINS N (THIRD TREE)))))))


; 2.
; INPUT: ordered tree TREE
; OUTPUT: minimum value in TREE
(DEFUN TREE-MIN (TREE)
  (IF (NOT (LISTP TREE)) TREE
    (TREE-MIN (FIRST TREE))))


; 3.
; INPUT: ordered tree TREE
; OUTPUT: TREE in prefix form
(defun TREE-ORDER (TREE)
  (IF (NOT (LISTP TREE)) (LIST TREE)
    (CONS (FIRST (TREE-ORDER (SECOND TREE)))
          (APPEND (TREE-ORDER (FIRST TREE)) (TREE-ORDER (THIRD TREE))))))


; 4.
; INPUT: list L, non-negative integers START & LEN
; OUTPUT: sublist of L from index START of length LEN
(DEFUN SUB-LIST (L START LEN)
  (if (OR (= LEN 0) (NULL L)) NIL
    (if (> START 0) (SUB-LIST (REST L) (- START 1) LEN)
      (CONS (FIRST L) (SUB-LIST (REST L) START (- LEN 1))))))


; 5.
; INPUT: list L
; OUTPUT: list (L1 L2) st (eq (concatonate L1 L2) L) && L1.length - L2.length < 2
(DEFUN SPLIT-LIST (L)
  (LET* ((LEN (LENGTH L))
         (SHORTER-LENGTH (IF (ODDP LEN) (/ (- LEN 1) 2) (/ LEN 2)))
         (LONGER-LENGTH (- LEN SHORTER-LENGTH))
         (LEFT-SUBLIST (SUB-LIST L 0 LONGER-LENGTH))
         (RIGHT-SUBLIST (SUB-LIST L LONGER-LENGTH SHORTER-LENGTH)))
        (LIST LEFT-SUBLIST RIGHT-SUBLIST)))


; 6.
; INPUT: binary tree TREE
; OUTPUT: height of TREE
(DEFUN BTREE-HEIGHT (TREE)
  (IF (OR (NULL TREE) (NOT (LISTP TREE))) 0
      (LET* ((L_HEIGHT (IF (NULL (REST TREE)) 0 (BTREE-HEIGHT (FIRST TREE))))
             (R_HEIGHT (IF (NULL (REST TREE)) 0 (BTREE-HEIGHT (SECOND TREE)))))
            (IF (> L_HEIGHT R_HEIGHT) (+ 1 L_HEIGHT) (+ 1 R_HEIGHT)))))


;; 7.
; INPUT: non-empty list LEAVES
; OUTPUT: left-biased binary tree formed from the elements of LEAVES
(DEFUN LIST2BTREE (LEAVES)
  (IF (NULL (SECOND LEAVES)) (FIRST LEAVES)
    (IF (NULL (THIRD LEAVES)) LEAVES
      (LET ((SUBTREES (SPLIT-LIST LEAVES)))
      (LIST (LIST2BTREE (FIRST SUBTREES)) (LIST2BTREE (SECOND SUBTREES)))))))


; 8.
; INPUT: binary tree TREE
; list of atoms within TREE
(DEFUN BTREE2LIST (TREE)
  (IF (NUMBERP TREE) (LIST TREE) (APPEND (BTREE2LIST (FIRST TREE)) (BTREE2LIST (SECOND TREE)))))


; 9.
; INPUT: expressions E1 & E2 of atoms
; OUTPUT: whether expressions are equal
(DEFUN IS-SAME (E1 E2)
  (COND ((AND (NULL E1) (NULL E2)) T)
        ((AND (ATOM E1) (ATOM E2)) (= E1 E2))
        ((AND (LISTP E1) (LISTP E2)) (AND (IS-SAME (FIRST E1) (FIRST E2)) (IS-SAME (REST E1) (REST E2))))
        (T NIL)))
