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
