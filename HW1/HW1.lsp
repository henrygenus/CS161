;; 1. nummber N, ordered tree TREE -> N in TREE ? t : NIL
(defun TREE-CONTAINS (N TREE)
  (cond ((NULL TREE) NIL)
        ((OR (NOT (LISTP TREE)) (NULL (REST TREE))) (= N TREE))
        (t (cond ((= N (second TREE)) T)
                 ((< N (SECOND TREE)) (TREE-CONTAINS N (FIRST TREE)))
                 ((> N (SECOND TREE)) (TREE-CONTAINS N (THIRD TREE)))))))

;; 2. ordered tree TREE -> minimum value in TREE
(defun TREE-MIN (TREE)
  (if (NOT (LISTP TREE)) TREE
    (TREE-MIN (FIRST TREE))))


;; 3. ordered tree TREE -> TREE in prefix form
(defun TREE-ORDER (TREE)
  (if (NOT (LISTP TREE)) (LISTP TREE)
    (cons (FIRST (TREE-ORDER (SECOND TREE)))
          (APPEND (TREE-ORDER (FIRST TREE)) (TREE-ORDER (THIRD TREE))))))

;; 4. list L, non-negative integers START & LEN -> sublist of L from index START of length LEN
(defun SUB-LIST (L START LEN)
  (if (OR (= LEN 0) (NULL L)) NIL
    (if (> START 0) (SUB-LIST (REST L) (- START 1) LEN)
      (CONS (FIRST L) (SUB-LIST (REST L) START (- LEN 1))))))

;; 5. list L -> lists L1 & L2 | L1 + L2 = L && L1.length - L2.length < 2
(defun SPLIT-LIST (L)
  (let* ((LEN (LENGTH L))
         (SHORTER_LENGTH (if (ODDP LEN) (/ (- LEN 1) 2) (/ LEN 2)))
         (LONGER_LENGTH (- LEN SHORTER_LENGTH))
         (LEFT_SUBLIST (SUB-LIST L 0 LONGER_LENGTH))
         (RIGHT_SUBLIST (SUB-LIST L LONGER_LENGTH SHORTER_LENGTH)))
         (LIST LEFT_SUBLIST RIGHT_SUBLIST)))

;; 6. binary tree TREE -> height of TREE
(defun BTREE-HEIGHT (TREE)
  (if (OR (NULL TREE) (NOT (LISTP TREE))) 0
      (let* ((L_HEIGHT (if (NULL (REST TREE)) 0 (BTREE-HEIGHT (FIRST TREE))))
             (R_HEIGHT (if (NULL (REST TREE)) 0 (BTREE-HEIGHT (SECOND TREE)))))
             (if (> L_HEIGHT R_HEIGHT) (+ 1 L_HEIGHT) (+ 1 R_HEIGHT)))))


;; 7. non-empty list of leaves LEAVES -> left-biased binary tree
(defun LIST2BTREE (LEAVES)
  (if (NULL (SECOND LEAVES)) (FIRST LEAVES)
    (if (NULL (THIRD LEAVES)) LEAVES
      (let ((SUBTREES (SPLIT-LIST LEAVES)))
      (LIST (LIST2BTREE (FIRST SUBTREES)) (LIST2BTREE (SECOND SUBTREES)))))))

;; 8. binary tree TREE -> list of atoms
(defun BTREE2LIST (TREE))

;; 9. expressions E1 & E2 of atoms -> whether expressions are equal
(defun IS-SAME (E1 E2) NIL)
;; split all sublists into binary trees, then flatten into a list; compare by element
