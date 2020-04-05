;; 1. nummber N, ordered tree TREE -> N in TREE ? t : NIL
(defun TREE-CONTAINS (N TREE)
  (cond ((null TREE) NIL)
        ((or (not (listp TREE)) (null (rest TREE))) (= N TREE))
        (t (cond ((= N (second TREE)) t)
                 ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
                 ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))))))

;; 2. ordered tree TREE -> minimum value in TREE
(defun TREE-MIN (TREE)
  (if (not (listp TREE)) TREE
    (TREE-MIN (first TREE))))


;; 3. ordered tree TREE -> TREE in prefix form
(defun TREE-ORDER (TREE)
  (if (not (listp TREE)) (list TREE)
    (cons (first (TREE-ORDER (second TREE)))
          (append (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))))

;; 4. list L, non-negative integers START & LEN -> sublist of L from index START of length LEN
(defun SUB-LIST (L START LEN)
  (if (or (= LEN 0) (null L)) NIL
    (if (> START 0) (SUB-LIST (rest L) (- START 1) LEN)
      (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))))

;; 5. list L -> lists L1 & L2 | L1 + L2 = L && L1.length - L2.length < 2
(defun SPLIT-LIST (L)
  (let* ((len (length L))
         (shorter_length (if (oddp len) (/ (- len 1) 2) (/ len 2)))
         (longer_length (- len shorter_length))
         (left_sublist (SUB-LIST L 0 longer_length))
         (right_sublist (SUB-LIST L longer_length shorter_length)))
         (list left_sublist right_sublist)))

;; 6. binary tree TREE -> height of TREE
(defun BTREE-HEIGHT (TREE)
  (if (or (null TREE) (not (listp TREE))) 0
      (let* ((L_height (if (null (rest TREE)) 0 (BTREE-HEIGHT (first TREE))))
             (R_height (if (null (rest TREE)) 0 (BTREE-HEIGHT (second TREE)))))
             (if (> L_height R_height) (+ 1 L-height) (+ 1 R_height)))))


;; 7. non-empty list of leaves LEAVES -> left-biased binary tree
(defun LIST2BTREE (LEAVES)
  (if (null (second LEAVES)) (first LEAVES)
    (if (null (third LEAVES)) LEAVES
      (let ((SUBTREES (SPLIT-LIST LEAVES)))
      (list (LIST2BTREE (first subtrees)) (LIST2BTREE (second subtrees)))))))

;; 8. binary tree TREE -> list of atoms
(defun BTREE2LIST (TREE))

;; 9. expressions E1 & E2 of atoms -> whether expressions are equal
(defun IS-SAME (E1 E2) NIL)
;; split all sublists into binary trees, then flatten into a list; compare by element
