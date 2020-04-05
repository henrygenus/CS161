;; root is becoming null at some point


;; nummber N, ordered tree TREE -> N in TREE ? t : NIL
(defun TREE-CONTAINS (N TREE)
  (cond ((null TREE) NIL)
        ((or (not (listp TREE)) (null (rest TREE))) (= N TREE))
        (t (cond ((= N (second TREE)) t)
                 ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
                 ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))))))

;; ordered tree TREE -> minimum value in TREE
(defun TREE-MIN (TREE)
  (if (not (listp TREE)) TREE
    (TREE-MIN (first TREE))))


;; ordered tree TREE -> TREE in prefix form
(defun TREE-ORDER (TREE)
  (if (not (listp TREE)) (list TREE)
    (cons (first (TREE-ORDER (second TREE)))
          (append (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))))

;; list L, non-negative integers START & LEN -> sublist of L from index START of length LEN
(defun SUB-LIST (L START LEN)
  (if (or (= LEN 0) (null L)) NIL
    (if (> START 0) (SUB-LIST (rest L) (- START 1) LEN)
      (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))))

;; list L -> lists L1 & L2 | L1 + L2 = L && L1.length - L2.length < 2
(defun SPLIT-LIST (L)
  (let* ((len (length L))
         (shorter_length (if (oddp len) (/ (- len 1) 2) (/ len 2)))
         (longer_length (- len shorter_length)))
    (list (SUB-LIST L 0 longer_length) (SUB-LIST L longer_length shorter_length))))

