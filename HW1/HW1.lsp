;; root is becoming null at some point


;; arguments: nummber N, ordered tree TREE -> out: N in TREE ? t : NIL
(defun TREE-CONTAINS (N TREE)
  (cond ((null TREE) NIL)
        ((or (not (listp TREE)) (null (rest TREE))) (= N TREE))
        (t (cond ((= N (second TREE)) t)
                 ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
                 ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))))))

;; arguments: ordered tree TREE -> out: smallest value in the ordered tree
(defun TREE-MIN (TREE)
  (if (not (listp TREE)) TREE
         (TREE-MIN (first TREE))))


(defun TREE-ORDER (TREE)
  
