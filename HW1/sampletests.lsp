(defun RUN-SAMPLE-TESTS ()
  (PRINT (TREE-CONTAINS 3 '((1 2 3) 7 8)))
  (PRINT (NOT (TREE-CONTAINS 4 '((1 2 3) 7 8))))

  (PRINT (EQUAL (TREE-MIN '((1 2 3) 7 8)) 1))

  (PRINT (EQUAL (TREE-ORDER 3) '(3)))
  (PRINT (EQUAL (TREE-ORDER '((1 2 3) 7 8)) '(7 2 1 3 8)))

  (PRINT (EQUAL (SUB-LIST '(a b c d) 0 3) '(A B C)))
  (PRINT (EQUAL (SUB-LIST '(a b c d) 3 1) '(D)))
  (PRINT (EQUAL (SUB-LIST '(a b c d) 2 0) NIL))

  (PRINT (EQUAL (SPLIT-LIST '(a b c d)) '((A B) (C D))))
  (PRINT (EQUAL (SPLIT-LIST '(A B C D E)) '((A B C) (D E))))
  (PRINT (EQUAL (SPLIT-LIST '(A B C D E F)) '((A B C) (D E F))))

  (PRINT (EQUAL (BTREE-HEIGHT 1) 0))
  (PRINT (EQUAL (BTREE-HEIGHT '(1 2)) 1))
  (PRINT (EQUAL (BTREE-HEIGHT '(1 (2 3))) 2))
  (PRINT (EQUAL (BTREE-HEIGHT '((1 2) (3 4))) 2))
  (PRINT (EQUAL (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3))
  (PRINT (EQUAL (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3))

  (PRINT (EQUAL (LIST2BTREE '(1)) 1))
  (PRINT (EQUAL (LIST2BTREE '(1 2)) '(1 2)))
  (PRINT (EQUAL (LIST2BTREE '(1 2 3)) '((1 2) 3)))
  (PRINT (EQUAL (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4))))
  (PRINT (EQUAL (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7))))
  (PRINT (EQUAL (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8)))))

  (PRINT (EQUAL (BTREE2LIST 1) '(1)))
  (PRINT (EQUAL (BTREE2LIST '(1 2)) '(1 2)))
  (PRINT (EQUAL (BTREE2LIST '((1 2) 3)) '(1 2 3)))
  (PRINT (EQUAL (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4)))
  (PRINT (EQUAL (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7)))
  (PRINT (EQUAL (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8)))

  (PRINT (EQUAL (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T))
  (PRINT (EQUAL (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) NIL)))
