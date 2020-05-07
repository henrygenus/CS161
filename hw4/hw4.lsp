;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that
; represents a model of delta,
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
;
(defun sat? (n delta)
    (sat n NIL delta))

(defun sat (n assignment delta)
  (cond ((null delta) (fill-out n assignment))
        ((and (not (null assignment)) (= n (length assignment))) NIL)
        (t (let ((sorted (merge-sort delta)))
             (try-sat n assignment sorted)))))

; keep sorted list of numbers entered; start w/ singletons
; try each assignment in a rule, if all fail then nil
;
(defun try-sat (n assignment delta)
  (cond ((null (first delta)) NIL)
        ((let* ((val (first (first delta)))
                (try-val-filter (filter val delta)) ;returns NIL if all are satisfied
                (try-sat (sat n (insert val assignment) try-val-filter)))
           (if (null try-sat)
                (try-sat n assignment (cons (rest (first delta)) (rest delta)))
                try-sat)))))

; insert element n into the strictly decreasing sorted list (by absolute value)
;
(defun insert (n assignment &optional (parsed NIL))
  (cond ((null assignment) (reverse (cons n parsed)))
        ((< (abs (first assignment)) (abs n)) (append (reverse parsed) (cons n assignment)))
        (t (insert n (rest assignment) (cons (first assignment) parsed)))))

; fills in positive values for any number below n, inclusive
;
(defun fill-out (n pre-list &optional (post-list NIL))
  (cond ((= n 0) post-list)
        ((null pre-list) (fill-out (- n 1) pre-list (cons n post-list)))
        ((= (abs (first pre-list)) n) (fill-out (- n 1) (rest pre-list) (cons (first pre-list) post-list)))
        (t (fill-out (- n 1) pre-list (cons n post-list)))))

; returns list DELTA with any sublists containing N having been removed & with any ~N removed
;
(defun filter (n delta &optional (filtered NIL))
  (cond ((null delta) (reverse filtered))
        ((member n (first delta)) (filter n (rest delta) filtered))
        (t (let ((filtered-rule (filter-rule (* -1 n) (first delta))))
             (if (null filtered-rule) NIL
               (filter n (rest delta) (cons filtered-rule filtered)))))))

; filter the inverse of the assigned value from a passed rule
;
(defun filter-rule (n rule &optional (filtered NIL))
       (cond ((null rule) filtered)
             ((= n (first rule)) (if (null (rest rule)) filtered (append (rest rule) filtered)))
             (t (filter-rule n (rest rule) (cons (first rule) filtered)))))

; merge sort implementation which uses a comparison of the list length
;
(defun combine (list1 list2 &optional (suffix NIL))
  (cond ((and (null list1) (null list2)) (reverse suffix))
        ((null list1) (append (reverse suffix) list2))
        ((null list2) (append (reverse suffix) list1))
        ((< (length (first list1)) (length (first list2)))
         (combine (rest list1) list2 (cons (first list1) suffix)))
        (t (combine list1 (rest list2) (cons (first list2) suffix)))))

(defun merge-sort (l)
  (cond ((null l) NIL)
        ((= (length l) 1) l)
        (t (let* ((dist (length l))
                 (halfDist (floor dist 2)))
             (combine (merge-sort (butlast l halfDist)) (merge-sort (nthcdr (- dist halfDist) l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

(defun satisfied (assignment cnf)
  (cond ((null cnf) T)
        ((is-satisfied assignment (first cnf)) (satisfied assignment (rest cnf)))
        (t NIL)))

(defun is-satisfied (assignment rule)
  (cond ((null rule) T)
        ((is-member (first rule) assignment) T)
        (t (is-satisfied assignment (rest rule)))))

(defun is-member (item l)
  (cond ((null l) NIL)
        ((= item (first l)) T)
        (t (is-member item (rest l)))))
