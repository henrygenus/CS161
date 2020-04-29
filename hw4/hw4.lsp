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
        (t (try-sat n assignment (rest delta) (first delta)))))

; keep sorted list of numbers entered; start w/ singletons
; try each assignment in a rule, if all fail then nil
;
(defun try-sat (n assignment delta curr-rule)
    (let ((val (first curr-rule)))
      (cond ((null val) NIL)
            ((is-member assignment val) (sat n assignment delta))
            ((is-member assignment (* -1 val)) (try-sat n assignment delta (rest curr-rule)))
            (t (let* ((try-val-filter (filter val delta))
                      (try-sat (sat n (insert val assignment) try-val-filter)))
                 (if (not (null try-sat)) try-sat
                   (try-sat n assignment delta (rest curr-rule))))))))

; check if value is a member of the sorted list
;
(defun is-member (sorted-list value)
  (cond ((or (null sorted-list) (< (abs (first sorted-list)) (abs value))) NIL)
        ((= value (first sorted-list)) t)
        (t (is-member (rest sorted-list) value))))

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

; returns list DELTA with any sublists containing N having been removed
;
(defun filter (n delta &optional (filtered NIL))
  (if (null delta) (reverse filtered)
    (let ((curr-rule (first delta)))
      (cond ((member n curr-rule) (filter n (rest delta) filtered))
            (t (filter n (rest delta) (cons curr-rule filtered)))))))


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


