;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; am doign it backward -- need to get ok values and cross off all not allowed by assignment


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (cond ((= n 0) (null delta))
        ((null delta) (fill-rest n))
        (t (let ((sat-pos (try-filter n delta)))
             (if (not (null sat-pos)) sat-pos
               (try-filter (* -1 n) delta))))))

; returns (1) a list of numbers {n, +/- [(abs n)-1, 1]) st all delta is satisfied or
;         (2) NIL if none exists
;
(defun try-filter (n delta)
  (let ((sat (sat? (- (abs n) 1) (filter n delta))))
    (cond ((= (abs n) 1) (if (null sat) NIL (list n)))
          ((= (length sat) (- (abs n) 1)) (cons n sat))
          (t NIL))))

; return a list of all integers [n, 1]
;
(defun fill-rest (n)
  (cond ((= n 0) NIL)
        (t (cons n (fill-rest (- (abs n) 1))))))


; returns the list of list DELTA with any sublists containing N having been removed
;
(defun filter (n delta)
  (if (null delta) NIL
    (let* ((curr-rule (first delta))
          (rest-rule (filter n (rest delta))))
        (cond ((invalid n curr-rule) NIL)
              ((member n curr-rule) rest-rule)
              (t (cons (first delta) rest-rule))))))


; returns true if the rule is invalid -- ie there are no numbers left that can make it true
;
(defun invalid (n rule)
  (cond ((null rule) NIL)
        ((>= (abs n) (abs (first rule))) NIL)
        (t (invalid n (rest rule)))))

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

