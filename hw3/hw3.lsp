;
; CS161 Hw3: Sokoban
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
;
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
;
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; BEGIN MY CODE ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; PERSONAL HELPER FUNCTIONS:

(defun set-pt (s pt v) (set-square s (first pt) (second pt) v))
(defun get-pt (s pt) (get-square s (first pt) (second pt)))

(defun count-board (seq board)
  (if (null board) 0
    (+ (count seq (first board)) (count-board seq (rest board)))))

(defun get-square (s r c)
  (cond ((or (<= (length s) r) (< r 0)) Wall)
        ((or (<= (length (first s)) c) (< c 0)) Wall)
        (t (first (nthcdr c (first (nthcdr r s)))))))

(defun insert (row index element)
  (if (< index 0) row
    (let ((prefix (butlast row (- (length row) index)))
          (suffix (nthcdr (+ index 1) row)))
      (append prefix (cons element suffix)))))

(defun set-square (s r c v)
  (if (not (= r 0)) (cons (first s) (set-square (rest s) (- r 1) c v))
    (cons (insert (first s) c v) (rest s))))

(defun add-point (pt1 pt2)
  (let ((r (+ (first pt1) (first pt2)))
        (c (+ (second  pt1) (second pt2))))
    (list r c)))

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game
; (neither any boxes nor the keeper is on a non-goal square)
;
(defun goal-test (s)
  (and (= (count-board box s) 0) (= (count-board keeper s) 0)))

; EXERCISE: Modify this function to return the list of
; successor states of s.
;

; defun empty: keep -> blank; keep* -> *
(defun empty (s space val)
  (cond ((isKeeper val) (set-pt s space Blank))
        ((isKeeperStar val) (set-pt s space Star))))

(defun step-to-blank (s dest src src-val)
  (empty (set-pt s dest Keeper) src src-val))

(defun step-to-star (s dest src src-val)
  (empty (set-pt s dest KeeperStar) src src-val))

(defun try-step (s dest dest-val src src-val)
   (cond ((or (< (first dest) 0) (>= (first dest) (length  s))) NIL)
         ((or (< (second dest) 0) (>= (second dest) (length (first s)))) NIL)
         ((isBlank dest-val) (step-to-blank s dest src src-val))
         ((isStar dest-val) (step-to-star s dest src src-val))
         (t NIL)))

(defun push-to (s dest dest-val)
  (cond ((isStar dest-val) (set-pt s dest BoxStar))
        ((isBlank dest-val) (set-pt s dest Box))))

(defun push-box (s dest mid src dest-val src-val)
  (step-to-blank (push-to s dest dest-val) mid src src-val))

(defun push-boxStar (s dest mid src dest-val src-val)
  (step-to-star (push-to s dest dest-val) mid src src-val))

(defun try-push (s dest mid src dest-val mid-val src-val)
  (cond ((or (< (first dest) 0) (>= (first dest) (length  s))) NIL)
        ((or (< (second dest) 0) (>= (second dest) (length (first s)))) NIL)
        ((not (or (isBlank dest-val) (isStar dest-val))) NIL)
        ((isBox mid-val) (push-box s dest mid src dest-val src-val))
        ((isBoxStar mid-val) (push-boxStar s dest mid src dest-val src-val))
        (t NIL)))

(defun try-move (s src d)
  (let* ((dest (add-point src d))
         (dest-val (get-pt s dest))
         (src-val (get-pt s src))
         (step-attmt (try-step s dest dest-val src src-val)))
    (if (not (null step-attmt)) step-attmt
         (let* ((final (add-point dest d))
                (final-val (get-pt s final))
                (push-attmt (try-push s final dest src final-val dest-val src-val)))
           (if (not (null push-attmt)) push-attmt
             NIL)))))

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (c (car pos))
	 (r (cadr pos))
	 (rc-pos (list r c)))
    (let ((move-up (try-move s rc-pos '(-1 0)))
          (move-right (try-move s rc-pos '(0 1)))
          (move-down (try-move s rc-pos '(1 0)))
          (move-left (try-move s rc-pos '(0 -1))))
      (cleanUpList (list move-up move-right move-down move-left)))))

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;
(defun h1 (s) (count-board box s))

; EXERCISE:  Then, modify this function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;


(defun MAX-STAR-DISTANCE (s)
  (let ((keeperPos (getKeeperPosition s 0))
        (stars (get-stars s)))
    (get-max-distance (list (second keeperPos) (first keeperPos)) stars)))

(defun MIN-STAR-DISTANCE (s)
  (let ((keeperPos (getKeeperPosition s 0))
        (stars (get-stars s)))
    (get-min-distance (list (second keeperPos) (first keeperPos)) stars)))

(defun SUM-STAR-DISTANCE (s)
  (let ((keeperPos (getKeeperPosition s 0))
        (stars (get-stars s)))
    (get-sum-distance (list (second keeperPos) (first keeperPos)) stars)))

(defun SUM-MIN-DISTANCE (s)
  (let ((boxes (get-boxes s))
        (stars (get-stars s)))
    (get-sum-min-distance boxes stars)))

(defun SUM-BOX-DISTANCE (s)
  (let ((keeperPos (getKeeperPosition s 0))
        (boxes (get-boxes s)))
    (get-sum-distance (list (second keeperPos) (first keeperPos)) boxes)))

(defun MIN-BOX-DISTANCE (s)
  (let ((keeperPos (getKeeperPosition s 0))
        (boxes (get-boxes s)))
    (get-min-distance (list (second keeperPos) (first keeperPos)) boxes)))

(defun MAX-BOX-DISTANCE (s)
  (let ((keeperPos (getKeeperPosition s 0))
        (boxes (get-boxes s)))
    (get-max-distance (list (second keeperPos) (first keeperPos)) boxes)))

(defun get-sum-distance (pt l &optional (sum 0))
  (if (null l) sum
    (get-sum-distance pt (rest l) (+ sum (distance pt (first l))))))

(defun get-max-distance (pt l &optional (acc 0))
  (if (null l) acc
    (let (m (max acc (distance pt (first l))))
      (get-max-distance pt (rest l) m))))

(defun get-min-distance (pt l &optional (acc -1))
  (cond ((null l) acc)
        ((= -1 acc) (get-min-distance pt (rest l) (distance pt (first l))))
        (t (get-min-distance pt (rest l) (min acc (distance pt (first l)))))))

(defun get-sum-min-distance (list1 list2 &optional(acc 0))
  (if (or (null list1) (null list2)) acc
    (get-sum-min-distance (rest list1) list2
                          (+ (get-min-distance (first list1) list2) acc))))

;;;;
(defun distance (pt1 pt2)
  (let ((dx (abs (- (first pt1) (first pt2))))
        (dy (abs (- (second pt1) (second pt2)))))
    (+ dx dy)))


(defun get-stars-in-row (row row-num &optional(col-num 0))
  (cond ((null row) NIL)
        ((not (isStar (first row))) (get-stars-in-row (rest row) row-num (+ col-num 1)))
        (t (cons (list row-num col-num)
                 (get-stars-in-row (rest row) row-num (+ col-num 1))))))

(defun get-stars (s &optional (row-num 0))
  (cond ((null s) NIL)
        (t (let ((this-stars (get-stars-in-row (first s) row-num))
                 (rest-stars (get-stars (rest s) (+ row-num 1))))
             (append this-stars rest-stars)))))

(defun get-boxes-in-row (row row-num &optional(col-num 0))
  (cond ((null row) NIL)
        ((not (isBox (first row))) (get-boxes-in-row (rest row) row-num (+ col-num 1)))
        (t (cons (list row-num col-num)
                 (get-boxes-in-row (rest row) row-num (+ col-num 1))))))

(defun get-boxes (s &optional (row-num 0))
  (cond ((null s) NIL)
        (t (let ((this-boxes (get-boxes-in-row (first s) row-num))
                 (rest-boxes (get-boxes (rest s) (+ row-num 1))))
             (append this-boxes rest-boxes)))))

; heuristic ideas

; match boxes+keeper with star (matching problem)
; add up distances
;
; keep pairs list and index-distance list
; first without pair is active
; first where distance < distance in list is new match
;       replace match with self and match in self list with -1


(defun first-unmatched (matched)
  (cond ((null matched) 0)
         ((= (first matched) 0) 0)
         (t (+ 1 (first-unmatched (rest matched))))))

(defun find-match (element list2 index-distance)
  (cond ((= (second (first index-distance)) -1) 0)
        ((< (distance element (first list2)) (second (first index-distance))) 0)
        (t (+ 1 (find-match element (rest list2) (rest index-distance ))))))

; currently -- sometimes not finding matches at all since not (-1 -1) replace?
(defun do-match-by-distance (list1 list2 matched index-dist)
    (let ((current-index (first-unmatched matched)))
      (if (= current-index (length list1)) index-dist
        (let* ((current-item (nth current-index list1))
               (match-index (find-match current-item list2 index-dist))
               (match-item (nth match-index list2))
               (match-pair (nth match-index index-dist))
               (remove-match (insert matched (first match-pair) 0))
               (new-matched (insert remove-match current-index 1))
               (delta (distance current-item match-item))
               (new-pairs (insert index-dist match-index (list current-index delta))))
          (do-match-by-distance list1 list2 new-matched new-pairs)))))

(defun sum-dist (l &optional (acc 0))
  (cond ((null l) acc)
        (t (sum-dist (rest l) (+ acc (second (first l)))))))

(defun initialize-list (l)
  (if (null l) NIL (cons 0 (initialize-list (rest l)))))

(defun initialize-pairs (l)
  (if (null l) NIL (cons '(-1 -1) (initialize-pairs (rest l)))))

(defun match-by-distance (list1 list2)
  (let ((matched (initialize-list list2))
        (index-dist (initialize-pairs list1)))
    (sum-dist (do-match-by-distance list1 list2 matched index-dist))))

(defun get-keeper-not-star (s)
  (let* ((c-r (getKeeperPosition s 0))
         (pos (list (second c-r) (first c-r))))
    (if (isKeeperStar (get-pt s pos)) NIL pos)))

(defun h304965058 (s)
  (let ((boxes (get-boxes s))
        (stars (get-stars s))
        (keeperPos (get-keeper-not-star s)))
    (if (and (null boxes) (null keeperPos)) 0
      (match-by-distance (cons keeperPos boxes) stars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 |
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 2 1 4 1)
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1)
	   (1 1 1 0 0 1 1 1 1)
	   (1 0 0 0 0 0 2 0 1)
s	   (1 0 1 0 0 1 2 0 1)
	   (1 0 4 4 4 1 3 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Utility functions for printing states and moves.
;

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
