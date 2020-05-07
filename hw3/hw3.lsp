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

; return the value at location ROW, COL in state S
;
(defun get-square (s r c)
  (cond ((or (<= (length s) r) (< r 0)) Wall)
        ((or (<= (length (first s)) c) (< c 0)) Wall)
        (t (nth c (nth r s)))))

; set the value at location ROW, COL in state S to V and return the new state
;
(defun set-square (s r c v &optional (prefix NIL))
  (cond ((null s) s)
        ((> r 0) (set-square (rest s) (- r 1) c v (append prefix (list (first s)))))
        (t (cleanUpList (append prefix (list (insert (first s) c v)) (rest s))))))

; use a point (row, col) rather than the two seperate parameters
; to call get-square and set-square
;
(defun set-pt (s pt v) (set-square s (first pt) (second pt) v))
(defun get-pt (s pt) (get-square s (first pt) (second pt)))

; given a sequence SEQ and board BOARD, return # of items in board and seq
;
(defun count-board (seq board &optional (sum 0))
  (if (null board) sum
    (+ (count-board seq (rest board)) (count seq (first board)))))

; insert ELEMENT at location INDEX inside of list ROW
;
(defun insert (row index element)
  (if (< index 0) row
    (let ((prefix (butlast row (- (length row) index)))
          (suffix (nthcdr (+ index 1) row)))
      (append prefix (cons element suffix)))))

; add the x and y values of two points
;
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

; given state S, space SPACE, and the value in the space VAL
; map: keeper -> blank; keeperStar -> star
;
(defun empty (s space val)
  (cond ((isKeeper val) (set-pt s space Blank))
        ((isKeeperStar val) (set-pt s space Star))))

; performs the setting necessary to step a keeper onto a blank space by
; setting space DEST to keeper & call EMPTY with S, SPACE, SRC-VAL
;
(defun step-to-blank (s dest src src-val)
  (empty (set-pt s dest Keeper) src src-val))

; performs the setting necessary to step a keeper onto a star space by
; setting space DEST to keeperStar & call EMPTY with S, SPACE, SRC-VAL
;
(defun step-to-star (s dest src src-val)
  (empty (set-pt s dest KeeperStar) src src-val))

; attempts to step piece of SRC to DEST by
; bound checking on the destination square DEST
; checking if DEST-VAL is blank, & calling STEP-TO-BLANK if true
; checking if DEST-VAL is a star, & calling STEP-TO-STAR if true
;
(defun try-step (s dest dest-val src src-val)
   (cond ((or (< (first dest) 0) (>= (first dest) (length  s))) NIL)
         ((or (< (second dest) 0) (>= (second dest) (length (first s)))) NIL)
         ((isBlank dest-val) (step-to-blank s dest src src-val))
         ((isStar dest-val) (step-to-star s dest src src-val))
         (t NIL)))

; performs setting necessary to place a box onto space DEST by mapping dest-val
; star -> boxStar | blank -> box
;
(defun push-to (s dest dest-val)
  (cond ((isStar dest-val) (set-pt s dest BoxStar))
        ((isBlank dest-val) (set-pt s dest Box))))

; does the three sets necessary for a keeper @ SRC to push a box at MID to DEST
; given the corresponding values DEST-VAL & SRC-VAL
;
(defun push-box (s dest mid src dest-val src-val)
  (step-to-blank (push-to s dest dest-val) mid src src-val))

; does the three sets necessary for a keeper @ SRC to push a boxStar at MID to DEST
; given the corresponding values DEST-VAL & SRC-VAL
;
(defun push-boxStar (s dest mid src dest-val src-val)
  (step-to-star (push-to s dest dest-val) mid src src-val))

; checks validity of the destination space & value then performs sets necessary
; to push a box or boxStar and step the keeper
;
(defun try-push (s dest mid src dest-val mid-val src-val)
  (cond ((or (< (first dest) 0) (>= (first dest) (length  s))) NIL)
        ((or (< (second dest) 0) (>= (second dest) (length (first s)))) NIL)
        ((not (or (isBlank dest-val) (isStar dest-val))) NIL)
        ((isBox mid-val) (push-box s dest mid src dest-val src-val))
        ((isBoxStar mid-val) (push-boxStar s dest mid src dest-val src-val))
        (t NIL)))

; attempts to push or step depending on the value of the space in the direction D
;
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

; attempts to move in each of the four directions, returning a list of the next state
; provided by moving in said direction
;
(defun next-states (s)
  (let* ((c-r-pos (getKeeperPosition s 0))
         (pos (list (second c-r-pos) (first c-r-pos))))
    (let ((move-up (try-move s pos '(-1 0)))
          (move-right (try-move s pos '(0 1)))
          (move-down (try-move s pos '(1 0)))
          (move-left (try-move s pos '(0 -1))))
      (cleanUpList (list move-up move-right move-down move-left)))))

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
; This is heuristic, since each box must move at least once to enter a star
;
(defun h1 (s) (count-board box s))

; EXERCISE:  Then, modify this function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

; given a point PT and list L of points, return the max distance between pt
; and any point in L
;
(defun get-min-distance (pt l &optional (acc 0))
  (cond ((or (null l) (null pt)) acc)
        ((= 0 acc) (get-min-distance pt (rest l) (distance pt (first l))))
        (t (get-min-distance pt (rest l) (min acc (distance pt (first l)))))))

; get the manhattan distance between the points PT1 & PT2
;

(defun distance (pt1 pt2)
  (let ((dx (abs (- (first pt1) (first pt2))))
        (dy (abs (- (second pt1) (second pt2)))))
    (+ dx dy)))

; return (ROW-NUM, COL-NUM) points for all the stars in row ROW
;
(defun get-stars-in-row (row row-num col-num &optional (stars NIL))
  (cond ((null row) stars)
        ((not (isStar (first row))) (get-stars-in-row (rest row) row-num (+ col-num 1) stars))
        (t (let ((stars-so-far (cons (list row-num col-num) stars)))
             (get-stars-in-row (rest row) row-num (+ col-num 1) stars-so-far)))))

; return (row, col) points for all the stars in the state
;
(defun get-stars (s row-num &optional (stars NIL))
  (cond ((null s) stars)
        (t (let ((this-stars (get-stars-in-row (first s) row-num 0)))
             (get-stars (rest s) (+ row-num 1) (append this-stars stars))))))

; return (ROW-NUM, COL-NUM) points for all the boxes in row ROW
;
(defun get-boxes-in-row (row row-num col-num &optional (boxes NIL))
  (cond ((null row) boxes)
        ((not (isBox (first row))) (get-boxes-in-row (rest row) row-num (+ col-num 1) boxes))
        (t (let ((boxes-so-far (cons (list row-num col-num) boxes)))
             (get-boxes-in-row (rest row) row-num (+ col-num 1) boxes-so-far)))))

; return (row, col) points for all the boxes in the state
;
(defun get-boxes (s row-num &optional (boxes NIL))
  (cond ((null s) boxes)
        (t (let ((this-boxes (get-boxes-in-row (first s) row-num 0)))
             (get-boxes (rest s) (+ row-num 1) (append this-boxes boxes))))))

; given a boolean match list, return the first false value in the list
;
(defun first-unmatched (matched &optional (index 0))
  (cond ((or (null matched) (= (first matched) 0)) index)
         (t (first-unmatched (rest matched) (+ index 1)))))

; given a point ELEMENT, list of points LIST2, and list INDEX-DISTANCE where
; the pair at index i represents (index of the item it is matched with, distance to pt)
; for the item with index i in LIST2
;
(defun find-match (priority index-distance)
  (let ((priority-pair (first priority)))
    (cond ((= (first (nth (first priority-pair) index-distance)) -1) priority-pair)
          ((< (second priority-pair) (second (nth (first priority-pair) index-distance))) priority-pair)
          (t (find-match (rest priority) index-distance)))))

; finds a solution to the stable matching problem given lists LIST1 & LIST2 where
; preference is represented by shorter distance with MATCHED & INDEX-DIST as above
;
(defun match-by-distance (matched priority index-dist)
    (let ((current-index (first-unmatched matched)))
      (if (= current-index (length matched)) index-dist
        (let* ((match-index-dist (find-match (nth current-index priority) index-dist))
               (match-index (first match-index-dist)))
          (let ((new-pairs (insert index-dist match-index (list current-index (second match-index-dist))))
                (new-matched (insert (insert matched (first (nth match-index index-dist)) 0) current-index 1)))
            (match-by-distance new-matched priority new-pairs))))))

; merge sort implementation which uses a comparison of the second element
;
(defun combine (list1 list2 &optional (suffix NIL))
  (cond ((and (null list1) (null list2)) (reverse suffix))
        ((null list1) (append (reverse suffix) list2))
        ((null list2) (append (reverse suffix) list1))
        ((< (second (first list1)) (second (first list2)))
         (combine (rest list1) list2 (cons (first list1) suffix)))
        (t (combine list1 (rest list2) (cons (first list2) suffix)))))

(defun merge-sort (l)
  (cond ((null l) NIL)
        ((= (length l) 1) l)
        (t (let* ((dist (length l))
                 (halfDist (floor dist 2)))
             (combine (merge-sort (butlast l halfDist)) (merge-sort (nthcdr (- dist halfDist) l)))))))

; initializes a list of (index, distance) pairs
;
(defun list-priorities (point l index &optional (prev NIL))
  (cond ((null l) prev)
        (t (let ((priorities-so-far (cons (list index (distance point (first l))) prev)))
         (list-priorities point (rest l) (+ index 1) priorities-so-far)))))

; initializes a bool list of length = (length L) to all false
;
(defun initialize-list (l &optional (new-list NIL))
  (if (null l) new-list (initialize-list (rest l) (cons 0 new-list))))

; intializes a list of points to (-1, -1) x list of length (length L)
; (we use (-1, -1) to represent unmatched points in the second list)
;
(defun initialize-pairs (l &optional (new-pairs NIL))
  (if (null l) new-pairs (initialize-pairs (rest l) (cons '(-1 -1) new-pairs))))

; find and sort list of (index, distance) pairs
;
(defun initialize-priority (point l)
  (merge-sort (list-priorities point l 0)))

; initializes a list of (index, distance) pairs from min to max distance for each element of list1
;
(defun initialize-priorities (list1 list2 &optional (prev NIL))
  (cond ((null list1) prev)
        (t (let ((previous-priorities (append prev (list (initialize-priority (first list1) list2)))))
             (initialize-priorities (rest list1) list2 previous-priorities)))))

; return the sum of the second portion of each element of a list of pairs L
;
(defun sum-dist (l &optional (acc 0))
  (cond ((null l) acc)
        ((= (second (first l)) -1) (sum-dist (rest l) acc))
        (t (sum-dist (rest l) (+ acc (second (first l)))))))

; calls the distance matching fcn on lists LIST1 & LIST2, return the sum
; of the distances between the pair matching (the minimal sum of the
; distance between items in LIST1 & LIST2)
;
(defun min-sum-distances (list1 list2)
  (if (null list1) 0
    (let ((matched (initialize-list list1))
          (index-dist (initialize-pairs list2))
          (priorities (initialize-priorities list1 list2)))
      (sum-dist (match-by-distance matched priorities index-dist)))))

; helper fcn for get-keeper-not-star
(defun get-keeper-in-row (row &optional (col 0))
  (cond ((or (null row) (isKeeper (first row))) col)
        ((isKeeperStar (first row)) -1)
        (t (get-keeper-in-row (rest row) (+ col 1)))))

; get the location of the keeper in state S, returning nil if keeper is keeperStar
; flips the (col, row) point provided by getKeeperPosition to (row, col)
;
(defun get-keeper-not-star (s &optional (row-num 0))
  (if (null s) NIL
    (let ((keeperCol (get-keeper-in-row (first s))))
      (cond ((= -1 keeperCol) NIL)
            ((= (length (first s)) keeperCol) (get-keeper-not-star (rest s)(+ 1 row-num)))
            (t (list row-num keeperCol))))))

; three stage heuristic based on the state
; 1. there are boxes and a keeper off of stars
;       return (*) minimal sum of distance between stable matching of stars & boxes
;       + minimal distance from keeper to nearest unmatched star
; 2. the keeper is not on a star but all boxes are
;       return the distance between the keeper and the closest star
; 3. keeper on star but not boxes -- return (*)
; 4. else return 0
;
 (defun h304965058 (s)
  (let ((stars (get-stars s 0))
        (boxes (get-boxes s 0))
        (keeperPos (get-keeper-not-star s)))
    (if (null boxes) (get-min-distance keeperPos stars)
      (let ((dBoxes (min-sum-distances boxes stars))
            (dKeeper (get-min-distance keeperPos boxes)))
        (+ dKeeper dBoxes)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; END MY CODE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	   (1 0 1 0 0 1 2 0 1)
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

;(51)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(41)
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

;(26)
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
