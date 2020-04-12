; simple search functions:

; BFS takes a single argument, a list representing a tree where a leaf is
; represented as an atom and a non-leaf is represented by a list of its
; child nodes.  If the list is empty, it returns an empty list.  It inspects
; the head; if it is an atom, append it to the front of a recursive call on
; the rest of the list.  If not, move it to the back of the list (appending
; it will unwrap it by one level, so it approaches the base case). By iterating
; as such, each level in the tree is represented by one pair of parentheses,
; so we traverse left-to-right by level
(defun bfs (L)
  (cond ((null L) NIL)
        ((atom (first L)) (append (list (first L)) (bfs (rest L))))
        (t (bfs (append (rest L) (first L))))))

; DFS functions much like BFS, except that an atom is appended to the front
; of the return value of the recursive call, and that when we move a sublist
; to the back of the list, we make a recursive call on that as well.  By
; doing so, we dive into the right first, and traverse depths right-to-left
(defun dfs (L)
    (cond ((null L) NIL)
        ((atom (first L)) (append (dfs (rest L)) (list (first L))))
        (t (append (dfs (rest L)) (dfs (first L))))))

; DLDFS takes two arguments: a list of the same format as above and a maximum
; depth to search is a depth-limited depth first search.  If the depth is zero,
; or if the list is empty, it returns an empty list. If the first item is an
; atom, it appends that to the front of a recursive call on the rest of the
; list.  Otherwise, it calls itself with a decremented depth and appends
; that to a recursive call on the rest of the list
(defun dldfs (L depth)
  (cond ((or (<= depth 0) (null L)) NIL)
        ((atom (first L)) (append (list (first L)) (dldfs (rest L) depth)))
        (t (append (dldfs (first L) (- depth 1)) (dldfs (rest L) depth)))))

; DFID takes two arguments: a list of the format above and a maximum depth
; to iterate toward when searching.  It is a depth first iterative deepening
; search that returns an empty list if the depth is zero, and otherwise
; appends a DLDFS call with the passed depth to the back of a recursive call
; with a decremented depth.
(defun dfid (L depth)
  (if (<= depth 0) NIL
    (append (dfid L (- depth 1)) (dldfs L depth) )))


; MISSIONARY AND CANNIBAL PROBLEM SOLUTION BELOW

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise. It does this by direct
; comparison to the hard-coded final state
(defun final-state (s)
  (equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.  It does this by computing the cannibals and
; missionaries that would be left on either side by the passed move, and then
; checking directly whether (a) there are enough of both group for the move
; (b) there will be enough missionaries to offset the cannibals on either side.
(defun next-state (s m c)
  (let* ((this-side-m (- (first s) m))
        (this-side-c (- (second s) c))
        (other-side-m (- 3 this-side-m))
        (other-side-c (- 3 this-side-c)))
    (cond ((or (< this-side-m 0) (< this-side-c 0)) NIL)
          ((and (> this-side-c this-side-m) (> this-side-m 0)) NIL)
          ((and (> other-side-c other-side-m) (> other-side-m 0)) NIL)
          (t (list (list other-side-m other-side-c (null (third s))))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.  It does this via hard-coded calls to next-state
(defun succ-fn (s)
  (append (next-state s 2 0) (next-state s 0 2) (next-state s 1 1)
          (next-state s 1 0) (next-state s 0 1)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.  It does this by comparing to the first state in
; STATES, then recursively calling on the rest of STATES
(defun on-path (s states)
  (cond ((null states) NIL)
         ((equal s (first states)) t)
         (t (on-path s (rest states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. It does this by first checking if there are no movea, in which case it
; returns NIL.  It calls the below DFS to expand and generate and search the
; state, and if this fails it calls itself on the rest of the list.
(defun mult-dfs (states path)
  (cond ((null states) NIL)
        ((on-path (first states) path) (mult-dfs (rest states) path))
        (t (let ((result (mc-dfs (first states) path)))
             (if (null result)
                 (mult-dfs (rest states) path)
               result)))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.  It expands and generates the state, then calls the above DFS
; on the states with the passed PATH.
(defun mc-dfs (s path)
  (if (final-state s) (cons s path) (mult-dfs (succ-fn s) (cons s path))))
