(IN-PACKAGE :puzzle)

;Heuristics
(DEFUN default-heuristic(node)
  "Method to calculate a heuristic h(x) = o(x) - c(x); o(x) pieces to capture from the beginning of the game; c(x) pieces captured, this is the same as only searching for the current
  pieces on the node"
  (get-pieces node)
)

(DEFUN get-heuristics()
  "Method to return the list of heuristics"
  (LIST 'default-heuristic)
)
;Heuristics

;Node related methods
(DEFUN new-node(state &optional (parent NIL) (heuristic 0))
  "Method to create a new node with the given params"
  (LIST
    state
    (IF (NULL parent) 0 (1+ (get-depth parent)))
    heuristic
    parent
  )
)

(DEFUN test()
  "Returns a test node"
  (LIST
    (LIST (LIST 8 8 8 8 8 8) (LIST 8 8 8 8 8 8)) ;State
    0 ;Depth
    0 ;Heuristic
    NIL ;Parent
  )
)

(DEFUN get-state(node)
  "Returns the state of the given node"
  (NTH 0 node)
)

(DEFUN get-depth(node)
  "Returns the depth value of the given node"
  (NTH 1 node)
)

(DEFUN get-heuristic(node)
  "Returns the heuristic value of the given node"
  (NTH 2 node)
)

(DEFUN get-parent(node)
  "Returns the depth value of the given node"
  (NTH 3 node)
)

(DEFUN get-cost(node)
  "Returns the cost (depth + heuristic) value of the given node"
  (+ (get-depth node) (get-heuristic node))
)

(DEFUN get-cell(node row column)
  "Method to get a cell value from a node"
  (get-cell-from-state (get-state node) row column)
)

(DEFUN get-pieces(node)
  (LET* (
      (node-state (get-state node)) ;Get the state
      (top-row (NTH 0 node-state)) ;Split state and get top row
      (bot-row (NTH 1 node-state)) ;Split state and get bottom row
    )
    (+ (APPLY #'+ top-row) (APPLY #'+ bot-row)) ;Sums both rows    
  )
)

(DEFUN is-solution(node)
  "Verifies is the given nodes is have the final state"
  (ZEROP (get-pieces node)) ;Verifies if the node has 0 pieces total
)
;Node related methods



;Node state related methods
(DEFUN get-cell-from-state(node-state row column)
  "Method to get a cell value from a node-state"
  (NTH column (NTH row node-state))
)
;Node state related methods



;;Dependencies exporter
(DEFUN node-dependencies()
  "Method to export the needed method that the algorithms might use"
  (LIST
    'get-state
    'get-cost
    'is-solution
    'nodes-spawner
  )
)
;;Dependencies exporter



;Operation aux
(DEFUN swap-in-row(row column &optional (value 0))
  "Method to swap the given column on the row(list) and set the number on that given column"
  (IF (<= column 0)
      (CONS value (CDR row))
      (CONS (CAR row) (swap-in-row (CDR row) (1- column) value)) ;Goes to next iteration
  )
)

(DEFUN swap(node-state row column &optional (value 0))
  "Method to swap on the given row and column the value that is there for a given value"
  (COND
    ((OR (>= row 2) (>= column 6)) NIL) ;Dont execute on invalid cell
    ((OR (< row 0) (< column 0)) NIL) ;Dont execute on invalid cell
    (T
      (IF (<= row 0) ;Searched for the row to do the swap
        (CONS (swap-in-row (FIRST node-state) column value) (CDR node-state)) ;This is the row to swap the value in
        (CONS (CAR node-state) (swap (CDR node-state) (1- row) column value)) ;Goes to next iteration
      )
    )
  )
)

(DEFUN increment(node-state row column &aux (cell-value (get-cell-from-state node-state row column)))
  "Method to increment by one the value on the given cell"
  (swap node-state row column (1+ cell-value))  
)

(DEFUN get-destribution(node-state row column &optional (amount (get-cell-from-state node-state row column)) (initial-row row) (initial-column column))
  "Method to calculate the distribution list from a state and cell"
  (COND
    ((= 0 amount) NIL)
    ((= row 1)
      (COND
        ((AND (= row initial-row) (= column initial-column)) ;Verifies if is the start cell
          (APPEND NIL (get-destribution node-state row (1+ column) amount initial-row initial-column))
        )
        ((>= column 6)
          (APPEND NIL (get-destribution node-state 0 5 amount initial-row initial-column))
        )
        (T (CONS (LIST row column) (get-destribution node-state row (1+ column) (1- amount) initial-row initial-column)))
      )
    )
    ((= row 0)
      (COND
        ((AND (= row initial-row) (= column initial-column)) ;Verifies if is the start cell
          (APPEND NIL (get-destribution node-state row (1- column) amount initial-row initial-column))
        )
        ((< column 0)
          (APPEND NIL (get-destribution node-state 1 0 amount initial-row initial-column))
        )
        (T (CONS (LIST row column) (get-destribution node-state row (1- column) (1- amount) initial-row initial-column)))
      )
    )
  )
)

(DEFUN get-available-operators(node &optional (row 0) (column 0))
  "Method to return a list of cells in (row column) that are available cells to play"
  (LET (
      (next-row (1+ row)) ;Calculate next row
      (next-column (1+ column)) ;Calculate next column
    )
    (COND
      ((>= row 2) ;Outside of the row range, end condition
        NIL
      )
      ((>= column 6) ;Outside of the column range, jump to next row
        (get-available-operators node next-row 0)
      )
      ((ZEROP (get-cell node row column)) ;Verifies if theres is not a value on the cell to play, jump to next column
        (get-available-operators node row next-column)
      )
      (T ;This cell have values so its a possible play
        (CONS (LIST row column) (get-available-operators node row next-column))
      )
    )
  )
)
;Operation aux



;Operation
(DEFUN operation(node-state row column)
  "Method to recieve a node state and execute the play on the given cell"
  (LET* (
      (copy-node-state node-state) ;Copy of node state to get changed
      (cell-value (get-cell-from-state node-state row column)) ;Get the amount to do the distribution
      (dist-list (get-destribution node-state row column cell-value)) ;Get the distribution list
    )

    (MAPCAR ;Iterates all the distribution list and executes the increment on them
      #'(LAMBDA (x)
        (SETF copy-node-state (increment copy-node-state (FIRST x) (SECOND x)))              
      )
      dist-list
    )

    ;Replaces the starting cell
    (SETF copy-node-state (swap copy-node-state row column 0))

    ;Replaces the last distribution cell if is a finishing value (1 or 3 or 5)
    (LET* (
        (last-row (NTH 0 (CAR (LAST dist-list))))
        (last-column (NTH 1 (CAR (LAST dist-list))))
        (last-cell-value (get-cell-from-state copy-node-state last-row last-column))
      )
      (COND
        ((OR (= last-cell-value 1) (= last-cell-value 3) (= last-cell-value 5))
          (swap copy-node-state last-row last-column 0)
        )
        (T copy-node-state)
      )
    )
  )  
)
;Operations



;Spawner
(DEFUN node-spawner(node-parent row column &optional (heuristic NIL)) 
  "Method to spawn a new node doing the operation on the given cell, this has to be only done on row&columns
  combinations that the get-available-operations return"
  "Even if the algorithm that is using this spawner doesnt use the heuristic value that value will still exist on the node"
  (LET* (
      (parent-state (get-state node-parent)) ;Get node state
      (spawned-state (operation parent-state row column)) ;Get a new state applying the given operator
      (tmp-node (new-node spawned-state node-parent)) ;Create a temp node to in the future calculate the heuristic value
    )
    (IF (NULL heuristic) ;Verifies if was not given an heuristic to calculate on this node
      tmp-node
      (new-node spawned-state node-parent (FUNCALL heuristic tmp-node)) ;Create the final node calculating the heuristic value using the tmp
    )
  )
)

(DEFUN nodes-spawner(node-parent &optional (heuristic NIL))
  "Method to generate the successors of the given node"
  (MAPCAR #'
    (LAMBDA (coord &aux (row (FIRST coord)) (column (SECOND coord))) (node-spawner node-parent row column heuristic))
    (get-available-operators node-parent) ;Get the available operators of the given node
  )
)
;Spawner

(DEFUN get-full-path-string(node)
  "Method to write the node state info up to the root of the path"
  (IF (NULL node)
    ""
    (LET (
        (state (get-state node))
        (depth (get-depth node))
        (heuristic (get-heuristic node))
      )
      (IF (ZEROP heuristic)
        (FORMAT NIL "~a | g - ~a~%~a" state depth (get-full-path-string (get-parent node)))
        (FORMAT NIL "~a | g - ~a | h - ~a~%~a" state depth heuristic (get-full-path-string (get-parent node)))
      )
    )
  )
)