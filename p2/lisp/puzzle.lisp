; (IN-PACKAGE :puzzle)

;Node related methods
(DEFUN constructor (state playing-row &key (play NIL) (parent NIL) (points '(0 0)))
  "Method to create a new node with the given params"
  "The points are related to the points of each player of each line, so the first value is the points that the player of the row 0 has"
  "the second value is the points that the player of the row 1 has"
  (LIST
    play ;coordinates that were used to get to this node
    state ;List with the state of the board
    points ;List with points from the players (p1 p2), points are the amount of pieces removed
    (IF (NULL parent) 0 (1+ (get-depth parent)))
    parent
    playing-row ;Row that this node can be played at
  )
)

(DEFUN get-state (node)
  "Returns the state of the given node"
  (NTH 1 node)
)

(DEFUN get-points (node)
  "Returns the points of the row"
  (NTH 2 node)
)

(DEFUN get-depth (node)
  "Returns the depth value of the given node"
  (NTH 3 node)
)

(DEFUN get-parent (node)
  "Returns the depth value of the given node"
  (NTH 4 node)
)

(DEFUN get-playing-row (node)
  "Returns the playing row, what row this node can be played at"
  (NTH 5 node)
)

(DEFUN oposite-playing-row (playing-row)
  (IF (= playing-row 1)
    0
    1
  )
)

;Test: (add-points '(0 1) 0 5)
;Results: (5 1)
(DEFUN add-points (points-list row points)
  "Recieves a list of points and add the given points to the right element"
  (IF (<= row 0)
      (CONS (+ (NTH 0 points-list) points) (CDR points-list))
      (CONS (CAR points-list) (add-points (CDR points-list) (1- row) points)) ;Goes to next iteration
  )
)

;Test: (is-solution (constructor '((0 0 0 0 0 0) (0 0 0 0 0 1)) 0))
(DEFUN is-solution (node &aux (node-state (get-state node)))
  "Verifies is the given nodes is have the final state"
  (LET (
      (top-row (NTH 0 node-state)) ;Split state and get top row
      (bot-row (NTH 1 node-state)) ;Split state and get bottom row
    )
    (ZEROP (+ (APPLY #'+ top-row) (APPLY #'+ bot-row))) ;Sums both rows and verify if is 0
  )
)

(DEFUN get-state-cell (node-state row column)
  "Method to get a cell value from a node-state"
  (NTH column (NTH row node-state))
)
;Node related methods



;Operation aux
(DEFUN swap-in-row (row column &optional (value 0))
  "Method to swap the given column on the row(list) and set the number on that given column"
  (IF (<= column 0)
      (CONS value (CDR row))
      (CONS (CAR row) (swap-in-row (CDR row) (1- column) value)) ;Goes to next iteration
  )
)

(DEFUN swap (node-state row column &optional (value 0))
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

(DEFUN increment (node-state row column)
  "Method to increment by one the value on the given cell"
  (LET (
      (current-cell-value (get-state-cell node-state row column))
    )
    (swap node-state row column (1+ current-cell-value))
  )
)

(DEFUN get-destribution (node-state row column &optional (amount (get-state-cell node-state row column)) (initial-row row) (initial-column column))
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

(DEFUN get-available-operators (node &optional (row 0) (column 0) &aux (node-state (get-state node)))
  "Method to return a list of cells in (row column) that are available cells to play"
  (COND
    ((>= row 2) NIL) ;Outside of the row range, end condition
    ((>= column 6) (get-available-operators node (1+ row) 0)) ;Outside of the column range, jump to next row
    ((NOT (= row (get-playing-row node))) (get-available-operators node (1+ row) 0)) ;Skip row if its not the allowed row to play
    ((ZEROP (get-state-cell node-state row column)) ;Verifies if theres is not a value on the cell to play, jump to next column
      (get-available-operators node row (1+ column))
    )
    (T ;This cell have values so its a possible play
      (CONS (LIST row column) (get-available-operators node row (1+ column)))
    )
  )
)
;Operation aux



;Operation
;Test: (operation '((4 4 4 4 4 4) (4 4 4 4 4 4)) 0 0)
;Result: (((0 4 4 4 4 4) (5 5 5 0 4 4)) 5)
(DEFUN operation (node-state row column &optional (dist-list (get-destribution node-state row column (get-state-cell node-state row column))) (removed-pieces 0))
  "Method to recieve a node state and execute the play on the given cell"
  "dist-list is the list of cells that will be played because of this (row column)"
  "row and column will not be used outside of when this method is called to get the dist-list"
  "The row is the 'playing-row' that the node is allowed to play, it is true because this method is to be called after the get-distribution from the node that has the original state"
  (COND
    ((NULL dist-list) ;When dist-list is empty return the last node-state created, here will swap the played cell to 0
      (LIST
        (swap node-state row column 0) ;Return the node
        removed-pieces ;Return the amount of pieces that were removed on the previous play
      )
    )
    (T ;Dist-list is not empty keep the operation going
      (LET (
          (dist-row (NTH 0 (CAR dist-list))) ;Get the row of the first distribution
          (dist-column (NTH 1 (CAR dist-list))) ;Get the column of the first distribution
        )
        (COND
          ((= 1 (LENGTH dist-list)) ;We are on the last distribution, need to verify if its to remove values
            (IF (= dist-row row)
              (operation (increment node-state dist-row dist-column) row column NIL) ;Dont remove because the last distribution is on the row that is playing (execute normaly)
              (LET* ( ;Remove if the pieces = 1/3/5, we are on diff row than the playing row
                  (incremented-state (increment node-state dist-row dist-column)) ;Increment first
                  (pieces (get-state-cell incremented-state dist-row dist-column)) ;Get the amount of pieces on the cell after increment
                )
                (IF (OR (= pieces 1) (= pieces 3) (= pieces 5)) ;Replace if after the increment we have 1/3/5 pieces on the cell
                  (operation (swap incremented-state dist-row dist-column 0) row column NIL pieces)
                  (operation incremented-state row column NIL) ;The last pieces amount was not valid to remove, so dont swap to 0
                )
              )
            )
          )
          (T ;Increment the pieces on the current (row column) and send the created state to next iteration
            (operation (increment node-state dist-row dist-column) row column (CDR dist-list))
          )
        )
      )
    )
  )
)
;Operations



;Spawner
;Test: (node-spawner (constructor '((4 4 4 4 4 4) (4 4 4 4 4 4)) 0) 0 0)
;Result: ((0 0) ((0 4 4 4 4 4) (5 5 5 0 4 4)) (5 0) 1 (NIL ((4 4 4 4 4 4) (4 4 4 4 4 4)) (0 0) 0 NIL 0) 1)
(DEFUN node-spawner (node-parent row column) 
  "Method to spawn a new node doing the operation on the given cell, this has to be only done on row&columns"
  "combinations that the get-available-operations return"
  "The return of the operation will originate a new node with the playing-row being the oposite of the parent node"
  (LET (
      (parent-points (get-points node-parent)) ;Get the points list of the parent
      (oposite-playing-row (oposite-playing-row (get-playing-row node-parent))) ;Get the oposite playing row from the parent node
      (spawned (operation (get-state node-parent) row column)) ;Get a new state applying the given operator and the amount of removed pieces
    )
    (constructor (NTH 0 spawned) oposite-playing-row :play (LIST row column) :parent node-parent :points (add-points parent-points row (NTH 1 spawned)))
  )
)

(DEFUN nodes-spawner (node-parent)
  "Method to generate the successors of the given node"
  (MAPCAR #'
    (LAMBDA (coord &aux (row (FIRST coord)) (column (SECOND coord))) (node-spawner node-parent row column))
    (get-available-operators node-parent) ;Get the available operators of the given node, this will only generate operator of the row that can play
  )
)
;Spawner
