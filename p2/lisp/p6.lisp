(IN-PACKAGE :p6)

;Teste: (jogar '(((8 8 8 8 8 8) (8 8 8 8 8 8)) (0 0)) 5000 1)
(DEFUN jogar (state execution-time playing-row)
  "
    Arguments:
      - state (list)
      - execution-time (integer): Max algorithm time
      - playing-row (integer): Player playing

    Will execute the minimax alphabeta algorithm on the given state.
  "
  (LET (
      (evaluation (NTH 0 (minimax-alphabeta (constructor (NTH 0 state) (1- playing-row) :points (NTH 1 state)) 9 :max-miliseconds execution-time)))
    )
    (LIST (get-play evaluation) (LIST (get-state evaluation) (get-points evaluation)))
  )
)

(LET (
    (timer-start NIL)
    (timer-max 0)
  )
  (PROGN
    (DEFUN set-timer-max (&optional (miliseconds 200))
      "
        Arguments
          - miliseconds (Integer)
        
        Method to set the given seconds as the allowed max time the algorithm can execute.
        It will remove a buffer of 0.2 seconds from the max time
      "
      ;Add the timer max as 0 or the sent seconds
      (setf timer-max (- miliseconds 200))
    )
    (DEFUN set-timer-start (time)
      "
        Arguments
          - time (Integer)

        Method to store the time miliseconds
      "
      (setf timer-start time)
    )

    (DEFUN minimax-alphabeta (node depth &key (max-miliseconds 36000))
      "
        Arguments:
          - spawner (method): Nodes spawner method, this method has one in param and it is the parent node to generate the childs from;
          - heuristic (method): Heuristic calculator, recieves the node to evaluate. If the node is final it has to return a big number (positive if the playing-row wins);
          - terminal (method): Method to evaluate if the node is terminal;
          - node (Node): Node to execute the minimax-alphabeta algorithm;
          - depth (integer): The max depth;

        Returns: The node returned by the algorithm.

        Executes the minimax-alphabeta algorithm and returns the best play.
        Algorithm is to not be called when the player has no available childs to spawn.
      "
      (PROGN
        (set-timer-start (get-internal-real-time)) ;Store time that started
        (set-timer-max max-miliseconds) ;Store max allowed seconds

        (LET (
            (evaluation (core node depth)) ;Execute algorithm
          )
          (PROGN
            (set-timer-start NIL)
            (set-timer-max)
            evaluation
          )
        )
      )
    )

    (DEFUN core (node depth &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (is-max-player T))
      "
        Arguments
          - node (Node)
          - depth (int)
          - &optional alpha (integer or most-negative-fixnum)
          - &optional beta (integer or most-positive-fixnum)
          - &optional is-max-player (boolean or T)

        Returns
          - (node heuristic-value 1 0 0)
          - (node heuristic-value analysed-nodes alpha-cuts beta-cuts): From min/max helpers

        Executes the minimax with alpha-beta pruning. It returns with the node evaluation, the node and some statistics related to the execution.
      "
      (COND ;Execute normaly to find the node value
        ;It will end if no more time, pseudo leaf and leaf
        ((OR (> (- (get-internal-real-time) timer-start) timer-max) (ZEROP depth) (is-solution node) (NULL (spawner node))) ;If one of the end conditions
          (LET (
              (heuristic-value (heuristic node)) ;Get the heursitic value of the node
              (depth-calc (IF (is-solution node) depth 0)) ;If a node is terminal give him a weight using the depth.
            ) ; The if validating the is-max-player is to inver the values, meaning if the node evaluation of a min node is negative to THAT PLAYER, means it positive to the max player, needing to flip signal
            (LIST node (* (IF is-max-player 1 -1) (1+ depth-calc) heuristic-value) 1 0 0)
          )
        )
        (is-max-player (max-node (spawner node) depth alpha beta)) ;minimax maximizer helper
        (T (min-node (spawner node) depth alpha beta)) ;minimax minimizer helper
      )
    )

    (DEFUN max-node (children children-depth alpha beta &optional (value most-negative-fixnum) (node NIL) (nodes-analysed 0) (alpha-cuts 0) (beta-cuts 0))
      "
        Max node helper
      "
      (IF (NULL children)
        (LIST node value nodes-analysed alpha-cuts beta-cuts)
        (LET* (
            (core-evaluation (core (CAR children) (1- children-depth) alpha beta NIL)) ;Execute the minimax evaluation
            (nodes-analysed (+ nodes-analysed (NTH 2 core-evaluation))) ;Get the amount of analysed nodes
            (alpha-cuts (+ alpha-cuts (NTH 3 core-evaluation))) ;Get the amount of alpha cuts
            (beta-cuts (+ beta-cuts (NTH 4 core-evaluation))) ;Get the amount of beta cuts

            (old-value value) ;saves the old value
            (value (max value (NTH 1 core-evaluation)))
            (node (IF (NOT (= old-value value)) (CAR children) node))
          )
          (IF (> value beta) ;Fail hard, beta cut
            (LIST node value nodes-analysed alpha-cuts (1+ beta-cuts))
            (max-node (CDR children) children-depth (max alpha value) beta value node nodes-analysed alpha-cuts beta-cuts)
          )
        )
      )
    )

    (DEFUN min-node (children children-depth alpha beta &optional (value most-positive-fixnum) (node NIL) (nodes-analysed 0) (alpha-cuts 0) (beta-cuts 0))
      "
        Min node helper
      "
      (IF (NULL children)
        (LIST node value nodes-analysed alpha-cuts beta-cuts)
        (LET* (
            (core-evaluation (core (CAR children) (1- children-depth) alpha beta T)) ;Execute the minimax evaluation
            (nodes-analysed (+ nodes-analysed (NTH 2 core-evaluation))) ;Get the amount of analysed nodes
            (alpha-cuts (+ alpha-cuts (NTH 3 core-evaluation))) ;Get the amount of alpha cuts
            (beta-cuts (+ beta-cuts (NTH 4 core-evaluation))) ;Get the amount of beta cuts

            (old-value value) ;saves the old value
            (value (min value (NTH 1 core-evaluation)))
            (node (IF (NOT (= old-value value)) (CAR children) node))
          )
          (IF (< value alpha) ;Fail hard, alpha cut
            (LIST node value nodes-analysed (1+ alpha-cuts) beta-cuts)
            (min-node (CDR children) children-depth alpha (min beta value) value node nodes-analysed alpha-cuts beta-cuts)
          )
        )
      )
    )
  )
)



;;; Node
(DEFUN constructor (state &optional (playing-row 0) &key (play NIL) (points '(0 0)))
  "Method to create a new node with the given params"
  "The points are related to the points of each player of each line, so the first value is the points that the player of the row 0 has"
  "the second value is the points that the player of the row 1 has"
  (LIST
    play ;coordinates that were used to get to this node
    state ;List with the state of the board
    points ;List with points from the players (p1 p2), points are the amount of pieces removed
    playing-row ;Row that this node can be played at
  )
)
(DEFUN heuristic (node)
  "Method to do an heuristic evaluation of the given node, and the player playing"
  (LET* (
      (playing-row (get-playing-row node))
      (oposite-row (swap-playing-row playing-row))

      (points-playing-row (* 10 (NTH playing-row (get-points node)) ))
      (points-oposite-row (* 10 (NTH oposite-row (get-points node)) ))
    )
    (IF (is-solution node)
      (IF (> points-playing-row points-oposite-row) 9999 -9999)
      (LET* (
          (empty-cells-playing-row (* 2 (get-row-empty-cells node playing-row)))
          (empty-cells-oposite-row (get-row-empty-cells node oposite-row))
        )
        (+ points-playing-row (- points-oposite-row) empty-cells-playing-row (- empty-cells-oposite-row))
      )
    )
  )
)
(DEFUN get-play (node)
  "Returns the play that originated the node"
  (NTH 0 node)
)
(DEFUN get-state (node)
  "Returns the state of the given node"
  (NTH 1 node)
)
(DEFUN get-points (node)
  "Returns the points of the row"
  (NTH 2 node)
)
(DEFUN get-playing-row (node)
  "Returns the playing row, what row this node can be played at"
  (NTH 3 node)
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
;;;Node related methods

;;;Operation aux
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
(DEFUN get-distribution (node-state row column &optional (amount (get-state-cell node-state row column)) (initial-row row) (initial-column column))
  "Method to calculate the distribution list from a state and cell"
  (COND
    ((= 0 amount) NIL)
    ((= row 1)
      (COND
        ((AND (= row initial-row) (= column initial-column)) ;Verifies if is the start cell
          (APPEND NIL (get-distribution node-state row (1+ column) amount initial-row initial-column))
        )
        ((>= column 6)
          (APPEND NIL (get-distribution node-state 0 5 amount initial-row initial-column))
        )
        (T (CONS (LIST row column) (get-distribution node-state row (1+ column) (1- amount) initial-row initial-column)))
      )
    )
    ((= row 0)
      (COND
        ((AND (= row initial-row) (= column initial-column)) ;Verifies if is the start cell
          (APPEND NIL (get-distribution node-state row (1- column) amount initial-row initial-column))
        )
        ((< column 0)
          (APPEND NIL (get-distribution node-state 1 0 amount initial-row initial-column))
        )
        (T (CONS (LIST row column) (get-distribution node-state row (1- column) (1- amount) initial-row initial-column)))
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
;;;Operation aux

;;;Operations
(DEFUN operation (node-state row column &optional (dist-list (get-distribution node-state row column (get-state-cell node-state row column))) (removed-pieces 0))
  "
    Arguments:
      - node-state (list): List holding the node state.
      - row (integer)
      - column (integer)

    Aux Arguments:
      - &optional dist-list (list): List of coordinates to distribute pieces.
      - &optional removed-pieces (integer): Amount of pieces that were removed

    Returns: A new node state where all the pieces on the original (row column) were distributed on the counter-clock wise cells.

    Having a node state and a coordinate, get the amount of pieces on that cell and distribute them counter-clock wise on the whole board.
    Increment one piece on every cell that the 'get-distribution' method says its to distribute, this will recursively call this method and saving the new value on the node state argument.
    On the last distribution verifies if we are on the oposite row from the staring 'row' argument, if yes we remove the pieces if the amount on the cell is 1 or 3 or 5.
    Saves the amount of removed pieces and then return a list with the new node state originated from the original one with the amount of pieces that were removed.
  "

  ;;Verifies if the dist-list is NIL
  ;;  If yes, means we finished the operation so we can finaly swap the starting cell to 0 and return the new node-state and the amount of removed pieces
  ;;  If not, we continue executing the operation on the recieved node-state
  (COND
    ((NULL dist-list) 
      (LIST (swap node-state row column 0) removed-pieces)
    )
    (T
      ;;Get the first row and column from the dist-list, those are the coordinates that its gonna be incremented at
      (LET (
          (dist-row (NTH 0 (CAR dist-list)))
          (dist-column (NTH 1 (CAR dist-list)))
        )
        ;;Verifies if we are on the last element of the dist-list (meaning its the last increment that is going to happen)
        ;;  If yes, keep executing doing validation to remove or not pieces
        ;;  If not, execute the increment on the cell and current node-state and send it to the next iteration of this method, sending the rest of the dist-list
        (COND
          ((= 1 (LENGTH dist-list))
            ;;Verifies if we the current distribution row is the same as the first row sended to the method
            ;;  If yes, the rules say we dont remove any pieces, so execute the normal increment and send to the next iteration the dist-list as NIL
            ;;  If not, its verified if the pieces on the cell are going to be 1 or 3 or 5 to remove or not
            (IF (= dist-row row)
              (operation (increment node-state dist-row dist-column) row column NIL)
              ;;Get the pieces on the cell to execute the distribution at, before the increment
              (LET* (
                  (pieces-before-increment (get-state-cell node-state dist-row dist-column))
                )
                ;;Verifies if the amount of pieces before the increment is 0 or 2 or 4
                ;;  If yes, by the rules, after the increment the pieces on the cell are going to be 1 or 3 or 5 meaning they have to be removed, and that cell swap to 0 pieces (they got removed)
                ;;  If not, just do a normal increment and send dist-list as NIL to last iteration
                (IF (OR (= pieces-before-increment 0) (= pieces-before-increment 2) (= pieces-before-increment 4))
                  (operation (swap node-state dist-row dist-column 0) row column NIL (1+ pieces-before-increment))
                  (operation (increment node-state dist-row dist-column) row column NIL)
                )
              )
            )
          )
          (T (operation (increment node-state dist-row dist-column) row column (CDR dist-list)))
        )
      )
    )
  )
)
;;;Operations

;;;Spawner
(DEFUN node-spawner (node-parent row column) 
  "Method to spawn a new node doing the operation on the given cell, this has to be only done on row&columns"
  "combinations that the get-available-operations return"
  "The return of the operation will originate a new node with the playing-row being the oposite of the parent node"
  (LET* (
      (spawned (operation (get-state node-parent) row column)) ;Get a new state applying the given operator and the amount of removed pieces

      (oposite-row (swap-playing-row row)) ;Get the oposite row of the one that was just played
      (spawned-points (NTH 1 spawned)) ;Get from the operation the points that were removed from the execution of it
      (concat-points-list (add-points-to-list (get-points node-parent) row spawned-points)) ;Concat to the parent points the points returned from the operation, placing them on the execution row
    )
    (constructor (NTH 0 spawned) oposite-row :play (LIST row column) :points concat-points-list)
  )
)
(DEFUN spawner (node-parent)
  "Method to generate the successors of the given node"
  (MAPCAR #'
    (LAMBDA (coord &aux (row (FIRST coord)) (column (SECOND coord))) (node-spawner node-parent row column))
    (get-available-operators node-parent) ;Get the available operators of the given node, this will only generate operator of the row that can play
  )
)
;;;Spawner

;;;Aux
(DEFUN swap-playing-row (playing-row)
  "Method to recieve the playing row and swap it for the opposite row"
  (IF (= playing-row 1) 0 1)
)
(DEFUN add-points-to-list (points-list row points)
  "Recieves a list of points and add the given points to the right element"
  (IF (<= row 0)
      (CONS (+ (NTH 0 points-list) points) (CDR points-list))
      (CONS (CAR points-list) (add-points-to-list (CDR points-list) (1- row) points)) ;Goes to next iteration
  )
)
(DEFUN get-row-empty-cells (node row)
  "
    Arguments:
      - node (Node)
      - row ({0,1})

    Returns: Amount of empty cells on the given row
  "
  (APPLY #'+ (MAPCAR #'
    (LAMBDA(cell) (IF (ZEROP cell) 1 0))
    (NTH row (get-state node)) ;Get the row from the state
  ))  
)
;;;Aux
