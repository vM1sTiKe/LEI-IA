(IN-PACKAGE :p202000634-202000584)

(DEFPACKAGE :puzzle)
(DEFPACKAGE :minimax-alphabeta)

(LOAD (COMPILE-FILE "./puzzle.lisp"))
(LOAD (COMPILE-FILE "./algorithm.lisp"))



;;; Methods
(DEFUN jogar (state execution-time)
  "
    Arguments:
      - state (list)
      - execution-time (int)

    Method to be called on the algorithms championship, it will call the algorithm and return the info given by it.
  "
  (LET (
      (evaluation (algorithm (puzzle::constructor (NTH 0 state) 0 :points (NTH 1 state))))
    )
    (LIST (puzzle::get-play evaluation) (LIST (puzzle::get-state evaluation) (puzzle::get-points evaluation)))
  )
)

(DEFUN play (&optional (wrong-input NIL))
  "
    Method to start a game, asking the pre-match information.
  "
  (PROGN
    (FORMAT T "~%~%~%~%~%")
    (IF wrong-input (FORMAT T "~%==================~%There was a miss input given, please do it again but with the right inputs~%==================~%"))
    (format t "Select the gamemode:~%")
    (format t "1- Player vs AI~%")
    (format t "2- AI vs AI~%")
    (FORMAT T "3- Player vs Player~%")
    (FORMAT T "Option (1 to 3): ")
    (LET (
        (option (READ)) ;Read user input
      )
      (COND
        ((NOT (NUMBERP option)) (play T)) ;Not a number
        ((OR (< option 1) (> option 3)) (play T)) ;Invalid option
        ((= option 3) (human-vs-human)) ;PvP selected
        ((= option 1) (human-vs-pc (human-vs-pc-pre-match) (pc-pre-match-memoization))) ;PvE selected
        ((= option 2) (pc-vs-pc (pc-pre-match-memoization))) ;EvE selected
      )
    )
  )
)

(DEFUN pc-pre-match-memoization (&optional (wrong-input NIL))
  "
    Method to select if is to use memoization
  "
  (PROGN
    (FORMAT T "~%~%~%~%~%")
    (IF wrong-input (FORMAT T "~%==================~%There was a miss input given, please do it again but with the right inputs~%==================~%"))
    (format t "Select the use of memoization:~%")
    (format t "1- Yes~%")
    (format t "2- No~%")
    (FORMAT T "Option (1 to 2): ")
    (LET (
        (option (READ)) ;Read user input
      )
      (COND
        ((NOT (NUMBERP option)) (pc-pre-match-memoization T)) ;Not a number
        ((OR (< option 1) (> option 2)) (pc-pre-match-memoization T)) ;Invalid option
        ((= option 1) T) ;Yes memoization
        ((= option 2) NIL) ;No memoization
      )
    )
  )
)




;;; VS's
(DEFUN human-vs-pc-pre-match (&optional (wrong-input NIL))
  "
    Method to select who's gonna do the first move.
  "
  (PROGN
    (FORMAT T "~%~%~%")
    (IF wrong-input (FORMAT T "~%==================~%There was a miss input given, please do it again but with the right inputs~%==================~%"))
    (FORMAT T "Select who's playing first (top row):~%")
    (FORMAT T "1- Player~%")
    (FORMAT T "2- AI~%")
    (FORMAT T "Option (1 or 2): ")
    (LET (
        (option (READ)) ;Read user input
      )
      (COND
        ((NOT (NUMBERP option)) (human-vs-pc-pre-match T)) ;Not a number
        ((OR (< option 1) (> option 2)) (human-vs-pc-pre-match T)) ;Invalid option
        ((= option 1) NIL) ;Player starts
        ((= option 2) T) ;AI starts
      )
    )
  )
)

(DEFUN pc-vs-pc (use-memoization &optional (node (constructor T) ))
  (IF (get-winner node)
    (winner (get-winner node))
    (pc-vs-pc use-memoization (pc-play node use-memoization))
  )
)

(DEFUN human-vs-pc (is-ai use-memoization &optional (node (constructor) ))
  "
    Arguments:
      - is-ai (bool): Stating if is AI playing.

    Method to do a game between a human and a AI.
  "
  (COND
    ((get-winner node) (winner (get-winner node))) ;Write who's the winner
    (is-ai (human-vs-pc (NOT is-ai) use-memoization (pc-play node use-memoization))) ;Let AI play
    (T (human-vs-pc (NOT is-ai) use-memoization (human-play node))) ;Let player do the play
  )
)

(DEFUN human-vs-human (&optional (node (constructor) ))
  "
    Method to do a game between two humans
  "
  (IF (get-winner node)
    (winner (get-winner node))
    (human-vs-human (human-play node))
  )
)
;;; VS's



;;; Plays
(DEFUN pc-play (node use-memoization)
  (IF (no-available-play node)
    (skip-turn node)
    (algorithm node use-memoization)
  )
)

(DEFUN human-play (node &optional (wrong-input NIL))
  "
    Arguments:
      - node (list): Node information for the human to play at.

    Returns: A new node with the play the human did.

    Having a node, its asked to the human of the row playing where does he want to play. With the selected cell its created a new node with the play.
  "
  (IF (no-available-play node)
    (skip-turn node)
    (PROGN
      (FORMAT T "~%~%~%~%~%")
      (IF wrong-input (FORMAT T "There was a miss input given, please do it again but with the right inputs~%==================~%"))
      (FORMAT T "Row playing: ~a~%" (IF (= (get-playing-row node) 0) "Top" "Bottom"))
      (FORMAT T "Player type: Human~%")
      (FORMAT T "==================~%")
      (print-node node)
      (FORMAT T "==================~%")
      (FORMAT T "Column to play (1 to 6, from left to right): ")
      ;;Save the operators that are available for the node
      ;;and ask the human to give what cell is to be played at
      (LET (
          (column (READ))
        )
        ;;Verifies if the column is a number
        ; If, not repeat the process for the human to fix the error
        (IF (NOT (NUMBERP column))
          (human-play node T)
          ;;Saves the playing cell (row column), removing 1 from the number sended by the user, since we need 0 to 5 and not 1 to 6 like its asked to him
          (LET (
              (playing-cell (LIST (get-playing-row node) (1- column)))
            )
            ;;Verifies if the cell is a valid operator
            ; If not, repeat the process for the human to fix the error
            ; If yes, executes the operation on the cell
            (IF (NOT (is-available-cell node playing-cell))
              (human-play node T)
              ;;Saves the operation, write it was a success and returns the new node
              (LET (
                  (executed-operation (spawn node playing-cell))
                )
                (PROGN
                  ; (FORMAT T "~%~%Success on execution the play~%")
                  ; (FORMAT T "==================~%")
                  ; (print-node executed-operation)
                  executed-operation
                )
              )
            )
          )
        )
      )
    )
  )
)
;;; Plays



;;; Auxiliar methods to call puzzle methods
(DEFUN constructor (&optional (big NIL))
  "
    Arguments
      - big (Boolean) Optional

    Returns: New node being a full board of 8's or 2's depending if is big or not.
  "
  (IF big
    (puzzle::constructor '((8 8 8 8 8 8) (8 8 8 8 8 8)))
    (puzzle::constructor '((2 2 2 2 2 2) (2 2 2 2 2 2)))
  )
)
(DEFUN algorithm (node &optional (use-memoization NIL))
  "
    Arguments:
      - node (Node)
      - use-memoization (Boolean)

    Returns: New node selected using the algorithm.
  "
  (LET* (
      (start-time (get-internal-real-time)) ;Get begining time of the algorithm execution
      (algorithm-evaluation (minimax-alphabeta::execute 'puzzle::spawner 'puzzle::heuristic 'puzzle::is-solution node 10 use-memoization)) ;Execute algorithm
      (elapsed-time (/ (- (get-internal-real-time) start-time) 1000.0)) ;Get the elapsed time of the algorithm

      (statistics-string (get-statistics-string node (NTH 0 algorithm-evaluation) (NTH 1 algorithm-evaluation) (NTH 2 algorithm-evaluation) (NTH 3 algorithm-evaluation) (NTH 4 algorithm-evaluation) 10 elapsed-time))
    )
    (PROGN
      (FORMAT T statistics-string)
      (write-statistics statistics-string)
      (NTH 0 algorithm-evaluation)
    )
  )
)
(DEFUN skip-turn (node)
  "
    Arguments:
      - node (Node)

    Returns: Node from the enemy player to play
  "
  (puzzle::skip node)
)
(DEFUN no-available-play (node)
  "
    Arguments:
      - node (Node)
    
    Returns: Boolean

    Returns if the given node has any available play, also printing saying if cannot play.
  "
  (IF (NOT (puzzle::can-node-play node))
    (PROGN ;No available play
      (FORMAT T "~%~%~%")
      (FORMAT T "There is no available play. Skipping")
      T
    )
    NIL
  )  
)
(DEFUN get-winner (node)
  "
    Arguments:
      - node (Node).

    Returns: {0,1,NIL}.

    Returns the winner if there is.
  "
  (puzzle::get-winner node)
)
(DEFUN winner (winner)
  "
    Arguments:
      - node ({0,1})

    Returns: void.

    Prints the winner information.
  "
  (PROGN
    (FORMAT T "~%~%~%~%")
    (FORMAT T "======================================~%")
    (COND
      ((EQUAL T winner) (FORMAT T "  No player WON. It was a TIE~%"))
      (T (FORMAT T "  WINNER: Player from the ~a row~%" (IF (= winner 0) "Top" "Bottom")))
    )
    (FORMAT T "======================================")
    (VALUES)
  )
)
(DEFUN print-node (node)
  "
    Arguments:
      - node (Node).

    Returns: void.

    Prints the given node on the screen.
  "
  (puzzle::print-node node)
)
(DEFUN get-playing-row (node)
  "
    Arguments:
      - node (Node).

    Returns: {0,1}.

    Returns that player that will do the next move on the node.
  "
  (puzzle::get-playing-row node)
)
(DEFUN spawn (node cell)
  "
    Arguments:
      - node (Node)
      - cell (int int): (row column), stating the selected play

    Returns: New node with the given row and column as the selected play
  "
  (puzzle::node-spawner node (NTH 0 cell) (NTH 1 cell))
)
(DEFUN is-available-cell (node cell)
  "
    Arguments:
      - node (Node)
      - cell (row column)

    Returns: Boolean, stating if the given cell is a valid play on the given node
  "
  (puzzle::cell-in-operators cell (puzzle::get-available-operators node))
)
;;; Auxiliar methods to call puzzle methods



;;;Statistics
(DEFUN get-statistics-string (original-node solution-node heuristic-value analised-nodes alpha-cuts beta-cuts max-time elapsed-time)
  (CONCATENATE 'string ;Concatenate the statistics into one string to be used
    (FORMAT NIL "~%~%~%")
    (FORMAT NIL "No original: ~a~%" original-node)
    (FORMAT NIL "No solucao: ~a~%" solution-node)
    (FORMAT NIL "Valor heuristico: ~a~%" heuristic-value)
    (FORMAT NIL "Nos analisados: ~a~%" analised-nodes)
    (FORMAT NIL "Cortes | Alpha: ~a | Beta: ~a~%" alpha-cuts beta-cuts)
    (FORMAT NIL "Tempo | Maximo: ~a | Execucao: ~a~%" max-time elapsed-time)
    (FORMAT NIL "-----------------------------------------------------------------------~%")
  )
)

(DEFUN write-statistics (statistics-string)
  "Method to create a log file with the statistics of the resolved problem"
  (WITH-OPEN-FILE (file "../log.dat" :direction :OUTPUT :if-exists :APPEND :if-does-not-exist :CREATE)
    (WRITE-LINE statistics-string file)
    (VALUES)
  )
)
