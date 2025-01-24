; (IN-PACKAGE :p202000634-202000584)

(DEFPACKAGE :puzzle)
(DEFPACKAGE :minimax-alphabeta)

(LOAD (COMPILE-FILE "./puzzle.lisp"))
(LOAD (COMPILE-FILE "./algorithm.lisp"))



;;; Methods
;Teste: (jogar '(((8 8 8 8 8 8) (8 8 8 8 8 8)) (0 0)) 15000)
(DEFUN jogar (state execution-time)
  "
    Arguments:
      - state (list)
      - execution-time (int)

    Method to be called on the algorithms championship, it will call the algorithm and return the info given by it.
  "
  (LET (
      (evaluation (NTH 0 (minimax-alphabeta::execute 'puzzle::spawner 'puzzle::heuristic 'puzzle::is-solution (puzzle::constructor (NTH 0 state) 0 :points (NTH 1 state)) 10 :max-seconds (/ execution-time 1000))))
    )
    (LIST (puzzle::get-play evaluation) (LIST (puzzle::get-state evaluation) (puzzle::get-points evaluation)))
  )
)

(DEFUN play ()
  "
    Method to start a game, asking the pre-match information.
  "
  (PROGN
    (FORMAT T "~%~%~%")
    (format t "Select the gamemode:~%")
    (format t "1- Player vs AI~%")
    (format t "2- AI vs AI~%")
    (FORMAT T "3- Player vs Player~%")
    (LET (
        (option (get-number-from-keyboard 1 3)) ;Read user input
      )
      (COND
        ((= option 3) ;PvP selected
          (PROGN
            (write-logs (FORMAT NIL "~%~%~%=============== Player vs Player ==============="))
            (human-vs-human)
          )
        )
        ((= option 1) ;PvE selected
          (PROGN
            (write-logs (FORMAT NIL "~%~%~%=============== Player vs AI ==============="))
            (human-vs-pc (human-vs-pc-pre-match) (pc-pre-match-memoization))
          )
        )
        ((= option 2) ;EvE selected
          (PROGN
            (write-logs (FORMAT NIL "~%~%~%=============== AI vs AI ==============="))
            (pc-vs-pc (pc-pre-match-memoization))
          )
        )
      )
    )
  )
)



;;; VS's
(DEFUN pc-pre-match-memoization ()
  "
    Method to select if is to use memoization
  "
  (PROGN
    (FORMAT T "~%~%~%")
    (format t "Select the use of memoization:~%")
    (format t "1- Yes~%")
    (format t "2- No~%")
    (LET (
        (option (get-number-from-keyboard 1 2)) ;Read user input
      )
      (COND
        ((= option 1) T) ;Yes memoization
        ((= option 2) NIL) ;No memoization
      )
    )
  )
)

(DEFUN human-vs-pc-pre-match ()
  "
    Method to select who's gonna do the first move.
  "
  (PROGN
    (FORMAT T "~%~%~%")
    (FORMAT T "Select who's playing first (top row):~%")
    (FORMAT T "1- Player~%")
    (FORMAT T "2- AI~%")
    (LET (
        (option (get-number-from-keyboard 1 2)) ;Read user input
      )
      (COND
        ((= option 1) NIL) ;Player starts
        ((= option 2) T) ;AI starts
      )
    )
  )
)

(DEFUN pc-vs-pc (use-memoization &optional (node (constructor T) ))
  (IF (get-winner node)
    (VALUES)
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
    ((get-winner node) (VALUES))
    (is-ai (human-vs-pc (NOT is-ai) use-memoization (pc-play node use-memoization))) ;Let AI play
    (T (human-vs-pc (NOT is-ai) use-memoization (human-play node))) ;Let player do the play
  )
)

(DEFUN human-vs-human (&optional (node (constructor) ))
  "
    Method to do a game between two humans
  "
  (IF (get-winner node)
    (VALUES)
    (human-vs-human (human-play node))
  )
)
;;; VS's



;;; Plays
(DEFUN pc-play (node use-memoization)
  "
    Arguments:
      - node (list)
      - use-memoization (Boolean)

    Returns: A new node with the play algorithm selected.
  "
  (PROGN
    (FORMAT T "~%~%~%")
    (LET (
        (board-string (get-node-string node T))
      )
      (PROGN
        (FORMAT T board-string)
        (write-logs board-string)
        (IF (no-available-play node)
          (skip-turn node)
          (algorithm node use-memoization)
        )
      )
    )
  )
)

(DEFUN human-play (node &optional (wrong-input NIL))
  "
    Arguments:
      - node (list): Node information for the human to play at.

    Returns: A new node with the play the human did.

    Having a node, its asked to the human of the row playing where does he want to play. With the selected cell its created a new node with the play.
  "
  (PROGN
    (FORMAT T "~%~%~%")
    (IF wrong-input (FORMAT T "There was a miss input given, please do it again but with the right inputs~%"))
    (LET (
        (board-string (get-node-string node))
      )
      (PROGN
        (FORMAT T board-string)
        (IF (no-available-play node)
          (PROGN
            (write-logs board-string)
            (skip-turn node)
          )
          (LET* ( 
              (column (get-number-from-keyboard 1 6)) ;Get the column to play at
              (playing-cell (LIST (get-playing-row node) (1- column))) ;Create a (row column) variable
            )
            (IF (NOT (is-available-cell node playing-cell)) ;Verifies if the playing cell is valid
              (human-play node T)
              (PROGN
                (write-logs board-string)
                (spawn node playing-cell)
              )
            )
          )
        )
      )
    )
  )
)
;;; Plays



;;; Aux
(DEFUN get-number-from-keyboard (min-n max-n)
  "
      Arguments
        - min (integer)
        - max (integer)
      
      Returns: Integer readed from keyboard, between the min and max
  "
  (PROGN
    (FORMAT T "Select a number (~a to ~a): " min-n max-n)
    (LET (
        (num (READ))
      )
      (IF (OR (NOT (NUMBERP num)) (OR (< num min-n) (> num max-n))) ;Verifies if is a number and inside the []
        (get-number-from-keyboard min-n max-n)
        num
      )
    )
  )
)
;;; Aux



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
      (algorithm-evaluation (minimax-alphabeta::execute 'puzzle::spawner 'puzzle::heuristic 'puzzle::is-solution node 10 :use-memoization use-memoization)) ;Execute algorithm
      (elapsed-time (/ (- (get-internal-real-time) start-time) 1000.0)) ;Get the elapsed time of the algorithm

      (statistics-string (get-statistics-string node (NTH 0 algorithm-evaluation) (NTH 1 algorithm-evaluation) (NTH 2 algorithm-evaluation) (NTH 3 algorithm-evaluation) (NTH 4 algorithm-evaluation) 10 elapsed-time))
    )
    (PROGN
      (FORMAT T statistics-string)
      (write-logs statistics-string)
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
  (NOT (puzzle::can-node-play node))  
)
(DEFUN get-winner (node)
  "
    Arguments:
      - node (Node).

    Returns: Boolean

    Returns the winner if there is.
  "
  (IF (NULL (puzzle::get-winner node))
    NIL
    (LET (
        (str (CONCATENATE 'string
          (FORMAT NIL (puzzle::get-node-string node))
          (FORMAT nil "~%======================================~%")
          (COND
            ((EQUAL T (puzzle::get-winner node)) (FORMAT NIL "  No player WON. It was a TIE~%"))
            (T (FORMAT NIL "  WINNER: Player from the ~a row~%" (IF (= (puzzle::get-winner node) 0) "Top" "Bottom")))
          )
          (FORMAT NIL "======================================")
        ))
      )
      (PROGN
        (FORMAT T "~%~%~%~%")
        (FORMAT T str)
        (write-logs str)
        T
      )
    )
  )
)
(DEFUN get-node-string (node &optional (is-ai NIL))
  "
    Arguments:
      - node (Node)
      - &optional is-ai (Boolean)

    Returns: Board string
  "
  (CONCATENATE 'string
    (FORMAT NIL "====  ~a ~a  ====~%" (IF is-ai "PC" "HUMAN") (IF (no-available-play node) "!SKIPPING!" ""))
    (FORMAT NIL (puzzle::get-node-string node))
  )
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
    (FORMAT NIL "No original: ~a~%" original-node)
    (FORMAT NIL "No solucao: ~a~%" solution-node)
    (FORMAT NIL "Valor heuristico: ~a~%" heuristic-value)
    (FORMAT NIL "Nos analisados: ~a~%" analised-nodes)
    (FORMAT NIL "Cortes | Alpha: ~a | Beta: ~a~%" alpha-cuts beta-cuts)
    (FORMAT NIL "Tempo | Maximo: ~a | Execucao: ~a~%" max-time elapsed-time)
    (FORMAT NIL "-----------------------------------------------------------------------~%")
  )
)

(DEFUN write-logs (string)
  "Method to create a log file and write on it"
  (WITH-OPEN-FILE (file "../log.dat" :direction :OUTPUT :if-exists :APPEND :if-does-not-exist :CREATE)
    (WRITE-LINE string file)
    (VALUES)
  )
)
