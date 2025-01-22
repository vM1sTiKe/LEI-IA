(IN-PACKAGE :minimax-alphabeta)

(defparameter *spawner* NIL)
(defparameter *heuristic* NIL)
(defparameter *terminal* NIL)

(defparameter *algorithm-hash* (make-hash-table))


(DEFUN set-spawner (method)
  (setf *spawner* method)
)
(DEFUN set-heuristic (method)
  (setf *heuristic* method)
)
(DEFUN set-terminal (method)
  (setf *terminal* method)
)

(DEFUN execute (spawner heuristic terminal node depth)
  "
    Arguments:
      - spawner (method): Nodes spawner method, this method has one in param and it is the parent node to generate the childs from;
      - heuristic (method): Heuristic calculator, recieves the node to evaluate. If the node is final it has to return a big number (positive if the playing-row wins);
      - terminal (method): Method to evaluate if the node is terminal;
      - node (Node): Node to execute the minimax-alphabeta algorithm;
      - depth (int): The max depth;

    Returns: The node returned by the algorithm.

    Executes the minimax-alphabeta algorithm and returns the best play.
    Algorithm is to not be called when the player has no available childs to spawn.
  "
  (PROGN
    (set-spawner spawner)
    (set-heuristic heuristic)
    (set-terminal terminal)
    (LET (
        (evaluation (core node depth)) ;Execute algorithm
      )
      (PROGN
        ; (set-spawner NIL)
        ; (set-heuristic NIL)
        ; (set-terminal NIL)
        ; evaluation
        (FORMAT T "~a~%" evaluation)
        (NTH 0 evaluation)
      )
    )
  )
)

(DEFUN children-spawner-sorter (node is-max-player)
  "
    Arguments
      - node (Node)
      - is-max-player (Boolean)

    Returns: Children from the given noded sorted.

    The use of this method makes the algorithm slower... but its asked to exist.
    No sorting: 1:34 a full game between 2 AIs, on a full 8 board game.
    Com sorting: 2:28
  "
  (SORT (FUNCALL *spawner* node) (IF is-max-player #'< #'>) :key *heuristic*)
  ; (FUNCALL *spawner* node)
)

(DEFUN core (node depth &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (is-max-player T))
  (IF (gethash node *algorithm-hash*) ;Verifies if the node is cached
    (LIST node (gethash node *algorithm-hash*)) ;Return if cached
    (COND ;Execute normaly to find the node value
      ((OR (ZEROP depth) (FUNCALL *terminal* node) (NULL (FUNCALL *spawner* node))) ;If one of the end conditions
        (LET (
            (heuristic-value (FUNCALL *heuristic* node)) ;Get the heursitic value of the node
            (depth-calc (IF (FUNCALL *terminal* node) depth 0)) ;If a node is terminal give him a weight using the depth.
          ) ; The if validating the is-max-player is to inver the values, meaning if the node evaluation of a min node is negative to THAT PLAYER, means it positive to the max player, needing to flip signal
          (PROGN
            ; (setf (gethash node *algorithm-hash*) heuristic-value) ;Link heuristic value to node on the hash-table
            (LIST node (* (IF is-max-player 1 -1) (1+ depth-calc) heuristic-value)) ;Depth-calc will be the weight or 0, simulating it was found on the last depth, it cannot be 0 because multiplication so increment 1
          )
        )
      )
      (is-max-player (max-node (children-spawner-sorter node is-max-player) depth alpha beta)) ;minimax maximizer helper
      (T (min-node (children-spawner-sorter node is-max-player) depth alpha beta)) ;minimax minimizer helper
    )
  )
)

(DEFUN teste (children heuristic)
  (dolist (child (SORT (COPY-SEQ children) #'> :key heuristic))
    (FORMAT T "~a~%" (FUNCALL heuristic child))
    (VALUES)
  )
)

;Hashtable is half done, cant continue because like there is no memory. LOL

(DEFUN max-node (children children-depth alpha beta &optional (value most-negative-fixnum) (node NIL) (alpha-cuts 0) (beta-cuts 0))
  (IF (NULL children)
    (LIST node value alpha-cuts beta-cuts)
    (LET* (
        (core-evaluation (core (CAR children) (1- children-depth) alpha beta NIL)) ;Execute the minimax evaluation
        (alpha-cuts (+ alpha-cuts (OR (NTH 2 core-evaluation) 0))) ;Get the amount of alpha cuts
        (beta-cuts (+ beta-cuts (OR (NTH 3 core-evaluation) 0))) ;Get the amount of beta cuts

        (old-value value) ;saves the old value
        (value (max value (NTH 1 core-evaluation)))
        (node (IF (NOT (= old-value value)) (CAR children) node))
      )
      (IF (> value beta) ;Fail hard, beta cut
        (LIST node value alpha-cuts (1+ beta-cuts))
        (max-node (CDR children) children-depth (max alpha value) beta value node alpha-cuts beta-cuts)
      )
    )
  )
)

(DEFUN min-node (children children-depth alpha beta &optional (value most-positive-fixnum) (node NIL) (alpha-cuts 0) (beta-cuts 0))
  (IF (NULL children)
    (LIST node value alpha-cuts beta-cuts)
    (LET* (
        (core-evaluation (core (CAR children) (1- children-depth) alpha beta T)) ;Execute the minimax evaluation
        (alpha-cuts (+ alpha-cuts (OR (NTH 2 core-evaluation) 0))) ;Get the amount of alpha cuts
        (beta-cuts (+ beta-cuts (OR (NTH 3 core-evaluation) 0))) ;Get the amount of beta cuts

        (old-value value) ;saves the old value
        (value (min value (NTH 1 core-evaluation)))
        (node (IF (NOT (= old-value value)) (CAR children) node))
      )
      (IF (< value alpha) ;Fail hard, alpha cut
        (LIST node value (1+ alpha-cuts) beta-cuts)
        (min-node (CDR children) children-depth alpha (min beta value) value node alpha-cuts beta-cuts)
      )
    )
  )
)
