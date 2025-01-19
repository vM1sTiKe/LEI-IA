(IN-PACKAGE :minimax-alphabeta)

(defparameter *spawner* NIL)
(defparameter *heuristic* NIL)
(defparameter *terminal* NIL)


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
        (NTH 1 evaluation)
      )
    )
  )
)

(DEFUN core (node depth &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (is-max-player T))
  (COND
    ; Leaf node (meaning its a final node)
    ((FUNCALL *terminal* node)
      (LIST (* (IF is-max-player 1 -1) (* depth (FUNCALL *heuristic* node))) node)
    )
    ; Pseudo leaf (depth 0 or has no possible nodes)
    ((OR (ZEROP depth) (NULL (FUNCALL *spawner* node)))
      (LIST (* (IF is-max-player 1 -1) (FUNCALL *heuristic* node)) node)
    )
    ;minimax helpers
    (is-max-player (max-node (FUNCALL *spawner* node) depth alpha beta))
    (T (min-node (FUNCALL *spawner* node) depth alpha beta))
  )
)

(DEFUN max-node (children children-depth alpha beta &optional (value most-negative-fixnum) (node NIL))
  (IF (NULL children)
    (LIST value node)
    (LET* (
        (old-value value) ;saves the old value
        (value (max value (FIRST (core (CAR children) (1- children-depth) alpha beta NIL))))
        (node (IF (NOT (= old-value value)) (CAR children) node))
      )
      (IF (>= value beta) ;Fail hard, beta cut
        (LIST value node)
        (max-node (CDR children) children-depth (max alpha value) beta value node)
      )
    )
  )
)

(DEFUN min-node (children children-depth alpha beta &optional (value most-positive-fixnum) (node NIL))
  (IF (NULL children)
    (LIST value node)
    (LET* (
        (old-value value) ;saves the old value
        (value (min value (FIRST (core (CAR children) (1- children-depth) alpha beta T))))
        (node (IF (NOT (= old-value value)) (CAR children) node))
      )
      (IF (<= value alpha) ;Fail hard, alpha cut
        (LIST value node)
        (min-node (CDR children) children-depth (min beta value) beta value node)
      )
    )
  )
)