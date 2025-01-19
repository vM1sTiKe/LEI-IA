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
        ; (FORMAT T "~a~%" evaluation)
        (NTH 0 evaluation)
      )
    )
  )
)

(DEFUN core (node depth &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (is-max-player T))
  (LET (
      (positive-negative (IF is-max-player 1 -1))
    )
    (COND
      ((FUNCALL *terminal* node) ;Leaf node. its gona have a influece on the heuristic value because its a leaf
        (LIST node (* positive-negative (1+ depth) (FUNCALL *heuristic* node)))
      )
      ((OR (ZEROP depth) (NULL (FUNCALL *spawner* node))) ;Pseudo leaf, last depth or no children
        (LIST node (* positive-negative (FUNCALL *heuristic* node)))
      )
      (is-max-player (max-node (FUNCALL *spawner* node) depth alpha beta)) ;minimax maximizer helper
      (T (min-node (FUNCALL *spawner* node) depth alpha beta)) ;minimax minimizer helper
    )
  )
)

(DEFUN max-node (children children-depth alpha beta &optional (value most-negative-fixnum) (node NIL))
  (IF (NULL children)
    (LIST node value)
    (LET* (
        (core-evaluation (core (CAR children) (1- children-depth) alpha beta NIL)) ;Execute the minimax evaluation

        (old-value value) ;saves the old value
        (value (max value (NTH 1 core-evaluation)))
        (node (IF (NOT (= old-value value)) (CAR children) node))
      )
      (IF (> value beta) ;Fail hard, beta cut
        (LIST node value)
        (max-node (CDR children) children-depth (max alpha value) beta value node)
      )
    )
  )
)

(DEFUN min-node (children children-depth alpha beta &optional (value most-positive-fixnum) (node NIL))
  (IF (NULL children)
    (LIST node value)
    (LET* (
        (core-evaluation (core (CAR children) (1- children-depth) alpha beta T)) ;Execute the minimax evaluation

        (old-value value) ;saves the old value
        (value (min value (NTH 1 core-evaluation)))
        (node (IF (NOT (= old-value value)) (CAR children) node))
      )
      (IF (< value alpha) ;Fail hard, alpha cut
        (LIST node value)
        (min-node (CDR children) children-depth alpha (min beta value) value node)
      )
    )
  )
)

; (DEFUN max-node (children children-depth alpha beta &optional (value most-negative-fixnum) (node NIL) (alpha-cuts 0) (beta-cuts 0))
;   (IF (NULL children)
;     (LIST node value alpha-cuts beta-cuts)
;     (LET* (
;         (core-evaluation (core (CAR children) (1- children-depth) alpha beta NIL))
;         (alpha-cuts (NTH 2 core-evaluation)) ;Get the amount of alpha cuts
;         (beta-cuts (NTH 3 core-evaluation)) ;Get the amount of beta cuts

;         (old-value value) ;saves the old value
;         (value (max value (NTH 1 core-evaluation)))
;         (node (IF (NOT (= old-value value)) (CAR children) node))
;       )
;       (IF (> value beta) ;Fail hard, beta cut
;         (LIST node value alpha-cuts (1+ beta-cuts))
;         (max-node (CDR children) children-depth (max alpha value) beta value node alpha-cuts beta-cuts)
;       )
;     )
;   )
; )

; (DEFUN min-node (children children-depth alpha beta &optional (value most-positive-fixnum) (node NIL) (alpha-cuts 0) (beta-cuts 0))
;   (IF (NULL children)
;     (LIST node value alpha-cuts beta-cuts)
;     (LET* (
;         (core-evaluation (core (CAR children) (1- children-depth) alpha beta T)) ;Execute the minimax evaluation
;         (alpha-cuts (+ alpha-cuts (NTH 2 core-evaluation))) ;Get the amount of alpha cuts
;         (beta-cuts (+ beta-cuts (NTH 3 core-evaluation))) ;Get the amount of beta cuts

;         (old-value value) ;saves the old value
;         (value (min value (NTH 1 core-evaluation)))
;         (node (IF (NOT (= old-value value)) (CAR children) node))
;       )
;       (IF (< value alpha) ;Fail hard, alpha cut
;         (LIST node value (1+ alpha-cuts) beta-cuts)
;         (min-node (CDR children) children-depth alpha (min beta value) value node alpha-cuts beta-cuts)
;       )
;     )
;   )
; )