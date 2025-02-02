(IN-PACKAGE :minimax-alphabeta)

(LET (
    (spawner NIL)
    (heuristic NIL)
    (terminal NIL)
    (memoization-usage NIL)

    (algorithm-hashtable NIL)

    (timer-start NIL)
    (timer-max 0)
  )
  (PROGN
    (DEFUN set-spawner (method)
      "
        Arguments
          - method (Function)
        
        Sets the method to generade nodes from one node
      "
      (setf spawner method)
    )
    (DEFUN set-heuristic (method)
      "
        Arguments
          - method (Function)

        Sets the method to be called to calculate the node heuristic/real value
      "
      (setf heuristic method)
    )
    (DEFUN set-terminal (method)
      "
        Arguments
          - method (Function)
        
        Sets the method to be called to verify is is a terminal node
      "
      (setf terminal method)
    )
    (DEFUN set-memoization-usage (boolean)
      "
        Arguments
          - boolean (Boolean)

        Defines if is to use memoization on the algorithm
      "
      (setf memoization-usage boolean)
    )
    (DEFUN set-hashtable ()
      "
        Method to create empty hashtable
      "
      (setf algorithm-hashtable (make-hash-table))
    )
    (DEFUN sethash (key hashtable value)
      "
        Arguments
          - key (Key)
          - hastable (Hash-table)
          - value (any)
        
        Method to add to the hashtable the pair key:value
      "
      (setf (gethash key hashtable) value)
    )
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

    (DEFUN children-spawner-sorter (node is-max-player)
      "
        Arguments
          - node (Node)
          - is-max-player (Boolean)

        Returns: Children from the given noded sorted.

        The use of this method makes the algorithm slower... but its asked to exist.
        No sorting: 1:34 a full game between 2 AIs, on a full 8 board game.
        With sorting: 2:28
      "
      (SORT (FUNCALL spawner node) (IF is-max-player #'< #'>) :key heuristic)
    )

    (DEFUN execute (spawner heuristic terminal node depth &key (use-memoization NIL) (max-miliseconds 36000))
      "
        Arguments:
          - spawner (method): Nodes spawner method, this method has one in param and it is the parent node to generate the childs from;
          - heuristic (method): Heuristic calculator, recieves the node to evaluate. If the node is final it has to return a big number (positive if the playing-row wins);
          - terminal (method): Method to evaluate if the node is terminal;
          - node (Node): Node to execute the minimax-alphabeta algorithm;
          - depth (integer): The max depth;
          - &optional use-memoization (boolean or NIL)

        Returns: The node returned by the algorithm.

        Executes the minimax-alphabeta algorithm and returns the best play.
        Algorithm is to not be called when the player has no available childs to spawn.
      "
      (PROGN
        (set-timer-start (get-internal-real-time)) ;Store time that started
        (set-timer-max max-miliseconds) ;Store max allowed seconds

        (set-spawner spawner) ;Store spawner method
        (set-heuristic heuristic) ;Store heuristic method
        (set-terminal terminal) ;Store evaluation method
        (set-memoization-usage use-memoization) ;Store if is allowed to use memoization
        (set-hashtable) ;Create empty hashtable
        (LET (
            (evaluation (core node depth)) ;Execute algorithm
          )
          (PROGN
            (set-spawner NIL)
            (set-heuristic NIL)
            (set-terminal NIL)
            (set-memoization-usage NIL)
            (set-hashtable)
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
      (IF (gethash node algorithm-hashtable) ;Verifies if the node is cached and return it
        (LIST node (gethash node algorithm-hashtable) 1 0 0)
        (COND ;Execute normaly to find the node value
          ;It will end if no more time, pseudo leaf and leaf
          ((OR (> (- (get-internal-real-time) timer-start) timer-max) (ZEROP depth) (FUNCALL terminal node) (NULL (FUNCALL spawner node))) ;If one of the end conditions
            (LET (
                (heuristic-value (FUNCALL heuristic node)) ;Get the heursitic value of the node
                (depth-calc (IF (FUNCALL terminal node) depth 0)) ;If a node is terminal give him a weight using the depth.
              ) ; The if validating the is-max-player is to inver the values, meaning if the node evaluation of a min node is negative to THAT PLAYER, means it positive to the max player, needing to flip signal
              (PROGN
                (IF memoization-usage (sethash node algorithm-hashtable heuristic-value)) ;Link heuristic value to node on the hash-table (if is to use memoization)
                (LIST node (* (IF is-max-player 1 -1) (1+ depth-calc) heuristic-value) 1 0 0)
              )
            )
          )
          (is-max-player (max-node (children-spawner-sorter node is-max-player) depth alpha beta)) ;minimax maximizer helper
          (T (min-node (children-spawner-sorter node is-max-player) depth alpha beta)) ;minimax minimizer helper
        )
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
