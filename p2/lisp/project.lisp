; (DEFPACKAGE :puzzle
;   (:EXPORT :node-dependencies :constructor :get-full-path-string :get-depth :default-heuristic :custom-heuristic)
; )

;Load and compile the packages, this because of the algorithms recursivity
(LOAD (COMPILE-FILE "./puzzle.lisp"))



(DEFUN play()
  (LET (
      (row (FORMAT NIL "Linha a jogar ~a" (read)))
    )
    row
  )
)

(DEFUN jogar (board execution-time)
  "Method to be called to join the champion ship"
  (constructor (NTH 0 board) 0 :points (NTH 1 board))
)

;Test: (human-play (constructor '((4 4 4 4 4 4) (4 4 4 4 4 4)) 0))
(DEFUN human-play (node &optional (wrong-input NIL))
  "
    Arguments:
      - node (list): Node information for the human to play at.

    Returns: A new node with the play the human did.

    Having a node, its asked to the human of the row playing where does he want to play. With the selected cell its created a new node with the play.
  "
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
          (IF (NOT (cell-in-operators playing-cell (get-available-operators node)))
            (human-play node T)
            ;;Saves the operation, write it was a success and returns the new node
            (LET (
                (executed-operation (node-spawner node (NTH 0 playing-cell) (NTH 1 playing-cell)))
              )
              (PROGN
                (FORMAT T "~%~%Success on execution the play~%")
                (FORMAT T "==================~%")
                (print-node executed-operation)
                executed-operation
              )
            )
          )
        )
      )
    )
  )
)