; (defpackage :puzzle
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