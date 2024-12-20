(defpackage :puzzle
  (:EXPORT :node-dependencies :constructor :get-full-path-string :get-depth :default-heuristic :custom-heuristic)
)

(defpackage :search
  (:EXPORT :bf :a* :df)
)

(defpackage :branching
  (:EXPORT :call)
)

;Load and compile the packages, this because of the algorithms recursivity
(LOAD (COMPILE-FILE "./puzzle.lisp"))
(LOAD (COMPILE-FILE "./search.lisp"))
(LOAD "./branching.lisp")

(DEFUN start()
  (clear-console) ;Clear console when starting
  (LET* (
      (problem (select-problem)) ;Get problem to solve
      (algorithm (select-algorithm)) ;Get algorithm to use
    )
    (execute algorithm problem)
  )
)

(DEFUN select-problem()
  "Method to allow the user to select the problem to work on"
  (LET (
      (problems (read-file-problems))
    )
    (PROGN
      (FORMAT T "~%~%Available Problems~%")
      (print-problems problems)
      (NTH (ask-number (LENGTH problems)) problems)
    )
  )
)

(DEFUN select-algorithm()
  "Method to allow the user to select the algorithm to use"
  (LET (
      (algorithms (get-algorithms))
    )
    (PROGN
      (FORMAT T "~%~%Available Algorithms~%")
      (FORMAT T "1- BF~%")
      (FORMAT T "2- A*~%")
      (FORMAT T "3- DF~%")
      (NTH (ask-number (LENGTH algorithms)) algorithms)
    )
  )
)

(DEFUN select-heuristic()
  "Method to allow the user to select the heuristic to use"
  (LET (
      (heuristics (get-heuristics))
    )
    (PROGN
      (FORMAT T "~%~%Available Heuristics~%")
      (FORMAT T "1- Given by professor~%")
      (FORMAT T "2- Created by group~%")
      (NTH (ask-number (LENGTH heuristics)) heuristics)
    )
  )
)

(DEFUN choose-max-depth()
  "Method to allow the user to choose a max-depth"
  (PROGN
    (FORMAT T "~%~%Max Depth~%")
    (1+ (ask-number 9999))
  )
)

(DEFUN execute(algorithm problem)
  "Method to execute the choosen algorithm on the choosen problem and write the Statistics"
  (LET* (
      (heuristic (IF (is-a* algorithm) (select-heuristic) NIL)) ;If the selected algorithm is a* ask the heuristic
      (max-depth (IF (is-df algorithm) (choose-max-depth) NIL)) ;If the selected algorithm is df ask the max-depth
      (starting-node (LIST (puzzle:constructor problem :heuristic heuristic))) ;Create a node with the selected problem
    )
    (PROGN
      (FORMAT T "~%~%Executing~%")
      (LET* (
          (start-time (get-internal-real-time)) ;Get begining time of the algorithm execution
          (solved-problem ;Call the algorithm depending on conditions
            (COND
              (heuristic (FUNCALL algorithm (puzzle:node-dependencies) heuristic starting-node)) ;a* because we have heuristic
              (max-depth (FUNCALL algorithm (puzzle:node-dependencies) max-depth starting-node)) ;df because we have max-depth
              (T (FUNCALL algorithm (puzzle:node-dependencies) starting-node)) ;bf
            )
          )
          (elapsed-time (/ (- (get-internal-real-time) start-time) 1000.0)) ;Get the elapsed time of the algorithm
        )
        (IF (ATOM solved-problem) ;If the algorithm returns atom (string saying it failed)
          solved-problem ;Return the message
          (LET (
              (statistic-string (get-statistics-string algorithm problem solved-problem elapsed-time :heuristic heuristic :max-depth max-depth))
            )
            (PROGN ;If the algorithm returns list it means it was the solution and no failure happned
              (FORMAT T "~%~%~%Problem solved!~%")
              (FORMAT T "~a" statistic-string)
              (write-statistics statistic-string)
            )
          )
        )
      )
    )
  )
)





;Aux
(DEFUN ask-number(max)
  "Method to read a number, from 0 to n"
  (FORMAT T "Choose one (number): ")
  (LET (
      (n (READ)) ;Read the number the user chooses
    )
    (COND
      ((NULL (NUMBERP n)) (ask-number max)) ;User didnt write a number
      ((OR (<= n 0) (> n max)) (ask-number max)) ;User didnt write a valid number
      (T (- n 1))
    )
  )
)

(DEFUN clear-console(&optional (i 0))
  "Method to clear the console (20 enters)"
  (IF (> i 35) (values) (PROGN (FORMAT T "~%") (clear-console (1+ i))))
)

(DEFUN get-algorithms()
  "Method to get all the available algorithms"
  (LIST
    'search:bf
    'search:a*
    'search:df
  )
)

(DEFUN get-heuristics()
  "Method to get all the available heuristics"
  (LIST
    'puzzle:default-heuristic
    'puzzle:custom-heuristic
  )
)

(DEFUN is-a*(algorithm)
  "Method to verify if the send algorithm is the a*"
  (EQUAL (NTH 1 (get-algorithms)) algorithm)
)

(DEFUN is-df(algorithm)
  "Method to verify if the send algorithm is the a*"
  (EQUAL (NTH 2 (get-algorithms)) algorithm)
)
;Aux



;Statistics & problems
(DEFUN get-statistics-string(algorithm problem solved-problem elapsed-time &key (heuristic NIL) (max-depth NIL))
  (LET* (
      (solution-node (NTH 0 solved-problem))
      (expanded (NTH 1 solved-problem))
      (generated (NTH 2 solved-problem))

      (depth (puzzle:get-depth solution-node))
      (penetration (* (/ depth generated) 1.0)) ;P = L/T
      (avg_branching (* (branching:call generated depth) 1.0)) ;B^1 + B^2 ... + B^L = T
    )
    (CONCATENATE 'string ;Concatenate the statistics into one string to be used
      (FORMAT NIL "Problem: ~a~%" problem)
      (FORMAT NIL "Algorithm: ~a~%" algorithm)
      (IF heuristic (FORMAT NIL "Heuristic: ~a~%" heuristic) NIL)
      (IF max-depth (FORMAT NIL "Max Depth: ~a~%" max-depth) NIL)

      (FORMAT NIL "~%Expanded noded: ~a~%" expanded)
      (FORMAT NIL "Generated noded: ~a~%" generated)

      (FORMAT NIL "~%Penetration: ~a" penetration)
      (FORMAT NIL "~%Average Branching: ~a~%" avg_branching)
      
      (FORMAT NIL "~%Elasped time: ~as~%" elapsed-time)
      (FORMAT NIL "Solution:~%~a" (puzzle:get-full-path-string solution-node))
    )
  )
)

(DEFUN print-problems(problems &optional (max (LENGTH problems)))
  (LABELS (
      (tmp (n p)
        (FORMAT T "Problema ~a: " n)
        (DOLIST (line p) (FORMAT T "~a " line))
        (FORMAT T "~%")
      )
    )
    (LET (
        (p (CAR problems))
        (rest-problems (CDR problems))
      )
      (IF (NULL problems) ;No more problems to print
        (values)
        (PROGN (tmp (- max (LENGTH rest-problems)) p) (print-problems rest-problems max))
      )
    )
  )
)
;Statistics & problems

;Files
(DEFUN read-file-problems()
  (LABELS (
      (line-reader (file)
        (LET (
            (line (READ-LINE file NIL))
          )
          (IF (NULL line)
            NIL
            (CONS (READ-FROM-STRING line) (line-reader file))
          )
        )
      )
    )
    (WITH-OPEN-FILE (file "../problemas.dat" :direction :INPUT :if-does-not-exist :ERROR)
      (line-reader file)
    )
  )
)

(DEFUN write-statistics(statistics-string)
  "Method to create a .dat file with the statistics of the resolved problem"
  (LABELS (
      (current-date-time ()
        (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (get-universal-time))
          (FORMAT NIL "~2,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~4,'0d" hour minute second day month year)
        )
      )
      (file-name ()
        (FORMAT NIL "../statistics/~a.dat" (current-date-time))
      )
    )
    (WITH-OPEN-FILE (file (file-name) :direction :OUTPUT :if-exists :OVERWRITE :if-does-not-exist :CREATE)
      (WRITE-LINE statistics-string file)
      (VALUES)
    )
  )
)