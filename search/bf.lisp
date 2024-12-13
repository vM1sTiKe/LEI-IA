;Aux methods





;Estou a ficar sem espaço
;Por causa que cada iteração está a guardar as listas
;Eu tenho que ter uma variavel com as listas e ir dando setf nelas
(DEFUN dasdsadsadasdsa(dependencies ol &optional (cl NIL))
  "Method to execute the algorithm"
  (IF (NULL ol) ;Error if the open list is empty
    "FAIL" 
    (LET* (
        (node (CAR ol)) ;Get first node from the open list, the one we going to execute the algorithm
        (open-list (CDR ol)) ;Save the rest of the open list
        (closed-list (CONS node cl)) ;Add the node into the closed list

        (successors (bf-generate-successors dependencies node)) ;Get the successors of the node
        (successors-not-closed (bf-not-on-list dependencies successors closed-list)) ;Filter the successors that are not on the closed list
        (successors-not-open (bf-not-on-list dependencies successors-not-closed open-list)) ;Filter again the successors that are not on the open list
        (solution (bf-solution-in-successors dependencies successors-not-open)) ;Get the solution if its on the list of successors

        (merged-open-list (APPEND open-list successors-not-open)) ;Merge open list with the valid successors from this iteration
      )
      (IF (NOT (NULL solution)) ;Verifies if there was a successor that was the solution
        (bf-return solution open-list closed-list) ;Return solution and the data
        (DASDSADSADASDSA dependencies merged-open-list closed-list);Go to next iteration
      )
    )
  )
)