(IN-PACKAGE :search)

(DEFUN node-state(dependencies) (NTH 0 dependencies))
(DEFUN node-is-solution(dependencies) (NTH 2 dependencies))
(DEFUN node-spawner(dependencies) (NTH 3 dependencies))

(DEFUN node-is-member(dependencies node nodes)
  "Method to verify if a node exists on a list of nodes, the existence is verified using the node state"
  (COND
    ((NULL nodes) NIL)
    ;Verify if the states of the node and the first of the list of nodes are equal
    ((EQUAL (FUNCALL (node-state dependencies) node) (FUNCALL (node-state dependencies) (CAR nodes))) T)
    (T (node-is-member dependencies node (CDR nodes))) ;Not equal goes to next iteration
  )
)

(DEFUN nodes-not-on-list(dependencies nodes list)
  "Method to return the nodes that are not on the given list"
  (LET (
      (node (CAR nodes)) ;Current node
    )
    (COND
      ((NULL node) NIL) ;No more nodes
      ;Verify if the current node exists on the given list, go next iteration
      ((node-is-member dependencies node list) (nodes-not-on-list dependencies (CDR nodes) list))
      ;Node doesnt exist on the list, return the node and go to next iteration
      (T (CONS node (nodes-not-on-list dependencies (CDR nodes) list)))
    )
  )
)

(DEFUN spawn-successors(dependencies node &optional (heuristic NIL))
  "Method to call the successors spawners on the given node"
  (FUNCALL (node-spawner dependencies) node heuristic) 
)

(DEFUN return-data(solution opened-list closed-list)
  "Method to return the data from this algortihm, it will return the solution, the generated noded and expanded nodes"
  "Expanded nodes are the ones on the closed list"
  "Generated nodes are the adition of the ones on the closed list, open list and the successors of that iteration"
  (LIST
    solution
    (LENGTH closed-list) ;Expanded nodes
    (+ (LENGTH opened-list) (LENGTH closed-list)) ;Generated nodes
  )
)



;BF Methods
(DEFUN bf(dependencies opened-list &optional (closed-list NIL))
  "Method to execute the breath-first algorithm, recieving the puzzle dependencies and the first node as a list on the opened list"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (LET* (
        (node (CAR opened-list)) ;Get current node
        ;Generate all successors (spawn-successors) and filter for only the valid successors
        (valid-successors (bf_successors-filter dependencies (spawn-successors dependencies node) opened-list closed-list)) 
        (solution (bf_solution dependencies valid-successors)) ;Get the solution if there is
      )
      (PROGN ;Open progn to be able to print to see if algorithhm is working
        (FORMAT T "Node: ~a~%" node)
        (IF (NOT (NULL solution)) ;Verify if solution
          ;Return the solution and the algorithm data, append the successors to the end of the rest of the opened list, add the node to the closed list
          (return-data solution (APPEND (CDR opened-list) valid-successors) (CONS node closed-list)) ;Return solution and the data
          (bf dependencies (APPEND (CDR opened-list) valid-successors) (CONS node closed-list));Go to next iteration
        )
      )
    )
  )
)

(DEFUN bf12(dependencies ol)
  (LET (
      (opened-list ol) ;Opened list
      (closed-list NIL) ;Closed list
    )
    (LOOP WHILE T DO ;Keep looping until solution is found / failure
      (IF (NULL opened-list)
        (RETURN "Failure on search")

        (LET* (
            (node (CAR opened-list)) ;Get current node
            
            (node-successors (spawn-successors dependencies node)) ;Generate all successors
            (valid-successors (bf_successors-filter dependencies node-successors opened-list closed-list)) ;Filter for only the valid successors

            (updated-opened-list (APPEND (CDR opened-list) valid-successors)) ;Update the open list adding the valid-successors into the end of the rest of the opened
            (updated-closed-list (CONS node closed-list)) ;Update the close list adding the current node
            
            (solution (bf_solution dependencies valid-successors)) ;Get the solution if there is
          )
          (PROGN ;Open progn to be able to print to see if algorithhm is working
            (FORMAT T "Node: ~a~%" node)
            (IF (NOT (NULL solution)) ;Verify if solution
              (RETURN (return-data solution updated-opened-list updated-closed-list)) ;Return solution and the data
              (PROGN ;Update globals and keep looping
                (SETF opened-list updated-opened-list)
                (SETF closed-list updated-closed-list)
              )
            )
          )
        )
      )
    )
  )
)

(DEFUN bf_successors-filter(dependencies successors opened-list closed-list)
  "Method to get the successors depending on the closed and open list, the successors cannot be on any of those"
  (LET* (
      (s-not-closed (nodes-not-on-list dependencies successors closed-list))
      (s-not-closed-and-opened (nodes-not-on-list dependencies s-not-closed opened-list))
    )
    s-not-closed-and-opened
  )
)

(DEFUN bf_solution(dependencies successors)
  "Method to return the first successor that is the solution"
  (COND
    ((NULL successors) NIL) ;Successors is empty
    ;Verify if the first node (CAR successors) is the solution
    ((FUNCALL (node-is-solution dependencies) (CAR successors)) (CAR successors))
    (T (bf_solution dependencies (CDR successors))) ;Not equal goes to next iteration
  )
)
;BF Methods
9
8
7
6
5
4
3
2
1
;A* Methods
;A* Methods