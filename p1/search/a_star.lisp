(IN-PACKAGE :a*)

;Dependency methods
(DEFUN a*-node-state(dependencies) (NTH 0 dependencies))
(DEFUN a*-node-cost(dependencies) (NTH 1 dependencies))
(DEFUN a*-node-is-solution(dependencies) (NTH 2 dependencies))
(DEFUN a*-node-spawner(dependencies) (NTH 3 dependencies))
(DEFUN a*-node-operators(dependencies) (NTH 4 dependencies))
;Dependency methods


;A* aux methods
(DEFUN a*-generate-successors(dependencies heuristic node)
  "Method to generate the successors of the given node"
  (LET (
      (spawner (a*-node-spawner dependencies)) ;Get the spawner method
      (operators (FUNCALL (a*-node-operators dependencies) node)) ;Get the list of possible operators, this is a list of coordinates (x y) where is possible to play
    )
    (MAPCAR #' ;On every operator create a successors of the current node
      (LAMBDA (coord &aux (x (FIRST coord)) (y (SECOND coord)))
        (FUNCALL spawner node x y heuristic)
      )
      operators
    )
  )
)

(DEFUN a*-is-member(dependencies node list)
  "Method to verify if a node exists on a list of nodes, the existence is verified using the state"
  (LET (
      (get-state (a*-node-state dependencies))
    )
    (COND
      ((NULL list) NIL)
      ((EQUAL (FUNCALL get-state node) (FUNCALL get-state (CAR list))) ;Verify if the node is equal to the first of the list
        T
      )
      (T ;Goes to next iteration
        (a*-is-member dependencies node (CDR list))
      )
    )
  )
)





(DEFUN a*-not-closed(dependencies nodes closed-list)
  "Method to return the nodes that are not on the closed list"
  (LET (
      (node (CAR nodes)) ;Current node
    )
    (COND
      ((NULL node) ;No more nodes
        NIL
      )
      ((a*-is-member dependencies node closed-list) ;Verify the current node exists on the closed list, go next iteration
        (a*-not-closed dependencies (CDR nodes) closed-list)
      )
      (T ;Node doesnt exist on the list, return the node and go to next iteration
        (CONS node (a*-not-closed dependencies (CDR nodes) closed-list))
      )
    )
  )
)

;(DEFUN a*-are-closed()) ;This would be done to allow to verify what nodes are already closed and to do the logic on them

(DEFUN a*-add-to-open(dependencies nodes open-list)
  "Method to merge a list of successor nodes and the open nodes list, doing the sort and duplicates removal
  this method is not responsible of verifing if the nodes to add are on the close list or not, that validation should be done before calling this method"
  (a*-remove-duplicates dependencies (a*-sort-nodes dependencies (APPEND open-list nodes)))
)
;A* aux methods



;This algorithm is not fully completed, it is not validating if a node is closed and if he is replacing if the current one has lower f(n)
(DEFUN a*(dependencies heuristic open-list &optional (closed-list NIL))
  "Method to execute the a* algorithm"
  (IF (NULL open-list) ;Error if the open list is empty
    "FAIL" 
    (LET (
        (node (CAR open-list)) ;Get first node from the open list, the one we going to execute the algorithm
        (rest-open-list (CDR open-list)) ;Save the rest of the open list
      )
      (IF (FUNCALL (a*-node-is-solution dependencies) node)
        node ;If the node that is being worked on from the open list is the solution return it
        (LET* (
            (successors (a*-generate-successors dependencies heuristic node)) ;Get the node successors
            (valid-successors (a*-not-closed dependencies successors closed-list)) ;Get sucessors that are not closed, so they are valid to save into the open list
            (merged-open-list (a*-add-to-open dependencies valid-successors rest-open-list)) ;Using the valid successors add them into the open list with the current working node already out of it, that method will already sort them by the f(n) and remove duplicates
            (updated-closed-list (CONS node closed-list)) ;Add the working node into the closed list
          )
          (a* dependencies heuristic merged-open-list updated-closed-list)
          
          ;Missing the removal of nodes from the close list if they have higher cost (and sub tree of that node)
        )
      )
    )
  )
)