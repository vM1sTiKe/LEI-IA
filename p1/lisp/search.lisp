(IN-PACKAGE :search)

(DEFUN dependency-state(dependencies) (NTH 0 dependencies))
(DEFUN dependency-cost(dependencies) (NTH 1 dependencies))
(DEFUN dependency-is-solution(dependencies) (NTH 2 dependencies))
(DEFUN dependency-spawner(dependencies) (NTH 3 dependencies))
(DEFUN dependency-depth(dependencies) (NTH 4 dependencies))

(DEFUN format-node(node)
  (FORMAT T "Node: ~a ~a ~a~%" (NTH 0 node) (NTH 1 node) (NTH 0 (NTH 3 node)))
)

(DEFUN node-is-member(dependencies node nodes)
  "Method to verify if a node exists on a list of nodes, the existence is verified using the node state"
  (COND
    ((NULL nodes) NIL)
    ;Verify if the states of the node and the first of the list of nodes are equal
    ((EQUAL (FUNCALL (dependency-state dependencies) node) (FUNCALL (dependency-state dependencies) (CAR nodes))) T)
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

(DEFUN return-data(solution opened-list closed-list)
  "Method to return the data from this algortihm, it will return the solution, the generated noded and expanded nodes"
  "Expanded nodes are the ones on the closed list"
  "Generated nodes are the adition of the ones on the closed list, open list and the successors of that iteration"
  "The opened list must have already the successors on it"
  "The closed list must have already the parent of the successors on it"
  (LET (
      ;Get the closed list length, we need to remove one from that value, this because the very first node of the closed list is the root node, that one was not expanded
      (closed-length (1- (LENGTH closed-list)))
      (opened-length (LENGTH opened-list))
    )
    (LIST
      solution
      closed-length ;Expanded nodes
      (+ opened-length closed-length) ;Generated nodes
    )
  )
  
)

(DEFUN spawn-successors-not-closed-not-opened(dependencies node opened-list closed-list &optional (heuristic NIL))
  "Method to get successsors from the node and to only return the successors that are not on the opened and closed list"
  (LET* (
      (successors (FUNCALL (dependency-spawner dependencies) node heuristic))
      (not-closed (nodes-not-on-list dependencies successors closed-list))
      (not-closed-and-opened (nodes-not-on-list dependencies not-closed opened-list))
    )
    not-closed-and-opened
  )
)

(DEFUN solution-from-successors(dependencies successors)
  "Method to return the first successor that is the solution"
  (COND
    ((NULL successors) NIL) ;Successors is empty
    ;Verify if the first node (CAR successors) is the solution
    ((FUNCALL (dependency-is-solution dependencies) (CAR successors)) (CAR successors))
    (T (solution-from-successors dependencies (CDR successors))) ;Not equal goes to next iteration
  )
)



;BF
(DEFUN bf(dependencies opened-list &optional (closed-list NIL))
  "Method to execute the breath-first algorithm"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (PROGN
      (format-node (CAR opened-list)) ;Print to see if algorithhm is working
      (LET* (
          (valid-successors (spawn-successors-not-closed-not-opened dependencies (CAR opened-list) opened-list closed-list)) ;Generate all successors
          (solution (solution-from-successors dependencies valid-successors)) ;Get the solution if there is
        )
        (IF solution ;Verify if solution
          ;Return the solution and the algorithm data, append the successors to the end of the rest of the opened list, add the node to the closed list
          (return-data solution (APPEND (CDR opened-list) valid-successors) (CONS (CAR opened-list) closed-list)) ;Return solution and the data
          (bf dependencies (APPEND (CDR opened-list) valid-successors) (CONS (CAR opened-list) closed-list));Go to next iteration
        )
      )
    )
  )
)
;BF



;DF Incomplete, not solving the closed list if the current node is there
(DEFUN df(dependencies max-depth opened-list &optional (closed-list NIL))
  "Method to execute the depth-first algorithm"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (PROGN
      (format-node (CAR opened-list)) ;Print to see if algorithhm is working
      ;Validate the current node depth
      (IF (>= (FUNCALL (dependency-depth dependencies) (CAR opened-list)) max-depth) ;Current node depth is higher than the max allowed depth
        (df dependencies max-depth (CDR opened-list) (CONS (CAR opened-list) closed-list)) ;Go to next iteration without spawning successors

        ;Max depth not reach so keep executing
        (LET* (
            (valid-successors (spawn-successors-not-closed-not-opened dependencies (CAR opened-list) opened-list closed-list)) ;Generate all successors
            (solution (solution-from-successors dependencies valid-successors)) ;Get the solution if there is
          )
          (IF solution ;Verify if solution
            ;Return the solution and the algorithm data, append the successors to the end of the rest of the opened list, add the node to the closed list
            (return-data solution (APPEND valid-successors (CDR opened-list)) (CONS (CAR opened-list) closed-list)) ;Return solution and the data
            (df dependencies max-depth (APPEND valid-successors (CDR opened-list)) (CONS (CAR opened-list) closed-list));Go to next iteration
          )
        )
      )
    )
  )
)
;DF



;A* Incomplete, not swaping higher cost from opened/closed list
(DEFUN a*(dependencies heuristic opened-list &optional (closed-list NIL))
  "Method to execute the a* algorithm, recieving the puzzle dependencies the heuristicm method and the first node as a list on the opened list"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (PROGN
      (format-node (CAR opened-list)) ;Print working node
      ;Validate if current node is the solution
      (IF (FUNCALL (dependency-is-solution dependencies) (CAR opened-list)) ;Verify if node to be worked is solution
        (return-data (CAR opened-list) opened-list closed-list) ;Node is the solution return data~

        ;Node not solution so keep executing
        (LET (
            (valid-successors (spawn-successors-not-closed-not-opened dependencies (CAR opened-list) opened-list closed-list heuristic)) ;Generate all successors
          )
          (a* dependencies heuristic (a*_add-to-open dependencies valid-successors (CDR opened-list)) (CONS (CAR opened-list) closed-list)) ;Go to next iteration
        )
      )
    )
  )
)

(DEFUN a*_sort-nodes(dependencies nodes)
  "Method to do ASC sort by node cost using the given calculator"
  (LABELS ( ;Aux method to do the sorting of the nodes
      (sort (x l) ;Sort nodes from less cost to more cost
        (COND
          ((NULL l) (LIST x))
          ((< (FUNCALL (dependency-cost dependencies) x) (FUNCALL (dependency-cost dependencies) (CAR l)))
            (CONS x l)
          )
          (T
            (CONS (CAR l) (sort x (CDR l)))
          )
        )
      )
    )
    (IF (NULL nodes)
      NIL
      (sort (CAR nodes) (a*_sort-nodes dependencies (CDR nodes)))
    )
  )
)

(DEFUN a*_add-to-open(dependencies nodes opened-list)
  "Method to merge a list of successor nodes and the open nodes list, doing the sort and duplicates removal
  this method is not responsible of verifing if the nodes to add are on the close list or not, that validation should be done before calling this method"
  (a*_sort-nodes dependencies (APPEND opened-list nodes))
)
;A*
