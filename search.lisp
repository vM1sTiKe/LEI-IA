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

(DEFUN spawn-successors(dependencies node &optional (heuristic NIL))
  "Method to call the successors spawners on the given node"
  (FUNCALL (dependency-spawner dependencies) node heuristic) 
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
      (PROGN
        (format-node node) ;Print to see if algorithhm is working
        (IF (NOT (NULL solution)) ;Verify if solution
          ;Return the solution and the algorithm data, append the successors to the end of the rest of the opened list, add the node to the closed list
          (return-data solution (APPEND (CDR opened-list) valid-successors) (CONS node closed-list)) ;Return solution and the data
          (bf dependencies (APPEND (CDR opened-list) valid-successors) (CONS node closed-list));Go to next iteration
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
    ((FUNCALL (dependency-is-solution dependencies) (CAR successors)) (CAR successors))
    (T (bf_solution dependencies (CDR successors))) ;Not equal goes to next iteration
  )
)
;BF Methods



;A* Methods
;This algorithm is not fully completed, it is not validating if a node is closed and if he is replacing if the current one has lower f(n)
(DEFUN a*(dependencies heuristic opened-list &optional (closed-list NIL))
  "Method to execute the a* algorithm, recieving the puzzle dependencies the heuristicm method and the first node as a list on the opened list"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (LET (
        (node (CAR opened-list)) ;Get current node
      )
      (PROGN
        (format-node node) ;Print working node
        (IF (FUNCALL (dependency-is-solution dependencies) node) ;Verify if node to be worked is solution
          (return-data node opened-list closed-list) ;Node is the solution return data

          ;Node to be worked is not the solution so get successors and keep going
          (LET (
              ;Generate all successors (spawn-successors) and filter for only the valid successors
              (valid-successors (a*_successors-filter dependencies (spawn-successors dependencies node heuristic) opened-list closed-list)) 
            )
            ;Add the valid successors into the rest of the opened list and sort them
            (a* dependencies heuristic (a*_add-to-open dependencies valid-successors (CDR opened-list)) (CONS node closed-list))
          )
        )
      )
    )
  )
)

(DEFUN a*_successors-filter(dependencies successors opened-list closed-list)
  "Method to get the successors depending on the closed and open list, the successors cannot be on any of those"
  (LET* (
      (s-not-closed (nodes-not-on-list dependencies successors closed-list))
      (s-not-closed-and-opened (nodes-not-on-list dependencies s-not-closed opened-list))
    )
    s-not-closed-and-opened
  )
)

(DEFUN a*_remove-duplicates(dependencies nodes &optional (seen NIL))
  "Method to verify if there are duplicated (using the state) on the list of nodes given, only the first node found will stay"
  (LET (
      (node (CAR nodes))
    )
    (COND
       ((NULL nodes) NIL);No more nodes to verify
       ((node-is-member dependencies node seen) ;Verify if current node is a member of the nodes already seen
         (a*_remove-duplicates dependencies (CDR nodes) seen)
       )
       (T ;Current node is not on the seen list so goes to next iteration adding this node on the seen list
         (CONS node (a*_remove-duplicates dependencies (CDR nodes) (CONS node seen)))
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
  (a*_remove-duplicates dependencies (a*_sort-nodes dependencies (APPEND opened-list nodes)))
)
;A* Methods



;DF Methods
(DEFUN df(dependencies max-depth opened-list &optional (closed-list NIL))
  "Method to execute the breath-first algorithm, recieving the puzzle dependencies and the first node as a list on the opened list"
  (IF (NULL opened-list) ;Error if the open list is empty
    "Failure on search"
    (LET (
        (node (CAR opened-list)) ;Get current node
      )
      ;Verify if the current node depth is bigger than the max-depth to cancel
      (IF (> (FUNCALL (dependency-depth dependencies) (CAR opened-list)) max-depth)
        (bf dependencies max-depth (CDR opened-list) (CONS node closed-list)) ;Go to next iteration
        
        ;The node is not above the max depth so get successors and execute the algoithm
        (df_successors dependencies node opened-list closed-list)
      )
    )
  )
)

(DEFUN df_successors(dependencies node opened-list closed-list)
  "Method to spawn the successors and filter them with the Depth first algorithm rules"
  (LET* (
      (successors (spawn-successors dependencies node)) ;Get successors
      (s-not-opened (nodes-not-on-list dependencies successors opened-list)) ;Ignore nodes on opened list
      (s-not-closed-and-opened (nodes-not-on-list dependencies s-not-closed opened-list))0
    )
    s-not-closed-and-opened
  )
)

(DEFUN df_()
)
;DF Methods