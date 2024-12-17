(IN-PACKAGE :branching)

(DEFUN call(generated-nodes depth-solution)
  (bisection 'operator 0 generated-nodes depth-solution generated-nodes)
)

(DEFUN bisection(f a b l total &optional (e 0.01))
  "F - function; a - [; b - ], e - error;"
  (LET* (
      (c (/ (+ a b) 2)) ;New middle point
      (aux (/ (- b a) 2)) ;Aux
      (fc (FUNCALL f c l total)) ;f(c)
      (fa (FUNCALL f a l total)) ;f(a)
    )
    (COND
      ((OR (= fc 0) (< aux e)) c) ;Solution
      ((= (fsignal fc) (fsignal fa)) (bisection f c b l total e)) ;a <- c
      (T (bisection f a c l total e)) ;b <- c
    )
  )
  
)

(DEFUN fsignal(n)
  (IF (< n 0) -1 1)
)

(DEFUN operator(b L total)
  "B^1 + B^2 + B^3 ... + B^L = T"
  (COND
    ((> l 1) ;If we are not on the last iteration we call the l - 1 and get b^L (current L)
      (+ (expt b L) (operator b (1- L) total))
    )
    ((>= l 1) ;When we are on the last L (1) we use the total
      (- b total)
    )

  )
)