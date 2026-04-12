(define (apply_f [f : (Integer -> Boolean)] [x : Integer] [y : Integer]) : Integer
  (if (f x) x y))

(apply_f 
  (lambda: ([x : Integer]) : Boolean (eq? x 0)) 
  0 
  1)
