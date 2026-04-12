(define (add [arg1 : Integer] [arg2 : Integer]) : Integer
  (+ arg1 arg2))

(define (mul [arg1 : Integer] [arg2 : Integer]) : Integer
  (* arg1 arg2))

(+ (add 10 20) (mul 2 3))
