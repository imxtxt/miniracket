(define (tuple_sum [arg : (Vector Integer Integer Integer)]) : Integer
  (+ (vector-ref arg 0)
     (+ (vector-ref arg 1)
        (vector-ref arg 2))))

(tuple_sum (vector 1 2 3))