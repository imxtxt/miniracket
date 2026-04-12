(define (fact [x : Integer]) : Integer
  (let ([acc 1])
    (begin
      (while (> x 0)
        (begin
          (set! acc (* acc x))
          (set! x (- x 1))))
      acc)))

(fact 5)