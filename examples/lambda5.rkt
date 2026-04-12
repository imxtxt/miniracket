(let ([x 0])
  (let ([y 0])
    (let ([z 20])
      (let ([f (lambda: ([a : Integer]) : Integer (+ a (+ x z)))])
        (begin
          (set! x 10)
          (set! y 20)
          (f y))))))
