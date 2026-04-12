(let ([arr (array 3 10)])
  (begin
    (array-set! arr 0 24)
    (array-ref arr 0)))
