(define (sum [arr : (Array Integer)] [len : Integer]) : Integer
  (let ([sum 0])
    (let ([index 0])
      (begin
        (while (< index len)
          (begin
            (set! sum (+ sum (array-ref arr index)))
            (set! index (+ index 1))))
        sum))))

(let ([arr (array 3 10)])
  (sum arr 3))
