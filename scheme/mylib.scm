(define (pov a n)
    (let loop ((a a) (n n) (acc 1))
      (if (= n 0) acc
        (loop a (- n 1) (* a acc)))))
