(define (fermat n)
  (= (modulo-expt (+ 1 (random (- n 2))) (- n 1) n) 1))

(define (prime? n tests)
  (and-map (lambda (x) (ferma n)) (iota tests)))

(define (pov a n)
    (let loop ((a a) (n n) (acc 1))
      (if (= n 0) acc
        (loop a (- n 1) (* a acc)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length1 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
  0
  coefficient-sequence))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (transpose1 mat)
  (accumulate-n cons '() mat))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
