(define square (x) (* x x))

(square (square 3))

(+ 2 2)
2
(car (cons 10 (cons 5 nil))) 

(if 0 13 14)
(if 4 15 16)

(define fact (N)
  (if N (* N (fact (- N 1))) 1))

(fact 10)

