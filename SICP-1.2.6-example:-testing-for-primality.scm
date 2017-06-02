(define (divides? a b)
	(= (remainder a b) 0))

(define (smallest-divisor n)
	(define (find-divisor n test-diviser)
		(cond ((divides? n test-diviser) test-diviser)
			  	((> (square test-diviser) n) n)
			  	(else (find-divisor n (+ test-diviser 1)))))
	(find-divisor n 2))

(define (prime? n)
	(= (smallest-divisor n) n))
