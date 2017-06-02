;; makin an accumulator

(define (make-accumulator start)
	(lambda (addition)
		(set! start (+ start addition))
		start))

(define my-a (make-accumulator 5))

(my-a 10) ; 15

(my-a 15) ; 30
