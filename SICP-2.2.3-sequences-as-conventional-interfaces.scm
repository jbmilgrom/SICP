(define one-four (list 1 2 3 4))
(define nil '())
(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (map proc items)
	(if (null? items)
		items
		(cons (proc (car items)) (map proc (cdr items)))))
(define (accumulate op initial sequence)
	(if (null? sequence)
	initial
	(op (car sequence)
			(accumulate op initial (cdr sequence)))))
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

; calling is sequence bc the book does, but this would work for any (unordered) list

(define (filter predicate sequence)
	(cond ((null? sequence) sequence)
				((predicate (car sequence))
					(cons (car sequence) (filter predicate (cdr sequence))))
				(else (filter predicate (cdr sequence)))))

;;; Exercise 2.35 - Redefine count leaves as an accumulation

; one way, kinda gross, that recursively calls count-leaves (and accumulate) every sub-tree
(define (count-leaves tree)
	(accumulate
		+
		0
		(map
			(lambda (sub-tree)
				(if (pair? sub-tree)
					(count-leaves sub-tree)
					1))
			tree)))

;; probably better to enumerate tree (recursively), prior to accumulating ;;

; append (although this there is a native implementation by same name)
(define (append items1 items2)
	(if (null? items1)
		items2
		(cons (car items1) (append (cdr items1) items2))))

; could also be called flatten
(define (enumerate-tree tree)
	(cond ((null? tree) tree)
				; Dealing with the "leaf nodes"
				((not (pair? tree)) (list tree))
				; OR
				; ((not (pair? (car tree)))
				; 	(cons (car tree) (enumerate-tree (cdr tree))))
				; OR
				; ((not (pair? (car tree)))
				; 	(append (list (car tree)) (enumerate-tree (cdr tree))))
				; Notice how the second and third choices duplicate logic that is already
				; present in the "non leaf node" logic below
				(else (append (enumerate-tree (car tree))
											(enumerate-tree (cdr tree))))))

; ahh much nicer
(define (count-leaves tree)
	(accumulate + 0 (map (lambda (x) 1)
										 	 (enumerate-tree tree))))

;;; Nested Mappings ;;;

(define (enumerate-interval start finish)
	(if (> start finish)
		nil
		(cons start (enumerate-interval (+ start 1) finish))))

(define (flatmap proc sequence)
	(accumulate append nil (map proc sequence)))

(define (combinations-of-postive-integers n)
	(flatmap
		(lambda (i)
			(map (lambda (j) (list i j))
					 (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; Find all pairs of integers less than n that sum to a prime number

(define (prime-sum-couples-below n)
	(map make-pair-sum
			 (filter prime-sum? (combinations-of-postive-integers n))))

;; Find all permutations of a set s

; this will remove all items that are equal to x
(define (remove item sequence)
	(filter (lambda (x) (not (= x item)))
					sequence))

(define (permutations s)
	(if (null? s)
			(list s)
			(map (lambda (x)
							 (map (lambda (p) (cons x p))
										 (permutations (remove x s))))
							 s)))

;; copy list

(define (copy-list items)
	(accumulate cons nil items))

(define (copy-list-2 items)
	(accumulate append nil (map list items)))


