;;; Exercise 2.54 - implement equal?, which determines whether two lists have eq? items in equal order

;solution 1
(define (equal? p1 p2)
	(cond ((and (null? p1) (null? p2)) #t)
				((and (pair? p1) (pair? p2))
					(and (equal? (car p1) (car p2))
							 (equal? (cdr p1) (cdr p2))))
				(else (eq? p1 p2))))

; a little better tho (noting that null case can be handled by eq?)
(define (equal? p1 p2)
	(if (and (pair? p1) (pair? p2))
			(and (equal? (car p1) (car p2))
					 (equal? (cdr p1) (cdr p2)))
			(eq? p1 p2)))

(define list1 '(a b c))
(define list2 '(a (a b c) c))
