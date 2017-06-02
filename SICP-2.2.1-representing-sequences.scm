; lambda syntax, since it has proven elusive

((lambda (a b) (+ a b)) 2 2)
; 4

;;; Exercise 2.17 - last item in list

(define one-four (list 1 2 3 4))
; the same as (cons 1 (cons 2 (cons 3 (cons 4 '()))))
(define nil '())

; with build-ins

(define (last items)
	(list-ref items (- (length items) 1)))

; iteratively, from scratch

(define (last2 items)
	(if (null? (cdr items))
		(car items)
		(last2 (cdr items))))

; iteratively, from scratch

(define (last3 items)
	(define (last-er items last)
		(if (null? items)
			last
			(last-er (cdr items) (car items))))
	(last-er items (car items)))

;;; Exercise 2.18 - reverse list

(define (reverse-iter items)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
  (iter items nil))

; (define (reverse-recurs items)
; 	(if (null? items)
; 		items
; 		(append (reverse-recurs (cdr items)) (list (car items)))))

;;; Exercise 2.21 - implement square-list

(define (map proc items)
	(if (null? items)
		items
		(cons (proc (car items)) (map proc (cdr items)))))

(define (square-list items)
	(map (lambda (x) (* x x)) items))
