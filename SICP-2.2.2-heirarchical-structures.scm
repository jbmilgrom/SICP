(define nil '())

;;; Exercise 2.25 - a bunch of cons/list practice

(define list-one (cons 1 (cons 3 (cons (cons 5 (cons 7 nil)) (cons 9 nil)))))
; (define list-one (list 1 3 (list 5 7) 9))
; (1 3 (5 7) 9)

(define list-two (cons (cons 7 nil) nil))
; (define list-two (list (list 7)))
; ((7))

;;; Exercise 2.26 - a bunch of list shit

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)

(cons x y)
; ((1 2 3) 4 6 7)

(list x y)
; ((1 2 3) (4 6 7))

;;; Mapping over trees

(define (map proc items)
	(if (null? items)
		items
		(cons (proc (car items)) (map proc (cdr items)))))

(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; i feel like (pair? tree) is the wrong check, writing this method to check
(define (scale-tree tree factor)
	(map (lambda (sub-tree)
		(if (pair? sub-tree)
			(scale-tree sub-tree factor)
			(* sub-tree factor)))
	tree))

;;; Exercise 2.32 - The set of all subsets

; (define (subsets set)
; 	(if (null? set)
; 		(list set)
; 		(let ((rest (subsets (cdr set))))
; 			(append rest (map [] rest)))))

; (subsets (list 1 2 3))
