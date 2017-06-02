;; trying to understand the need for apply

(define (weird-add . args)
	(let ((add +))
		(apply add args)))

