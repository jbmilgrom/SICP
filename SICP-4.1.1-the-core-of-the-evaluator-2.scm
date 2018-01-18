;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Execsize 4.6 - write a syntac transformation that transforms let into lambda expressions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-let-syntax `(let ((a 2) (b 4) (c 8)) (+ a b c)))

(define (let-definitions exp) (cadr exp))
(define (let-body exp) (caddr exp))

(define (let-parameter-names exp)
  (map car (let-definitions exp)))

(define (let-parameter-values exp)
  (map cadr (let-definitions exp)))

;;;;;; lambda data representation (constructors and selectors) from the book (which has multiple use-cases)
(define (make-lambda parameters body)
  (cons `lambda (cons parameters body)))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
;;;;;;

; translate data returned by make-lambda into valid Scheme combination
(define (lambda-data->combination data)
  (list
    (car data)
    (lambda-parameters data)
    (lambda-body data)))

; boom tho
(define (let->combination exp) ; the transformation from let->lambda
  (cons
    (lambda-data->combination (make-lambda (let-parameter-names exp) (let-body exp)))
    (let-parameter-values exp)))

; test!

(let->combination example-let-syntax); => ((lambda (a b c) (+ a b c)) 2 4 8)
