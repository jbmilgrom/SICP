(apply
  (eval (operator exp) env)
  (list-of-values (operands exp) env))

; where list-of-values is

(define (list-of-values exps env)
  (if (no-operands? exp)
    `()
    (cons (eval (first-operand exps) env)
      (list-of-values (rest-operands exps) env))))

; is equivelant to
(apply
  (eval (operator exp) env)
  (map
    (lambda (operand) (eval operand env))
    (operands exp)))

; which is equivelant to
(let ((eval-in-env (lambda (operand) (eval operand env))))
  (apply
    (eval-in-env (operator exp))
    (map eval-in-env (operands exp))))

; Exercise 4.4.: implement And and Or special forms

(define (tagged-list? exp symbol)
  (if (pair? exp)
    (eq? (car exp) symbol)
    false))

(define (and? exp) (tagged-list? exp 'and))

; can't test this without `eval` defined
(define (eval-and exp env)
  (if (null? exp)
    true
    (if (eval (cadr exp) env) ; note the ca_d_r to offset the list by the 'and tag
      (eval-and exp env)
      false)))

(define (or? exp) (tagged-list? exp 'or))

; can't test this without `eval` defined
(define (eval-or exp env)
  (if (null? exp)
    false
    (if (eval (cadr exp) env) ; note the ca_d_r to offset the list by the 'or tag
      true
      (eval-or exp env))))

(define (add-from-user)
  (let ((input (read)))
    (apply + input)))

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




