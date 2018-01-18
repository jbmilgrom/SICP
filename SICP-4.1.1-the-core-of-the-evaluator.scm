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



