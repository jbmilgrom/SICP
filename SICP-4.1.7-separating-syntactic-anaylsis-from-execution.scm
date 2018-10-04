(define (evaluator expression env)
  ((analyze expression) env))

(define (analyze expression)
  (cond
    ((self-evaluating? expression) (analyze-expression expression))
    ((variable? expression) (analyze-variable expression))
    ((definition? expression) (analyze-definition expression))
    ((lambda? expression) (analyze-lambda expression))
    ((application? expression) (analyze-application expression))
    (else (error "Unknown expression type -- ANALYZE"))))

(define (self-evaluating? expression) 
  (cond 
    ((number? expression) true)
    ((string? expression) true)
    (else false)))

(define (variable? expression) (symbol? expression))

(define (definition? expression) (tagged-list? expression `define))

(define (lambda? expression) (tagged-list? expression `lambda))
(define (lambda-parameters expression) (cadr expression))
(define (lambda-body expression) (cddr expression))
(define (make-lambda) (cons `lambda (cons parameters body)))

(define (application? expression) (pair? expression))

(define (analyze-expression expression)
  (lambda (env) expression))

(define (analyze-variable? expression)
  (lambda (env) (lookup-variable-value expression env)))

(define (analyze-lambda expression)
  (let 
    ((parameters (lambda-parameters expression))
      (body (lambda-body expression)))
    (lambda (env) (make-procedure parameters body env))))

(define (analyze-application expression)
  (lambda (env) ()))

(define (tagged-list? expression tag)
  (if (pair? expression)
    (eq? (car expression) tag)
    false))

(define (execute-application proc args)
  (cond 
    ((primitive-procedure? proc) (apply-primitive-procedure proc args))
    ((compound-procedure? proc)
      ((procedure-body proc)
        (extend-environment
          (procedure-parameters proc)
          args
          (procedute-environment proc))))))

(define (compound-expression? expression) (tagged-list? expression `lambda))
(define (procedure-parameters) (cadr expression))
(define (procedure-body) (caddr expression))
(define (procedure-env (cadddr expression)))
(define (make-procedure parameters body env) (list `procedure parameters body env))

; (define my-env `())

; (define (make-frame variables values)
;   (cons variables values))  

; (define (add-binding-to-frame var val frame)
;   (set-car! (cons var (car frame)))
;   (set-cdr! (cons val (cdr frame))))

