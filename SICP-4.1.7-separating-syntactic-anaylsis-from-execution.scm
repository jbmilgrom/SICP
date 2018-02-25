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

(define (application? expression) (pair? expression))

(define (analyze-expression expression)
  (lambda (env) expression))

(define (analyze-variable? expression)
  (lambda (env) (lookup-variable-value expression env)))

(define (analyze-lambda expression)
  ())

(define (analyze-application expression)
  ())

(define (tagged-list? expression tag)
  (if (pair? expression)
    (eq? (car expression) tag)
    false))


; (define my-env `())

; (define (make-frame variables values)
;   (cons variables values))

; (define (add-binding-to-frame var val frame)
;   (set-car! (cons var (car frame)))
;   (set=-cdr! (cons val (cdr frame))))

