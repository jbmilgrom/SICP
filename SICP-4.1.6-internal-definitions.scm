; this works

(define (add x)
  (define a 2)
  (define b (+ a 2))
  (+ x a b))

; this errors

(define (add x)
  (define b (+ a 2))
  (define a 2)
  (+ x a b))

; this errors

(define (add x)
  (+ x a 2)
  (define a 2))

; lambda in a let? - Yes

(define (add-two x)
  (let ((add (lambda (i) (+ i 2))))
    (add x)))

; sequential reference in let? - NOPE!

(define (add-two-twice x)
  (let ((a 2) (b (+ 2 a))) ; this throws an "ubound 'a'" error however a and b definitions are ordered
    (+ b x)))

; sequential reference in letrec? - NOPE!

(define (add-two-twice x)
  (letrec ((a 2) (b (+ 2 a))) ; this throws an "ubound 'a'" error however a and b definitions are ordered
    (+ b x)))