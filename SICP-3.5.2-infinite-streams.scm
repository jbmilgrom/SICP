(define (add-streams s1 s2)
        (stream-map + s1 s2))

; implicit fibonacci infite series
(define fib
        (cons-stream 0
                (cons-stream 1
                        (add-streams fib (stream-cdr fib)))))

(define fib-expanded
        (begin (display "shits been called \n") ; only gets called once!
                (cons-stream 0
                        (cons-stream 1
                                (add-streams
                                        (cons-stream 0
                                                (cons-stream 1
                                                        (add-streams fib-expanded (stream-cdr fib-expanded))))
                                        (cons-stream 1
                                                (add-streams fib-expanded (stream-cdr fib-expanded))))))))

; explicit generator of fibonacci infinite series
(define (fib-generator a b)
        (cons-stream a (fib-generator b (+ a b))))

(define (display-stream s iterations)
        (if (> iterations 0)
                (begin
                        (display "\n")
                        (display (stream-car s))
                        (display-stream (stream-cdr s) (- iterations 1)))))


(define what-this-done
        (cons-stream 0
                (cons-stream 1
                        (add-streams what-this-done what-this-done))))