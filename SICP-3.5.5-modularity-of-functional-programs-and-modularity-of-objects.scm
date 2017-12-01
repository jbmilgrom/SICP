; helpers

(define (add-streams s1 s2)
        (stream-map + s1 s2))
        
(define amount-stream (cons-stream 10 (cons-stream 15 `())))

; express style, where procedural self reference is used to build strem

(define (stream-withdraw balance amount-stream)
        (cons-stream
                balance
                (stream-withdraw
                        (- balance (stream-car amount-stream))
                        (stream-cdr amount-stream))))

(define balance (stream-withdraw 100 amount-stream))
(stream-car (stream-cdr (stream-cdr balance))) ; -> 75
(stream-ref balance 2); -> 75
(stream-ref balance 1); -> 90
(stream-ref balance 0); -> 100 

; implicit style, where stream self-reference is used to build stream --verbose

(define (make-balance-stream-verbose balance amount-stream)
        (define balance-stream 
                (cons-stream
                        balance
                        (add-streams balance-stream (stream-map - amount-stream))))
        balance-stream)

(define balance (make-balance-stream-verbose 100 amount-stream))
(stream-car (stream-cdr (stream-cdr balance))) ; -> 75
(stream-ref balance 2); -> 75
(stream-ref balance 1); -> 90
(stream-ref balance 0); -> 100 

; implicit style, where stream self-reference is used to build stream --check, please

(define (make-balance-stream balance amount-stream)
        (define balance-stream 
                (cons-stream
                        balance
                        (stream-map - balance-stream amount-stream)))
        balance-stream)

(define balance (make-balance-stream 100 amount-stream))
(stream-car (stream-cdr (stream-cdr balance))) ; -> 75
(stream-ref balance 2); -> 75
(stream-ref balance 1); -> 90
(stream-ref balance 0); -> 100 
