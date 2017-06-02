(define (abs x)
	(cond ((> x 0) x)
		    ((= x 0) 0)
        ((< x 0) (- x))))

; (display (abs (- 9)))

; Section 1.1.7 - Find the square root by successively improving guess

(define (average x y)
  (/ (+ x y) 2))

(define (nextguess x prev)
  (average (/ x prev) prev))

(define (good-enough? x guess)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-er x guess)
  (if (good-enough? x guess)
      guess
      (sqrt-er x (nextguess x guess))))

(define (sqrt x)
  (sqrt-er x 1.0))

; (display "\n")
; (display (sqrt 2))
; (display "\n")
; (display (sqrt 9))
; (display "\n")
; (display (sqrt 11))

; FAST exponentiation with a recursive process

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(display "\n")
(display (fast-exp 2 3))
(display "\n")
(display (fast-exp 2 4))
(display "\n")
(display (fast-exp 2 5))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m)
                    m)))))

; my

; (display "\n")
; (display (expmod 3 3 2))
; (display "\n")
; (display (expmod 3 4 2))

; Structure and Interpretation of Computer Programs

(define (inc n) (+ n 1))
(define (indentity x) x)

; Exercise 1.31

(define (product-series fn a next b)
  (if (= a b)
    (fn b)
    (* (fn a) (product-series fn (next a) next b))))

(define (factorial n)
  (product-series indentity 1 inc n))

(display "\n#### Exercise 1.31 ####")
(display "\nFactorial 3: ")(display (factorial 3))
(display "\nFactorial 4: ")(display (factorial 4))
(display "\nFactorial 6: ")(display (factorial 6))

; ; Turn the above recursive process into an iterative one

(define (product-series-iterative fn a next b product)
  (if (= a b)
    (* (fn b) product)
    (product-series-iterative fn (next a) next b (* (fn a) product))))

(define (factorial-iterative n)
  (product-series-iterative indentity 1 inc n 1))

(display "\n#### Exercise 1.31 ####")
(display "\n factorial-iterative 3: ")(display (factorial-iterative 3))
(display "\n factorial-iterative 4: ")(display (factorial-iterative 4))
(display "\n factorial-iterative 6: ")(display (factorial-iterative 6))
