(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

;;;;;;;;; 2.1.4 Extended Exercise

(define (print-interval interval)
	(newline)
	(display (lower-bound interval))
	(display "--")
	(display (upper-bound interval)))

;;; Exercise 2.7 ; Note how all public (non constructor) methods expect an input of type interval

; the "interval" constructor

(define (make-interval a b) (cons a b))

; the "interval" selectors

(define (lower-bound pair) (car pair)) ; first value: a

(define (upper-bound pair) (cdr pair)) ; second value: b

; some test variables

(define three-four (make-interval 3 4))
(define two-three (make-interval 2 3))

;;; Exercise 2.8 - subtraction procedure

(define (sub-interval x y) ; expects inputs of type interval
	(make-interval (- (lower-bound x) (lower-bound y))
				   (- (upper-bound x) (upper-bound y))))

;;; Exercise 2.12 - make-center-percent interval and selectors

(define (decimal percent) (* percent (/ 1 100)))
(define (percent decimal) (* decimal 100.0))
(define (percent-from-rat numerator denomenator)
  (percent (abs (/ numerator denomenator))))

;; The constructor
(define (make-center-percent c p)
	(let ((width (* (decimal p) c)))
		(make-interval (+ c width) (- c width))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2.0))

; The selector
(define (percentage-tolerance i) ; expects inputs of type interval
  (percent-from-rat (width i) (center i)))

; Tests
(define eight-off-by-ten (make-center-percent 8 10))

(define ten-off-by-ten (make-center-percent 10 10))

