#lang sicp

#|excercise 1.37 continued fraction |#
#| a |#
(define (cont-frac-rec n d k)
  (define (temp-cont-frac n d i last)
    (if (= i last)
        (/ (n last) (d last))
        (/ (n i) (+ (d i) (temp-cont-frac n d (+ 1 i) last)))))
  (temp-cont-frac n d 1 k))

(newline)
(display "1.37.a")
(newline)
(/ 1.0 (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 20))

#| b |#
(define (cont-frac-iter n d k)
  (define (temp-cont-frac n d i value)
    (if (= i 1)
        (/ (n i) (+ value (d i)))
        (temp-cont-frac n d (- i 1) (/ (n i) (+ value (d i))))))
  (temp-cont-frac n d k 0))

(newline)
(display "1.37.b")
(newline)
(/ 1.0 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 20))

#|excercise 1.39 |#
#;not-working
(newline)
(newline)
(display "1.39")
(newline)

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (* x x -1)))
  (cont-frac-iter n (lambda (i) (+ (* 2 i) 1)) k))


;1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))
(define (square x)
  (* x x))


(display "
1.41
")
(((double (double double)) inc) 5)

;1.42
(display "
1.42
")
((compose square inc) 6)

;1.43
(display "
1.43
")

(define (repeat f n)
  (if (<= n 1)
      f
      (repeat (double f) (- n 1))))

((repeat square 2) 5)

;1.44
(display "
1.44
")
(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define (n-smooth f n)
  ((repeat smooth n) f))