#lang sicp
(define (average a b)
  (/ (+ a b) 2))

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment l)
  (let ((start-point (start-segment l))
        (end-point (end-segment l)))
    (make-point
     (average
      (x-point start-point)
      (x-point end-point))
     (average
      (y-point start-point)
      (y-point end-point)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display " ,")
  (display (y-point p))
  (display ")"))

(define l (make-segment
           (make-point 1.0 2.0)
           (make-point 10.0 7.0)))
(print-point (midpoint-segment l))