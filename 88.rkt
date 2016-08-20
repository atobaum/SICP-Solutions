#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.001))
  (define (try guess)
    (let ((next (f guess)))
      #;(display guess)
      #;(newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;excercise 1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

#;excercise 1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)