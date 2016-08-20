#lang sicp

;sec2.1.3
;ex2.5
;calculate a^b for positive b.
(define (power a b)
  (if (= b 0)
      1
      (* a (power a (- b 1)))))

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (car z)
  (if (= 0 (remainder z 2))
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))
