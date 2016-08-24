#lang sicp
(require "basic.rkt")

;for 2.3
(provide variable?)
(provide same-variable?)
(provide sum?)
(provide addend)
(provide augend)
(provide make-sum)
(provide product?)
(provide multiplier)
(provide multiplicand)
(provide make-product)


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define (addend e) (caddr e))
(define (augend e) (cadr e))
(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
(define (multiplier e) (caddr e))
(define (multiplicand e) (cadr e))
(define (make-product m1 m2)
  (list '* m1 m2))