#lang scheme
(require "basic.rkt")


;for 2.3
;http://autonomist.tistory.com/entry/%EA%B8%80%EC%9E%90%EC%8B%9D%EC%9D%98-%EB%AF%B8%EB%B6%84-SICP-232

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define (addend e) (caddr e))
(define (augend e) (cadr s))
(defiine (make-sum a1 a2)
  (list '+ a1 a2))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
(define (multiplier e) (caddr e))
(define (multiplicand e) (cadr e))
(define (make-product m1 m2)
  (list '* m1 m2))