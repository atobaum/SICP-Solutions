#lang scheme
(require "lib/basic.rkt")

(provide make-vect)
(provide xcor-vect)
(provide ycor-vect)
(provide add-vect)
(provede sub-vect)
(provide scale-vect)

(provide make-frame)
(provide origin-frame)
(provide edge1-frame)
(provide edge2-frame)

;ex-2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cadr)
(define (add-vect vec1 vec2)
  (make-vect (+ (xcor-vect vec1) (xcor-vect vec2))
             (+ (ycor-vect vec1) (ycor-vect vec2))))
(define (sub-vect vec1 vec2)
  (make-vect (- (xcor-vect vec1) (xcor-vect vec2))
             (- (ycor-vect vec1) (ycor-vect vec2))))
(define (scale-vect scalar vec)
  (make-vect (* scalar (xcor-vect vec))
             (* scalar (ycor-vect vec))))

;ex-2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))