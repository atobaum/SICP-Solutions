#lang scheme
(require "lib/basic.rkt")
(require sicp-pict)
;ex-2.2.4

;2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;2.45
(define (split first second)
  (define (inner painter n)
    (if (= n 0) painter
        (let ((smaller (inner painter (- n 1))))
          (first painter (second smaller smaller)))))
  (lambda (painter n)
    (inner painter n)))

(define right-split (split beside below))
(define up-split1 (split below beside))

;2.46
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

;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))


(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame2 frame)
  (car frame))
(define (edge1-frame2 frame)
  (cadr frame))
(define (edge2-frame2 frame)
  (cddr frame))