#lang scheme
(require "lib/basic.rkt")
(require sicp-pict)

;2.2.4
(define ein einstein)
(define ein2
  (beside ein (flip-vert ein)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
              (beside (below painter top-left)
                      (below bottom-right corner))))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below (beside (bl painter) (br painter))
           (beside (tl painter) (tr painter)))))

;test
(paint ((square-of-four flip-vert identity identity flip-vert) ein))

(define (flipped-pairs painter)
  ((square-of-four flip-horiz identity rotate180 flip-vert) painter))

(define (square-limit painter n)
  ((square-of-four flip-horiz identity
                   rotate180 flip-vert)
   (corner-split painter n)))

;test
(paint (square-limit ein 5))


(define (frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v)
                           (frame-edge1 frame))
               (vector-scale (vector-ycor v)
                           (frame-edge2 frame))))))

