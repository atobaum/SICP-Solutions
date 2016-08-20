#lang scheme
(require "lib/basic.rkt")
;2.2.3

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (< low high)
      (cons low (enumerate-interval (+ low 1) high))
      nil))

;겹친 매핑

(define (enumerate-2d-interval1 n)
  (accumulate append
              nil
              (map (lambda (x)
                     (accumulate (lambda (y items) (cons (list y x) items))
                                 nil
                                 (enumerate-interval 1 x)))
                   (enumerate-interval 1 n))))

(define (enumerate-2d-interval n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 i)))
                   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (append pair (list (+ (car pair) (cadr pair)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (enumerate-2d-interval n))))

(define (remove seq item)
  (filter (lambda (x) (not (= item x))) seq))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x) (map (lambda (y) (cons x y))
                                (permutations (remove s x))))
               s)))

;2016/8/32