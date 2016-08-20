#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define dx (/ (- b a) n))
  (define (add-dx a)
    (+ a dx))
  (* (+ (sum f (+ a (/ dx 2.0)) add-dx b))
     dx))


(define (integral-Simpson f a b n)
  (define dx (/ (- b a) n))
  (define (get-x n)
    (+ a
       (* dx n)))
  (define (term n)
    (if (= (remainder n 2) 0)
        (* 4 (f (get-X n)))
        (* 2 (f (get-X n)))))
  (define (inc a)
    (+ a 1))
  (* (+ (f a) (sum term 1 inc (- n 1)) (f b))
     (/ dx 3.0)))

(define (cube x)
  (* x x x))

(integral cube 0 1 1000)

(integral-Simpson cube 0 1 1000)