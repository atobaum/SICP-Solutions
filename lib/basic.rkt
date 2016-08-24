#lang scheme
;basic
(provide nil)

;math
(provide power)
(provide average)
(provide inc)
(provide dec)

(provide prime?)


(provide cmap)

;basic
(define nil null)

(define (error x . y)
  (define (iter x . y)
    (if (not (null? x))
        (let ()
          (display x)
          (newline)
          (iter (car y) (cdr y)))
        (newline)))
  (display "ERROR!!!: ")
  (iter x y))

;math
;calculate a^b for positive b.
(define (power a b)
  (if (= b 0)
      1
      (* a (power a (- b 1)))))

(define (average a b)
  (/ (+ a b) 2))

(define inc (lambda (x) (+ x 1)))
(define dec (lambda (x) (- x 1)))


(define (prime? x)
  (define (iter i x)
    (cond ((< x (* i i)) #t)
          ((= 0 (remainder x i)) #f)
          (else (iter (+ i 2) x))))
  (if (= 0 (remainder x 2))
      #f
      (iter 3 x)))



;custom map
(define (cmap proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))