#lang planet neil/sicp
(define (search-for-primes start count)
  (cond ((= count 0) )
        ((timed-prime-test start) (search-for-primes (+ start 1) (- count 1)))
        (else (search-for-primes (+ start 1) count))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
         (report-prime n (- (runtime) start-time))
         #t)
        (else #f)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (newline)
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))