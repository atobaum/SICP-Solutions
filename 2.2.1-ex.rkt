#lang sicp
;ex2.2.1

(define test-list (list 23 241 59 1 83 4))

;2.17
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (last-pair items)
  (if (= (length items) 1)
      items
      (last-pair (cdr items))))

;2.18
(define (reverse items)
  (define (reverse-temp list1 list2)
    (if (null? list1)
        list2
        (reverse-temp (cdr list1) (cons (car list1) list2))))
  (reverse-temp items nil))

;2.19
(define (same-parity x . y)
  (define (append-temp list1 list2 parity)
    (if (null? list1)
        list2
        (if (= (remainder (car list1) 2) parity)
            (cons (car list1) (append-temp (cdr list1) list2 parity))
            (append-temp (cdr list1) list2 parity))))
  (if (= (remainder x 2) 0)
     (append-temp (cons x y) nil 0)
     (append-temp (cons x y) nil 1)))


;2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons ((lambda (x) (* x x)) (car things))
                    answer))))
  (iter items nil))


;2.23
(define (for-each proc items)
  (if (not (null? items))
      (let () (proc (car items))
       (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x)) test-list)