#lang sicp
;ex2.2.2

;2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

(car (car '((7))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;2.27
(define x1 (list (list 1 2) (list 3 4)))
(define (deep-reverse tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) tree)
        (else (append (deep-reverse (cdr tree)) (list (deep-reverse (car tree)))))))

(deep-reverse x1)

;2.28
(define (fringe tree)
  (define (print-one x)
    (display " ")
    (display x))
  (cond ((null? tree) )
        ((not (pair? tree)) (print-one tree))
        (else (fringe (car tree))
              (fringe (cdr tree)))))

;2.31
(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

(square-tree x1)

;2.32
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;20160817