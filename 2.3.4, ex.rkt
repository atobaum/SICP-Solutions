#lang scheme
(require "lib/basic.rkt")

;2.3.4
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? 'leaf (car object)))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))



(define pairs '((A 4) (B 2) (C 1) (D 1)))

;ex-2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;ex-2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (contain-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (contain-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (encode-1 symbol tree)
    (cond ((leaf? tree) '())
          ((contain-set? symbol (symbols (left-branch tree)))
           (cons '0 (encode-1 symbol (left-branch tree))))
          (else (cons '1 (encode-1 symbol (right-branch tree))))))
  
  (if (not (contain-set? symbol (symbols tree)))
      (let ()
        (error "tree does not contain given symbol - ")
        (newline)
        (display symbol)
        (newline)
        (display tree))
      (encode-1 symbol tree)))

;ex-2.69
(define (generate-huffman-tree pairs)
  (define (successive-merge pairs)
    (if (= (length pairs) 1)
        (car pairs)
        (let ((first (car pairs))
              (second (cadr pairs))
              (rest (cddr pairs)))
          (successive-merge (adjoin-set (make-code-tree first second) rest)))))
  (successive-merge (make-leaf-set pairs)))

;ex-2.70
(define rock-words '((A 1) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define rock-tree (generate-huffman-tree rock-words))
(define rock-lyrics '(GET A JOB
                          SHA NA NA NA NA NA NA NA NA
                          GET A JOB
                          SHA NA NA NA NA NA NA NA NA
                          WAH YIP YIP YIP YIP YIP YIP YIP YIP
                          SHA BOOM))
(decode (encode rock-lyrics rock-tree) rock-tree)