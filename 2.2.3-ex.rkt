#lang scheme
(require "lib/basic.rkt")
;ex-2.2.3
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

;2.33
(define (cmap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (inc y)) 0 seq))

;2.34
(define (honer-eval x coef)
  (accumulate (lambda (this-coef higher-terms) (+ this-coef (* x higher-terms)))
              0
              coef))

;2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (cond
                           ((not (pair? x)) 1)
                           (else (count-leaves x)))) t)))

;2.36
(define test-seqs (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))
(define (first-elements seqs)
  (accumulate (lambda (x y) (cons (car x) y)) nil seqs))

(define (rest-elements seqs)
  (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (first-elements seqs))
            (accumulate-n op init (rest-elements seqs)))))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

;test
(define matrix (list '(1 2 3) '(4 5 6) '(7 8 9)))
(define vector '(1 2 3))
(dot-product vector vector)
(matrix-*-vector matrix vector)
(transpose matrix)
(matrix-*-matrix matrix matrix)


;2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result  (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

;2.39
(define (reverse1 seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

;test
(reverse1 '(1 2 3 4))
(reverse2 '(1 2 3 4))

;2.40
;see 겹친 매핑 in 2.2.3.rkt

;2.41
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (add-one-element-in-front-of-list items)
  (flatmap (lambda (li)
             (let ((a (car li)))
               (map (lambda (x) (cons x li)) (enumerate-interval 1 a))))
           items))
               
(define (triple-sum n s)
  (filter (lambda (li) (= s (+ (car li) (cadr li) (caddr li))))
          (add-one-element-in-front-of-list
           (add-one-element-in-front-of-list
            (map (lambda (x) (list x)) (enumerate-interval 1 n))))))
(triple-sum 9 15)

;2.42
#|(define test-board (list '(#t #f #f) '(#f #f #f) '(#f #f #t)))
(define test-boards (list test-board test-board test-board))
(define (print-boards boards)
    (define (print-board board)
      (if (null? board)
          #t
          (let ()
            (display (map (lambda (x) (if x 1 0)) (car board)))
            (newline)
            (print-board (cdr board)))))
  
    (if (null? boards)
        #t
        (let ()
          (newline)
          (print-board (car boards))
          (print-boards (cdr boards)))))

(define empty-board (list ))

(define (safe? k position)
   ;難しい。
  (define (and-list list1 list2)
    ;do and operation component-wise and return #t iff there is a #t in result list.
    (accumulate (lambda (x y) (or x y)) #f (map (lambda (x y) (and x y)) list1 list2)))
    #|(cond ((null? list1) #f)
          ((and (car list1) (car list2)) #t)
          (else (and-list (cdr list1) (cdr list2)))))|#
  (define (shift-left li)
    (append (cdr li) '(#f)))
  (define (shift-right li)
    (define (delete-last li)
      (if (null? (cddr li))
          (list (car li))
          (cons (car li) (delete-last (cdr li)))))
    (cons #f
          (delete-last li)))

  (define (transform-kth-row k position)
    (define (iter i items)
      (if (= i (- k 1))
          nil
          (cons items
                (iter (+ i 1)
                      (list (shift-left (car items)) (cadr items) (shift-right (caddr items)))))))
    (iter 0 (list (car position) (car position) (car position))))

  (not (accumulate (lambda (x y) (or x y)) #f
              (map (lambda (x) (or (car x) (cadr x) (caddr x)))
                          (map (lambda (kth-rows row)
                                 (map (lambda (kth-row) (and-list kth-row row)) kth-rows))
                               (transform-kth-row k position)
                               (cdr position))))))

(define (adjoin-position new-row k rest-of-queens)
  (define (make-new-row i)
    ;this function  makes new row that has queen in i-th position.
    (define (iter j result)
      (cond ((= j 0) result)
            ((= j i) (iter (- j 1) (cons #t result)))
            (else (iter (- j 1) (cons #f result)))))
    (iter k nil))
  (cons (make-new-row new-row) rest-of-queens))
|#

;board representation: first element is the board size. rest are location of the queens.
(define board1 (list 3 '(1 1) '(2 3) '(3 2)))
(define board2 (list 3 '(1 3) '(2 2) '(3 2)))
(define board3 (list 4 '(1 2) '(2 4) '(3 1) '(4 3)))
(define test-boards (list board1 board2))

(define empty-board (list 0))

(define (print-board board)
  (define (ith-element i li)
    (if (= i 1)
        (car li)
        (ith-element (- i 1) (cdr li))))
  
  (define (convert-board board)
    (let ((size (car board))
          (locations (cdr board)))
      (define (make-row i k)
          (define (iter i j)
            (cond ((> j k) nil)
                  ((= i j) (cons "■ " (iter i (+ j 1))))
                  (else (cons "□ " (iter i (+ j 1))))))
          (apply string-append (iter i 1)))
      
      (define (empty-board)
        (define (helper i)
          (if (= i 0)
              nil
              (cons (make-row (+ 1 size) size) (helper (- i 1)))))
        (helper size))          
        
      (define (iter locations board)
        (if (null? locations)
            board
            (let ((y (caar locations))
                  (x (cadar locations)))
              (define (change-row row board)
                (cond ((null? board) board)
                      ((= row y) (cons (make-row x size) (change-row (+ row 1) (cdr board))))
                      (else (cons (car board) (change-row (+ row 1) (cdr board))))))
              (iter (cdr locations) (change-row 1 board)))))
      (iter locations (empty-board))))

  (define (print-converted-board converted-board)
    (if (null? converted-board)
        #t
        (let ()
          (display (car converted-board))
          (newline)
          (print-converted-board (cdr converted-board)))))

  (print-converted-board (convert-board board)))

(define (print-boards boards)
  (if (null? boards)
      #f
      (let ()
        (newline)
        (print-board (car boards))
        (print-boards (cdr boards)))))

(define (safe? board)
  (if (null? (cddr board))
      #t
      (let ((size (car board))
            (y (car (cadr board)))
            (x (cadr (cadr board)))
            (rest-y (car (caddr board)))
            (rest-x (cadr (caddr board))))
        (cond ((= x rest-x) #f) ;verticla
          ((= (+ x y) (+ rest-x rest-y)) #f ) ;upper right
          ((= (- x y) (- rest-x rest-y)) #f ) ;upper left
          (else (safe? (cons size (cons (list y x) (cdddr board)))))))))

(define (adjoin-position new-row k rest-of-queens)
  (let ((locations (cdr rest-of-queens)))
    (cons k (cons (list k new-row) (cdr rest-of-queens)))))
  
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 (+ 1 board-size))))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;test
(print-boards (queens 6))

;2.43
(define (queens1 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 (+ 1 board-size))))))
  (queen-cols board-size))

;each time evauates the loop of flatmap, program re-calculate (queens-cols (- k 1)) board-size times for each k..
;on the other hand, in 2.42, program only needs to calculate (queens-cols (- k 1)) once for each k.

;2016/8/21