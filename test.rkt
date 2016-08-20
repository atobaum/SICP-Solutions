#lang racket
(define nil null)

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

(safe? board3)