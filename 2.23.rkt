#lang racket
(define (for-each operation items)
  (cond ((not (null? items)) 
         (operation (car items)) (for-each operation (cdr items)))))
(define x (list 1 2 3))
(define y (list 4 5 6))
(cons x y)