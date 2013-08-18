#lang racket
(define (square x) (* x x))

(define (tree-map operation tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (operation tree))
        (else 
         (cons
          (tree-map operation (car tree))
          (tree-map operation (cdr tree))
          ))))
(define (square-tree tree) (tree-map square tree))
(square-tree (list 1 (list 2 (list 3 4) 5 (list 6 7))))