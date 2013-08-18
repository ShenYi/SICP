#lang racket
(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence) (accumulate op initial (cdr sequence))))))
(define (count-leaves t)
  (accumulate (lambda (x c) (+ x c)) 0
              (map 
               (lambda (tree) 
                 (if (pair? tree) (count-leaves tree)
                     1)) t)))
(define tree (list (list 1 (list 2 3)) (list (list 4 5) 6)))
(count-leaves tree)
                     
                     
