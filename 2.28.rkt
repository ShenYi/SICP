#lang racket
(define (fringe tree)
  (define (fringe-iter tree answer)
    (cond ((null? tree) answer)
          ((not (pair? (car tree))) (fringe-iter (cdr tree) (cons (car tree) answer)))
          (else (fringe-iter (cdr tree) (fringe-iter (car tree) answer)))))
  (fringe-iter tree null))
  
(define x (list (list 1 2) (list 3 4)))
