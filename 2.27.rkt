#lang racket
(define (deep-reverse l)
  (define (deep-reverse-inner l current)
  (cond ((null? l) current)
        ((not (pair? (car l))) (deep-reverse-inner (cdr l) (cons (car l) current)))
        (else (deep-reverse-inner (cdr l) (cons (deep-reverse-inner (car l) null) current)))))
  (deep-reverse-inner l null))
(define x (list (list 1 2) (list 3 4)))
