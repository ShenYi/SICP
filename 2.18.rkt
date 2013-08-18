#lang racket
(define (reverse l)
  (define (reverse-inner l current)
    (cond ((null? l) current)
          (else (reverse-inner (cdr l) (cons (car l) current)))))
  (reverse-inner l null))
        