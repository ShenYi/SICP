#lang racket
(define (last-pair l)
  (cond ((null? l) null)
        ((null? (cdr l)) (list (car l)))
        (else (last-pair (cdr l)))))