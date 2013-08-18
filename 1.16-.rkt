#lang racket
(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((= (remainder n 2) 0) (square (fast-expt b (/ n 2))))
        (else (* b (square (fast-expt b (/ (- n 1) 2)))))))