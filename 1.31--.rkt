#lang racket
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (f x) x)
(define (next x) (+ x 1))
(define (factorial n)
  (cond ((< n 0) 0)
        ((< n 2) 1)
        (else (product f 1 next n))))
(factorial 4)