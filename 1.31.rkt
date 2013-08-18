#lang racket
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (square x) (* x x))
(define (next x) (+ x 1))
(product square 1 next 3)