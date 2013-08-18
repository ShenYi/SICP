#lang racket
(define (product term a next b)
  (define (product-iter a result)
  (if (> a b) result
      (product-iter (next a) (* result (term a)))))
  (product-iter a 1))
(define (square x) (* x x))
(define (next x) (+ x 1))
(product square 1 next 3)