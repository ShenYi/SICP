#lang racket
(require math/number-theory)
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (if (filter a) (term a) null-value)
      (filtered-accumulate filter
                           combiner null-value term 
                           (next a) next b))))
(define (filtered-sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))
(define (filtered-product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))
(define (pa a b)
  (define (term a) a)
  (define (next a) (+ a 1))
  (filtered-sum prime? term a next b))
(define (pb n)
  (define (filter k) (= (gcd k n) 1))
  (define (term a) a)
  (define (next a) (+ a 1))
  (filtered-product filter term 1 next n))