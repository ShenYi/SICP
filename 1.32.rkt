#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (ac-iter a result)
    (if (> a b) result
        (ac-iter (next a) (combiner result (term a)))))
  (ac-iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))