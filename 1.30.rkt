#lang racket
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0)
  )
(define (cube x) (* x x x))
(define (n x) (+ x 1))

(sum cube 1 n 2)