#lang racket
(define (make-accumulator sum)
  (lambda (x)
    (begin
      (set! sum (+ sum x))
      sum)))

(define A (make-accumulator 0))
(A 1)
(A 19)