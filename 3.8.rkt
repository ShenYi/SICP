#lang racket
(define (f x)
  (define i 0)
  (if (= i 1)
      0
      (begin
        (set! i 1)
        x)))

(+ (f 0) (f 1))