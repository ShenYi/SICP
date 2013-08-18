#lang racket
(define (average x y) (/ (+ x y) 2))
(define (cont-frac n d k)
  (define (compute current)
    (let ((nk (n current))
          (dk (d current)))
    (if (= current k)
        (/ nk dk)
        (/ nk (+ dk (compute (+ current 1)))))))
  (compute 1))
               
           