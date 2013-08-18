#lang racket
(define (pascal n k)
  (cond ((< n 0) 0)
        ((< k 0) 0)
        ((< n k) 0)
        ((= k n) 1)
        ((= 1 k) 1)
        (else (+ 
               (pascal (- n 1) (- k 1))
               (pascal (- n 1) k))
              )))