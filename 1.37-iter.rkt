#lang racket
(define (cont-frac n d k)
  (define (compute result current)
    (let ((nk (n current))
          (dk (d current)))
      (let ((temp (/ nk (+ dk result))))
        (if (= current 0)
          result
          (compute temp (- current 1))))))
  (compute 0 k))
