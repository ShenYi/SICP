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

(define (tan-cf x k)
  (* (cont-frac (lambda (i) (- (* 2 i) 1))
             (lambda (i) (if (= i 1) x
                             (- (* x x))))
             k) 1.0))