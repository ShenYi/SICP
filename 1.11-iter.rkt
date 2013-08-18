#lang racket
(define (f-iter first second third k)
  (cond ((= k 0) (+ third (* 2 second) (* 3 first)))
        (else (f-iter second third (+ third (* 2 second) (* 3 first)) (- k 1)))))
            
(define (f n)
  (cond ((< n 3) n)
        (else (f-iter 0 1 2 (- n 3)))))