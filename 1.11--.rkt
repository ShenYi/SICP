#lang racket
(define (coin-map kinds)
  (cond ((= kinds 1) 1)
        ((= kinds 2) 5)
        ((= kinds 3) 10)
        ((= kinds 4) 25)
        ((= kinds 5) 50)))

(define (coin-count-inner amount kinds n)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((= kinds 0) 0)
        