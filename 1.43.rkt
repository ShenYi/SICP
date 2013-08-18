#lang racket
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n)
  (lambda (x) 
    (if (= n 1) (f x)
        (f ((repeated f (- n 1)) x)))))

(define (repeated-iter f n)
  (if (= n 1)
      f
      (compose f (repeated-iter f (- n 1)))))