#lang racket
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated-iter f n)
  (if (= n 1)
      f
      (compose f (repeated-iter f (- n 1)))))
(define dx 0.0000001)
(define (smooth f)
  (lambda (x) (/ (f (- x dx)) (f x) (f (+ x dx)))))
(define (smooth-repeated f n)
  (repeated-iter (smooth f) n))