#lang racket
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define f
  (lambda (x) (+ x 1)))
(define (add-church a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define (multiply-church a b)
  (lambda (f) (lambda (x) ((a (b f)) x))))
(define (expt a b)
  (lambda (f) (lambda (x) (((b a) f) x))))
(define three
  (lambda (f) (lambda (x) (f (f (f x))))))