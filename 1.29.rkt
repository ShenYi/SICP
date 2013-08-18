#lang racket
(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b n)
  (define (next k)  (+ k 1))
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (factor k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          ((odd? k) 4)))
  (define (term k)
    (* (factor k) (y k)))
  (* (sum term 0 next n) (/ h 3)))
(* (integral cube 0 1 1000) 1.0)