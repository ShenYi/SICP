#lang racket
(define atom?
  (lambda (element) (not (pair? element))))
(define (equal? l1 l2)
  (cond ((and (atom? l1) (atom? l2)) (eq? l1 l2))
        ((or
          (and (atom? l1) (pair? l2))
          (and (pair? l1) (atom? l2)))
         #f)
        (else
         (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))))
(define a1 'a)
(define a2 'a)
(define a3 'b)
(define l1 '(r s t))
(define l2 '(p 1))
(define l3 '(r s t))
(equal? a1 a2)
(equal? a1 a3)
(equal? a1 l1)
(equal? l1 l2)
(equal? l1 l3)