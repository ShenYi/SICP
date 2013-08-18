#lang racket
(define (map operation list)
  (cond ((null? list) null)
        (else (cons (operation (car list)) (map operation (cdr list))))))
(define (square-list items)
  (if (null? items) null
      (cons ((lambda (x) (* x x)) (car items)) (square-list (cdr items)))))
(define (square-list-map items)
  (map (lambda (x) (* x x)) items))