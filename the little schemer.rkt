#lang racket
(define atom?
  (lambda (x) (and (not (pair? x)) (not (null? x)))))
(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))
(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))

(define first
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) null)
          (else (cons (car (car l)) (first (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))
    