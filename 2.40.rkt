#lang racket
(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence) (accumulate op initial (cdr sequence))))))
(define (enumerate n)
  (define (iter current result)
    (cond ((< current 1) result)
          (else (iter (- current 1) (cons current result)))))
  (iter n null))
(define (unique-pairs n)
  (accumulate append null
              (map (lambda (i) (map (lambda (j) (list i j)) (enumerate (- i 1)))) (enumerate n))))
(define (unique-tribles n)
  (accumulate append null
              (map (lambda (i) (map (lambda (pair) (cons i pair)) (unique-pairs (- i 1)))) (enumerate n))))
(map (lambda (i) (map (lambda (j) (list i j)) (enumerate (- i 1)))) (enumerate 5))

