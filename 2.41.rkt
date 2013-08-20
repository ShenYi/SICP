#lang racket
(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence) (accumulate op initial (cdr sequence))))))
(define (unique-pairs n)
  (accumulate append null
              (map (lambda (i) (map (lambda (j) (list i j)) (enumerate (- i 1))))
                   (enumerate n))))
(define (enumerate n)
  (define (iter current result)
    (cond ((< current 1) result)
          (else (iter (- current 1) (cons current result))))
    )
  (iter n null))
(unique-pairs 5)
(define (unique-tribles n)
  (accumulate append null
              (map (lambda (i) (map (lambda (pair) (cons i pair)) (unique-pairs (- i 1)))) (enumerate n))))
(unique-tribles 5)

(define (sum-equal? s)
  (lambda (trible) (= s (+ (car trible) (cadr trible) (caddr trible)))))

(define (filter predicate sequence)
  (define (iter result left)
    (cond ((null? left) result)
          ((predicate (car left)) (iter (cons (car left) result) (cdr left)))
          (else (iter result (cdr left)))))
  (iter null sequence))

(define (get n s)
  (filter (sum-equal? s) (unique-tribles n)))
(get 6 9)
