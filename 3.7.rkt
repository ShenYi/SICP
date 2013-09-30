#lang racket
(define (make-account balance password)
  (define left balance)
  (define (dispatch pw op)
    (if (eq? pw password)
        (cond ((eq? op 'withdraw) withdraw)
              ((eq? op 'deposit) deposit)
              (else (error "unrecognized operation")))
        (error "wrong password")))
  (define (withdraw amount)
    (if (> left amount) 
        (begin
          (set! left (- left amount))
          left)
        (error "not enough amount")))
  (define (deposit amount)
    (begin (set! left (+ left amount))
           left))
  dispatch)

(define (make-joint account old-password new-password)
  (let ((left ((account old-password 'withdraw) 0)))
    (lambda (pw op)
      (if (eq? pw new-password)
          (account old-password op)
          (error "password incorrect")))))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 20)

((paul-acc 'rosebud 'withdraw) 50)