#lang racket
(define (make-monitored f)
  (define count 0)
  (define (reset-count)
           (set! count 0))
  (define (how-many-calls?)
    count)
  (define (dispatch op)
    (cond ((eq? op 'how-many-calls?) (how-many-calls?))
          ((eq? op 'reset-count) (reset-count))
          (else 
           (begin
             (set! count (+ count 1))
             (f op)))))
  dispatch)

(define mf (make-monitored (lambda (x) (* x x))))
(mf 1)
(mf 10)
(mf 'how-many-calls?)
(mf 'reset-count)
(mf 'how-many-calls?)