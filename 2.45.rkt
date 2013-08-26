#lang racket
(define (split recursion-outer recursion-inner)
  (lambda (painter n)
    (if (= n 0) painter
        (let ((inner-element ((split recursion-outer recursion-inner) painter (- n 1))))
        (recursion-outer painter (recursion-inner inner-element inner-element))))))