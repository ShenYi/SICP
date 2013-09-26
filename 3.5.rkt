#lang racket
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (area-rectangle x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (in-area)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
      (* (area-rectangle x1 x2 y1 y2)
     (monte-carlo trials in-area)))

(define (square x) (* x x))
(estimate-integral 
 (lambda (x y) (<= (+ (square (- x 5)) (square (- y 7))) 9))
 2
 8
 4
 10
 9999)