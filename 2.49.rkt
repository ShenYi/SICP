#lang racket
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v) (* s (ycor-vect v)))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
(define (segments-painter segment-list)
  (lambda (frame) 
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
(define (for-each operation elements)
  (cond ((not (null? elements)) (operation (car elements)) (for-each operation (cdr elements)))))

(define p00 (make-vector 0 0))
(define p10 (make-vector 1 0))
(define p01 (make-vector 0 1))
(define p11 (make-vector 1 1))
(define painter-a
  (segments-painter (list 
                     (make-segment p00 p10)
                     (make-segment p00 p01)
                     (make-segment p10 p11)
                     (make-segment p01 p11))))
(define painter-b 
  (segments-painter (list
                     (make-segment p00 p11)
                     (make-segment p01 p10))))

(define p00-p10 (scale-vector (add-vector p00 p10) (/ 1.0 2)))
(define p00-p01 (scale-vector (add-vector p00 p01) (/ 1.0 2)))
(define p10-p11 (scale-vector (add-vector p10 p11) (/ 1.0 2)))
(define p01-p11 (scale-vector (add-vector p01 p11) (/ 1.0 2)))
(define painter-c
  (segments-painter (list
                     (make-segment p00-p10 p10-p11)
                     (make-segment p00-p10 p00-p01)
                     (make-segment p10-p11 p01-p11)
                     (make-segment p01-p11 p00-p01))))
