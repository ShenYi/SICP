#lang racket
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-down
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))
                              
(define (below painter1 painter2)
  (reverse-rotate-270 (reverse-rotate-180 
                       (beside
                        (reverse-rotate-270 painter1)
                        (reverse-rotate-270 painter2)))))