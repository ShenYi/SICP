#lang racket
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence) (accumulate op initial (cdr sequence))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init 
                        (map car seqs))
            (accumulate-n op init 
                          (map cdr seqs)))))
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 1 0 0 1))
(define i (list (list 1 0 0) (list 0 1 0) (list 0 0 1) (list 0 0 0)))
(define (matrix-*-vector m v)
  (map
   (lambda (mi) (accumulate + 0 
                            (accumulate-n * 1 (list mi v))))
   m))
(define (transpose mat)
  (accumulate-n cons null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi)
           (map (lambda (ni)
                  (accumulate + 0 (accumulate-n * 1 (list mi ni)))) cols))
         m)))
  
(matrix-*-matrix m i)