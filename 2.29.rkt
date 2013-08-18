#lang racket
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
    (cond ((null? branch) 0)
          ((not (pair? (branch-structure branch))) (branch-structure branch))
          (else (total-weight (branch-structure branch)))))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        (else
         (let ((lb (left-branch mobile)) (rb (right-branch mobile)))
           (+ (branch-weight lb) (branch-weight rb))))))


(define l (make-branch 10 1))
(define r (make-branch 10 1))
(define m (make-mobile l r))
(define ll (make-branch 1 m))
(define rr (make-branch 1 m))
(define n (make-mobile ll rr))
(total-weight n)

(define (balance? mobile)
  (cond ((null? mobile) #t)
        (else
         (let
             ((lb (left-branch mobile)) (rb (right-branch mobile)))
           (and
            (= (* (branch-length lb) (branch-weight lb)) (* (branch-length rb) (branch-weight rb)))
            (cond ((pair? (branch-structure lb)) (balance? (branch-structure lb)))
                  (else #t))
            (cond ((pair? (branch-structure rb)) (balance? (branch-structure rb)))
                  (else #t))
            )))))