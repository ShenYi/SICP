#lang racket
(define (compare key1 key2)
  (cond ((= key1 key2) 0)
        ((> key1 key2) 1)
        ((< key1 key2) -1)))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records? #f))
        (else
         (let ((r (compare given-key (key (car set-of-records)))))
           (cond ((= r 0) (car set-of-records))
                 ((< r 0) (lookup given-key (left-branch set-of-records)))
                 ((> r 0) (lookup given-key (right-branch set-of-records))))))))
