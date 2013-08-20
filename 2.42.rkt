#lang racket
(define (enumerate n)
  (define (iter current result)
    (cond ((< current 1) result)
          (else (iter (- current 1) (cons current result))))
    )
  (iter n null))

(define (filter predicate sequence)
  (define (iter result left)
    (cond ((null? left) result)
          ((predicate (car left)) (iter (cons (car left) result) (cdr left)))
          (else (iter result (cdr left)))))
  (iter null sequence))
(define (my-map op sequence)
  (cond ((not (null? sequence)) (op (car sequence)) (my-map op (cdr sequence)))))

(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence) (accumulate op initial (cdr sequence))))))

(define (my-and p q)
  (and p q))

(define (queens board-size)
  (define (back-track current result result-length)
    (cond ((> current board-size) (newline) (display result))
          (else 
           (let ((pos (filter (valid? result result-length) (enumerate board-size))))
             (cond ((not (null? pos))
                 (my-map (lambda (x) (back-track (+ current 1) (append result (list x)) (+ result-length 1)))
                      pos
                     )))))))
  (back-track 1 '() 0))

(define (valid? result result-length)
  (lambda (x)
    (if (null? result) #t
        (accumulate my-and #t 
                    (map (lambda (i j)
                           (and (not (= x j))
                                (not (= x (- j (- (+ result-length 1) i))))
                                (not (= x (+ j (- (+ result-length 1) i))))))
                         (enumerate result-length) result)))))


(queens 10)