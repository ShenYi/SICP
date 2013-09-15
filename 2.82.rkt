#lang racket
(define (apply-generic op . args)
  (define (find-common-type types)
    (define (could? target-type other-types)
      (fold-left 
       (lambda (t) 
         (if (eq t target-type) #t
             (let ((cov-proc (get-coercion t target-type)))
               (if cov-proc #t #f))))
       #t
       other-types))
    (define (find-inner left-types current-type right-types)
      (if (could? current-type (append left-types right-types))
          current-type
          (if (null? right-types) null
              (find-inner (append left-types current-type) (car right-types) (cdr right-types)))))
    (find-inner '() (car types) (cdr types)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc args)
          (let ((common-type (find-common-type type-tags)))
            (if common-type
                (let ((new-args (map
                                 (lambda (arg)
                                   (if (eq? common-type (type-tag arg))
                                       arg
                                       (let ((convert-proc (get-coercion (type-tag arg) common-type)))
                                         (convert-proc arg))))
                                 args)))
                  (apply-generic op new-args))
                (error "No Method")))))))