#lang racket
(define type-tower '(integer rational real complex))
(define (project x)
  (define (get-type-convert-proc type)
    (define (find-target-type left-types)
      (cond ((null? left-types) null)
            ((null? (cdr types)) null)
            (else
             (let ((first (car left-types))
                   (second (cadr left-types)))
               (if (eq? type first)
                   second
                   (find-target-type (cdr left-types)))))))
    (let ((target-type (find-target-type type-tower)))
      (let ((t-c-proc (get-coercion type target-type)))
        (if t-c-proc
            t-c-proc
            null))))
  (let ((type (type-tag x)))
    (let ((type-convert-proc (get-type-convert-proc type)))
      (if type-convert-proc
          (type-covert-proc x)
          x))))

(define (drop x)
  (let ((type (type-tag x)))
    (let ((lower (project x)))
      (if (eq? type (type-tag lower))
          lower
          (let ((higher (raise lower)))
            (let ((equal-proc (get 'equal (list (type-tag higher) (type-tag x)))))
              (if (equal-proc x higher)
                  (drop lower)
                  x)))))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let (result (apply op args))
            (drop result))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (car args))
                    (let ((type1-order (get-order type1))
                          (type2-order (get-order type2)))
                      (cond ((= type1-order type2-order) (error "No Method"))
                            ((> type1-order type2-order) 
                             (let ((type2->type1 (get-coercion type2 type1)))
                               (apply-generic op a1 (type2->type1 a2))))
                            (else
                             (let ((type1->type2 (get-coercion type1 type2)))
                               (apply-generic op (type1->type2 a1) a2)))))))
              (error "No Method")
              (error "No Method")
              )))))