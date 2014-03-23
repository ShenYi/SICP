(load "section3.3.3-.scm")
(define (same-key? key-to-find key-in-table)
  (equal? key-to-find key-in-tabel))

(define (make-table same-key-func)
  (let ((table (list "*table*")))
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((same-key-func key (car (car records))) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
	(if record
	    (cdr record)
	    #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! table
		      (cons (cons key value) (cdr table)))))
      'ok)
    (define dispatch
      (lambda (m)
	(cond ((eq? m 'lookup) lookup)
	      ((eq? m 'insert!) insert!)
	      (else
	       (error "Unknown Operation")))))
    dispatch))
  