(load "section3.3.3.scm")
(define (lookup key1 key2 table)
  (let ((sub-table (assoc key1 (cdr table))))
    (if sub-table
	(let ((record (assoc key2 (cdr subtable))))
	  (if record
	      (cdr record)
	      #f))
	#f)))

(define (insert! key1 key2 value table)
  (let ((sub-table (assoc key1 (cdr table))))
    (if sub-table
	(let ((record (assoc key2 (cdr sub-table))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! sub-table
			(cons (cons key2 value)
			      (cdr sub-table)))))
	(set-cdr! table
		  (cons (list key1
			      (cons key2 value))
			(cdr table)))))
  'ok)