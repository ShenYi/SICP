(load "3.24.scm")
(define (make-table)
  (list '*table*))
(define (assoc key records)
  (cond ((null? records)
	 #f)
	((equal? key (car (car records)))
	 (car records))
	(else
	 (assoc key (cdr records)))))
  
(define (lookup keys table)
  (cond ((null? keys) table)
	((not (pair? (cdr table))) #f)
	(else
	 (let ((sub-table (assoc (car keys) (cdr table))))
	   (lookup (cdr keys) sub-table)))))

(define (insert! table keys value)
  (cond ((null? keys)
	 (error "Keys null"))
	(else
	 (let ((key (car keys))
	       (remain-keys (cdr keys)))
	   (let ((record (assoc key (cdr table))))
	     (if record
		 (if (null? remain-keys)
		     (set-cdr! record value)
		     (if (pair? (cdr record))
			 (insert! record remain-keys value)
			 (set-cdr! record (mt remain-keys value))))
		 (set-cdr! table (mt keys value))))))))
(define (mt keys value)
  (let ((current-key (car keys))
	(remain-keys (cdr keys)))
    (if (null? remain-keys)
	(cons current-key value)
	(cons current-key (mt remain-keys value)))))
	  
	   
	 
