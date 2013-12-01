(define (count-pairs x)
  (define counted '())
  (define (counted? pointer counted-list)
    (cond ((null? counted-list) #f)
	  ((eq? pointer (car counted-list)) #t)
	  (counted? pointer (cdr counted-list))))
  (cond ((not (pair? x)) 0)
	(else
	 (let ((xcar (car x))
	      (xcdr (cdr x)))
	  (let ((xcar-counted? (counted? xcar counted))
		(xcdr-counted? (counted? xcdr counted)))
	    (let ((xcar-count (count-pairs xcar))
		  (xcdr-count (count-pairs xcdr)))
	      (cond ((and xcar-counted? xcdr-counted?) 1)
		    ((and xcar-counted? (not xcdr-counted?))
		     (set! counted (cons xcdr counted))
		       (+ 1 xcdr-count))
		    ((and (not xcar-counted?) xcdr-counted?)
		     (set! counted (cons xcar counted))
		       (+ 1 xcar-count))
		    (else 
		     (set! counted (cons xcar counted))
		      (set! counted (cons xcdr counted))
		      (+ 1 xcar-count xcdr-count)))))))))
;; wrong program, second call to count-pairs will generate 
;; new environment, so counted will not be shared by 
;; each call to count-pairs