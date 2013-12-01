(define (has-circle? sequence)
  (define travelled '())
  (define (travelled? node s)
    (cond ((or (null? s) (not (pair? s))) #f)
	  ((eq? node (car s)) #t)
	  (else (travelled? node (cdr s)))))
  (define (hc? node)
    (if (or (null? node) (not (pair? node)))
	#f
	(if (travelled? node travelled)
	    #t
	    (begin
	      (set! travelled (cons node travelled))
	      (hc? (cdr node))))))
  (hc? sequence)
)
 