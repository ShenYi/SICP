(define make-deque
  (lambda ()
    (cons '() '())))
(define front-deque
  (lambda (queue)
    (car queue)))
(define rear-deque
  (lambda (queue)
    (cdr queue)))
(define empty-deque?
  (lambda (queue)
    (null? (front-deque queue))))
(define set-front-deque!
  (lambda (queue item)
    (set-car! queue item)))
(define set-rear-deque!
  (lambda (queue item)
    (set-cdr! queue item)))
(define front-insert-deque!
  (lambda (queue item)
    (let ((new-pair (cons (cons item '()) '())))
      (cond ((empty-deque? queue)
	     (set-front-deque! queue new-pair)
	     (set-rear-deque! queue new-pair))
	    (else
	     (set-cdr! new-pair (front-deque queue))
	     (set-front-deque! queue new-pair))))))
(define front-delete-deque!
  (lambda (queue)
    (cond ((empty-deque? queue)
	   (error "Queue is Empty"))
	  (else
	   (set-front-deque! queue (cdr (front-deque queue)))
	   (cond ((not (empty-deque? queue))
		  (set-cdr! (car (front-deque queue)) '())))))))
(define rear-insert-deque!
  (lambda (queue item)
    (cond ((empty-deque? queue)
	   (let ((new-pair (cons (cons item '()) '())))
	     (set-front-deque! queue new-pair)
	     (set-rear-deque! queue new-pair)))
	  (else
	   (let ((last-item (rear-deque queue)))
	     (let ((new-pair (cons (cons item last-item) '())))
	       (set-cdr! last-item new-pair)
	       (set-rear-deque! queue new-pair)))))))
(define rear-delete-deque!
  (lambda (queue)
    (cond ((empty-deque? queue)
	   (error "Queue is Empty"))
	  (else
	   (let ((pre-second-item (cdr (car (rear-deque queue)))))
	     (cond ((null? pre-second-item)
		    (set-front-deque! queue '()))
		   (else
		    (set-cdr! pre-second-item '())
		    (set-rear-deque! queue pre-second-item))))))))
(define print-deque
  (lambda (queue)
    (define (print-item item)
      (cond ((null? item)
	     (display ""))
	    (else
	     (display (car (car item)))
	     (display " ")
	     (print-item (cdr item)))))
    (cond ((empty-deque? queue)
	   (display "Empty Deque"))
	  (else 
	   (print-item (front-deque queue))
	   (newline)))))

(define q (make-deque))
(front-insert-deque! q 'a)
(front-insert-deque! q 'b)
(rear-insert-deque! q 'c)
(rear-insert-deque! q 'd)
(print-deque q)
(front-delete-deque! q)
(print-deque q)
(rear-delete-deque! q)
(print-deque q)
(rear-delete-deque! q)
(print-deque q)