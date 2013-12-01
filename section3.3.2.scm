(define front-ptr (lambda (queue) (car queue)))
(define rear-ptr (lambda (queue) (cdr queue)))
(define set-front-ptr! (lambda (queue item) (set-car! queue item)))
(define set-rear-ptr! (lambda (queue item) (set-cdr! queue item)))
(define empty-queue? (lambda (queue) (null? (front-ptr queue))))
(define make-queue (lambda () (cons '() '())))
(define front-queue
  (lambda (queue)
    (if (empty-queue? queue)
	(error "Queue is empty!")
	(car (front-ptr queue)))))
(define insert-queue!
  (lambda (queue item)
    (let ((new-pair (cons item '())))
      (cond ((empty-queue? queue)
	     (set-front-ptr! queue new-pair)
	     (set-rear-ptr! queue new-pair)
	     queue)
	    (else
	     (set-cdr! (rear-ptr queue) new-pair)
	     (set-rear-ptr! queue new-pair)
	     queue)))))
(define delete-queue!
  (lambda (queue)
    (cond ((empty-queue? queue)
	   (error "Queue is empty"))
	  (else
	   (set-front-ptr! queue (cdr (front-queue queue)))
	   queue))))

  (define (print-queue-inner item)
    (if (null? item)
	(display "")
(begin
  (display " ")
	(display (car item))
	(print-queue-inner (cdr item)))))

(define print-queue  (lambda (queue)
    (if (empty-queue? queue)
	(display "Empty queue")
	(print-queue-inner (front-ptr queue)))))