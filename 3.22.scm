(define (make-queue)
  (let ((front-ptr (cons '() '()))
	(rear-ptr (cons '() '())))
    (define set-front-ptr!
      (lambda (item)
	(set-car! front-ptr item)))
    (define set-rear-ptr!
      (lambda (item)
	(set-car! rear-ptr item)))
    (define empty-queue?
      (lambda ()
	(null? (car front-ptr))))
    (define front-queue
      (lambda ()
	(if (empty-queue?)
	    (error "Queue is empty")
	    (car (car front-ptr)))))
    (define insert-queue!
      (lambda (item)
	(let ((new-pair (cons item '())))
	  (cond ((empty-queue?)
		 (set-front-ptr! new-pair)
		 (set-rear-ptr! new-pair))
		(else
		 (set-cdr! (car rear-ptr) new-pair)
		 (set-rear-ptr! new-pair))))))
    (define delete-queue!
      (lambda ()
	(cond ((empty-queue?)
	       (display "Queue is empty"))
	      (else
	       (set-front-ptr! (cdr (car front-ptr)))))))
    (define print-queue
      (lambda ()
	(define (print-inner item)
	  (if (null? item)
	      (display "")
	      (begin 
		(display " ")
		(display (car item))
		(print-inner (cdr item)))))
	(cond ((empty-queue?)
	       (display "Empty queue"))
	      (else
	       (print-inner (car front-ptr))))))
    (define dispatch
      (lambda (m)
	(cond ((eq? m 'empty-queue?) (empty-queue?))
	      ((eq? m 'front-queue) (front-queue))
	      ((eq? m 'insert-queue!) insert-queue!)
	      ((eq? m 'delete-queue!) delete-queue!)
	      ((eq? m 'print-queue) (print-queue))
	      (else (error "Wrong operation")))))
    dispatch))
(define q (make-queue))
;; 这并不是最好的解法
;; 实际上，直接使用front-ptr和rear-ptr代表首位两个节点，而不是使用它的car来表示指针。