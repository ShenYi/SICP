(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define (reverse x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cons (car x) y)))
	  (loop (cdr x) temp))))
  (loop x '()))