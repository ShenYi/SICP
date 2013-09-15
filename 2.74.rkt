#lang racket
(define (get-record name records)
  ((get 'get-record (type-tag records)) name records))

(define (get-salary name records)
  (let ((tag (type-tag records)))
    (let ((record ((get 'get-record tag) name records)))
      ((get 'get-salary tag) record))))

(define (find-employee-record name record-files)
  (if (and (pair? record-files) (not (null? record-files)))
      (let ((current-records (car record-files)))
        (let ((record ((get 'get-record (type-tag current-records)) name current-records)))
          (if (null? record) (find-employee-record name (cdr record-files))
              record)))))