#lang racket
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (and (pair? object) (eq? 'leaf (car object))))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit"))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))



(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message) 
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (in? symbol symbols)
  (cond ((null? symbols) #f)
        ((eq? symbol (car symbols)) #t)
        (else (in? symbol (cdr symbols)))))
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) 
         (cond ((eq? symbol (symbol-leaf tree)) '())
               (else (error "not in"))))
        (else
         (let ((lb (left-branch tree))
               (rb (right-branch tree)))
           (cond ((in? symbol (symbols lb)) (cons '0 (encode-symbol symbol lb)))
                 ((in? symbol (symbols rb)) (cons '1 (encode-symbol symbol rb)))
                 (else (error "not in")))))))
(encode '(A D A B B C A) sample-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge l)
  (cond ((null? l) '())
        ((equal? 1 (length l)) (car l))
        (else
         (let ((first (car l))
               (second (cadr l)))
           (let ((merged (make-code-tree first second)))
             (let ((newlist (adjoin-set merged (cdr (cdr l)))))
               (successive-merge newlist)))))))

(define music-tree
(generate-huffman-tree 
 '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (JOB 2) (YIP 9) (WAH 1))))
(define codes
(encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
        music-tree))
(length codes)
