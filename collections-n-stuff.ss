#lang sicp

(define xs (list 4 2 391 394 29 19 31 1 384 ))

;; Right Associative Reduce 
(define (r-fold init op coll)
  (if (null? coll)
      init
      (op
       (car coll)
       (r-fold init op (cdr coll)))))

;; Simple Map
(define (map op coll)
  (if (null? coll)
      '()
      (cons
       (op (car coll))
       (map op (cdr coll)))))

;; Map Using Right Associative Reduce
(define (rf-map op coll)
  (r-fold
   '()
   (lambda (first rest) (cons (op first) rest))
   coll))

(define (add3 x) (+ 3 x))


